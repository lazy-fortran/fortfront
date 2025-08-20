module semantic_analyzer
    ! Hindley-Milner type inference (Algorithm W) - dialect-agnostic
    use type_system_hm, only: type_env_t, type_var_t, mono_type_t, poly_type_t, &
                              substitution_t, allocation_info_t, &
                              create_mono_type, create_type_var, &
                              create_poly_type, create_fun_type, free_type_vars, &
                              compose_substitutions, occurs_check, &
                              TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    use scope_manager
    use type_checker
    use ast_core  ! TODO: Migrate to explicit imports (complex due to extensive usage)
    use ast_nodes_data, only: intent_type_to_string, declaration_node, &
                              parameter_declaration_node, INTENT_NONE, &
                              INTENT_IN, INTENT_OUT, INTENT_INOUT
    use ast_nodes_bounds, only: array_spec_t, array_bounds_t, array_slice_node, &
                                range_expression_node, get_array_slice_node
    use ast_nodes_core, only: identifier_node, assignment_node, binary_op_node, &
                              call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_control, only: if_node, do_loop_node
    use parameter_tracker
    use expression_temporary_tracker_module
    use constant_folding, only: fold_constants_in_arena
    implicit none
    private

    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    public :: validate_array_bounds, check_shape_conformance
    public :: undeclared_variable_t, collect_undeclared_variables
    public :: type_constraint_t, infer_types_for_undeclared_variables
    public :: usage_pattern_t
    public :: declaration_t

    ! Constants for variable usage types
    integer, parameter, public :: USAGE_PARAMETER = 1
    integer, parameter, public :: USAGE_RESULT_VAR = 2
    integer, parameter, public :: USAGE_LOCAL_VAR = 3

    ! Constants for type constraint sources
    integer, parameter, public :: CONSTRAINT_LITERAL = 1      ! x = 5.0
    integer, parameter, public :: CONSTRAINT_BINARY_OP = 2    ! x + y
    integer, parameter, public :: CONSTRAINT_FUNCTION = 3     ! sin(x)
    integer, parameter, public :: CONSTRAINT_ASSIGNMENT = 4   ! x = expression
    integer, parameter, public :: CONSTRAINT_ARRAY_INDEX = 5  ! arr(i)
    integer, parameter, public :: CONSTRAINT_STRING_OP = 6    ! x // "text"

    ! Usage pattern data structure for intent inference
    type :: usage_pattern_t
        logical :: is_read = .false.
        logical :: is_written = .false.
        logical :: is_array_access = .false.
        logical :: is_passed_to_func = .false.
        logical :: is_modified_element = .false.
        integer :: first_read_location = 0
        integer :: first_write_location = 0
    contains
        procedure :: assign => usage_pattern_assign
        generic :: assignment(=) => assign
    end type usage_pattern_t

    ! Type constraint for variable type inference
    type :: type_constraint_t
        character(len=:), allocatable :: variable_name
        type(mono_type_t) :: required_type
        integer :: source_type  ! CONSTRAINT_*
        integer :: source_node  ! AST node that created this constraint
        real :: confidence      ! 0.0 to 1.0 confidence in this constraint
    contains
        procedure :: assign => type_constraint_assign
        generic :: assignment(=) => assign
    end type type_constraint_t

    ! Data structure for undeclared variable information
    type :: undeclared_variable_t
        character(len=:), allocatable :: name
        integer :: usage_type  ! USAGE_PARAMETER, USAGE_RESULT_VAR, USAGE_LOCAL_VAR
        integer :: first_usage_node  ! AST node index
        logical :: is_read
        logical :: is_written
        logical :: is_array_access
        type(mono_type_t) :: inferred_type  ! Result of type inference
        integer :: inferred_intent = INTENT_NONE  ! Inferred intent attribute
        type(usage_pattern_t) :: usage_pattern  ! Detailed usage pattern
    contains
        procedure :: assign => undeclared_variable_assign
        generic :: assignment(=) => assign
    end type undeclared_variable_t

    ! Data structure for variable declaration generation
    type :: declaration_t
        character(len=:), allocatable :: type_spec     ! "real(8)", "integer", "character(len=*)"
        character(len=:), allocatable :: intent_attr   ! "intent(in)", "intent(out)", etc.
        character(len=:), allocatable :: variable_name
        logical :: is_array
        character(len=:), allocatable :: array_spec    ! "(:)", "(:,:)", etc.
    contains
        procedure :: assign => declaration_assign
        generic :: assignment(=) => assign
    end type declaration_t

    ! Semantic analysis context
    type :: semantic_context_t
        type(type_env_t) :: env  ! Legacy flat environment (to be removed)
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(parameter_tracker_t) :: param_tracker  ! Track parameter attributes
        type(temp_tracker_t) :: temp_tracker  ! Track expression temporaries
    contains
        procedure :: infer => infer_type
        procedure :: infer_stmt => infer_statement_type
        procedure :: unify => unify_types
        procedure :: instantiate => instantiate_type_scheme
        procedure :: generalize => generalize_type
        procedure :: fresh_type_var => generate_fresh_type_var
        procedure :: apply_subst_to_type => apply_current_substitution
        procedure :: get_builtin_function_type
        procedure :: compose_with_subst
        procedure :: deep_copy => semantic_context_deep_copy
        procedure :: assign => semantic_context_assign
        procedure :: validate_bounds => validate_array_access_bounds
        procedure :: check_conformance => check_array_shape_conformance
        procedure :: collect_undeclared_variables
        procedure :: infer_types_for_undeclared_variables
        procedure :: analyze_parameter_intent
        procedure :: generate_variable_declarations
        generic :: assignment(=) => assign
    end type semantic_context_t

contains

    ! Create a new semantic context with builtin functions
    function create_semantic_context() result(ctx)
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type

        ! Initialize substitution
        ctx%subst%count = 0

        ! Initialize hierarchical scope stack
        ctx%scopes = create_scope_stack()

        ! Initialize expression temporary tracker
        ctx%temp_tracker = create_temp_tracker()

        ! Initialize legacy flat environment
        ctx%env%count = 0
        ctx%env%capacity = 10
        allocate (character(len=256) :: ctx%env%names(ctx%env%capacity))
        allocate (ctx%env%schemes(ctx%env%capacity))

        ! Initialize environment with builtin functions
        ctx%next_var_id = 1

        ! Create real -> real type for math functions
        real_type = create_mono_type(TREAL)
        real_to_real = create_fun_type(real_type, real_type)

        ! Create polymorphic type scheme (no type variables to generalize)
        builtin_scheme = create_poly_type(forall_vars=[type_var_t::], mono=real_to_real)

        ! Add common math functions to global scope
        call ctx%scopes%define("sin", builtin_scheme)
        call ctx%scopes%define("cos", builtin_scheme)
        call ctx%scopes%define("tan", builtin_scheme)
        call ctx%scopes%define("sqrt", builtin_scheme)
        call ctx%scopes%define("exp", builtin_scheme)
        call ctx%scopes%define("log", builtin_scheme)
        call ctx%scopes%define("abs", builtin_scheme)

        ! Also add to legacy flat environment for compatibility
        call ctx%env%extend("sin", builtin_scheme)
        call ctx%env%extend("cos", builtin_scheme)
        call ctx%env%extend("tan", builtin_scheme)
        call ctx%env%extend("sqrt", builtin_scheme)
        call ctx%env%extend("exp", builtin_scheme)
        call ctx%env%extend("log", builtin_scheme)
        call ctx%env%extend("abs", builtin_scheme)

    end function create_semantic_context

    ! Main entry point: analyze entire program
    subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (ast => arena%entries(root_index)%node)
        type is (program_node)
            call analyze_program_node_arena(ctx, arena, ast, root_index)
        type is (module_node)
            ! Skip module semantic analysis to avoid AST corruption
            ! Modules will be processed as-is during code generation
            return
        class default
            ! Single statement/expression
            call infer_and_store_type(ctx, arena, root_index)
        end select
        
        ! Perform constant folding after type inference
        call fold_constants_in_arena(arena)
    end subroutine analyze_program

    ! Analyze a program node with arena-based AST
    subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer :: i

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    call infer_and_store_type(ctx, arena, prog%body_indices(i))
                end if
            end do
        end if
    end subroutine analyze_program_node_arena

    ! Analyze a program node (legacy interface)
    subroutine analyze_program_node(ctx, prog)
        type(semantic_context_t), intent(inout) :: ctx
        type(program_node), intent(inout) :: prog
        integer :: i

        if (allocated(prog%body_indices)) then
            ! This is an arena-based program node but called without arena
            ! For now, skip analysis - this should be updated to use &
            ! analyze_program_arena
            return
        end if
    end subroutine analyze_program_node

    ! Infer type and store in AST node
    subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        ! DEBUG: print *, "infer_and_store_type for node", node_index, "type:", &
        !        trim(arena%entries(node_index)%node_type)
        inferred = ctx%infer_stmt(arena, node_index)

        ! Store the inferred type in the AST node (assignment now does deep &
        ! copy automatically)
        if (.not. allocated(arena%entries(node_index)%node%inferred_type)) then
            allocate (arena%entries(node_index)%node%inferred_type)
        end if
        arena%entries(node_index)%node%inferred_type = inferred
    end subroutine infer_and_store_type

    ! Infer type of a statement
    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        if (stmt_index <= 0 .or. stmt_index > arena%size) then
            typ = create_mono_type(TINT)  ! Default fallback
            return
        end if
        if (.not. allocated(arena%entries(stmt_index)%node)) then
            typ = create_mono_type(TINT)  ! Default fallback
            return
        end if

        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            typ = infer_assignment(this, arena, stmt, stmt_index)
            ! Store inference metadata in the assignment_node for error detection
            stmt%type_was_inferred = .true.
            stmt%inferred_type_name = typ%to_string()
        type is (print_statement_node)
            typ = create_mono_type(TINT)  ! print returns unit/void, use int
        type is (declaration_node)
            typ = analyze_declaration(this, arena, stmt, stmt_index)
        type is (module_node)
            typ = analyze_module(this, arena, stmt, stmt_index)
        type is (function_def_node)
            typ = analyze_function_def(this, arena, stmt, stmt_index)
        type is (subroutine_def_node)
            typ = analyze_subroutine_def(this, arena, stmt, stmt_index)
        type is (if_node)
            typ = analyze_if_node(this, arena, stmt, stmt_index)
        type is (do_loop_node)
            typ = analyze_do_loop(this, arena, stmt, stmt_index)
        type is (do_while_node)
            typ = analyze_do_while(this, arena, stmt, stmt_index)
        class default
            ! For expressions, use general inference
            typ = this%infer(arena, stmt_index)
        end select
    end function infer_statement_type

    ! Infer type of assignment and update environment
    function infer_assignment(ctx, arena, assign, assign_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(inout) :: assign
        integer, intent(in) :: assign_index
        type(mono_type_t) :: typ, target_type
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        character(len=:), allocatable :: var_name

        ! Pre-constrain target if RHS has character operations
        ! Temporarily disabled to isolate segfault source
        ! if (assign%value_index > 0 .and. assign%value_index <= arena%size) then
        !     if (node_has_character_op(arena, assign%value_index)) then
        !         call constrain_for_character_operation(ctx, arena, &
        !                                               assign%target_index)
        !     end if
        ! end if

        ! Infer type of RHS
        typ = ctx%infer(arena, assign%value_index)

        ! Get variable name from target
        if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
            select type (target => arena%entries(assign%target_index)%node)
            type is (identifier_node)
                var_name = target%name

                ! Check if variable already exists
                call ctx%scopes%lookup(var_name, existing_scheme)

                if (allocated(existing_scheme)) then
                    ! Variable exists - check assignment compatibility
                    target_type = ctx%instantiate(existing_scheme)

                    ! Check for string length changes requiring allocatable
                    if (typ%kind == TCHAR .and. target_type%kind == TCHAR) then
                        if (typ%size /= target_type%size) then
                            ! String length changed - mark as needing allocatable
                            typ%alloc_info%needs_allocatable_string = .true.
                            target_type%alloc_info%needs_allocatable_string = .true.
                        end if
                    end if

                    ! Issue 188: Array reassignment detection moved to standardizer
                    ! The standardizer handles multi-pass detection of &
                    ! reassignment patterns

                    if (.not. is_assignable(typ, target_type)) then
                        ! Type error - for now, just continue with inference
                        ! In a full implementation, we would report an error
                        ! error stop type_error(target_type, typ, &
                        !              "assignment to " // var_name)
                    end if

                    ! Use the existing type for consistency but preserve &
                    ! allocatable flag
                    if (typ%alloc_info%is_allocatable) then
                        target_type%alloc_info%is_allocatable = .true.
                    end if
                    if (typ%alloc_info%needs_allocatable_string) then
                        target_type%alloc_info%needs_allocatable_string = .true.
                    end if
                    typ = target_type
                end if

                ! Check for INTENT violations will be done by caller if needed
                ! This avoids circular dependency

                ! Store type in the identifier node
                if (.not. allocated(target%inferred_type)) then
                    allocate (target%inferred_type)
                end if
                target%inferred_type = typ

                ! If new variable, add to environment (TEMPORARY: should be error)
                ! TODO: Re-enable strict checking after fixing parser &
                ! multi-variable declaration bug
                if (.not. allocated(existing_scheme)) then
                    scheme = ctx%generalize(typ)
                    call ctx%scopes%define(var_name, scheme)
                else
                    ! Update existing variable with new type (including &
                    ! allocatable flag)
                    scheme = ctx%generalize(typ)
                    call ctx%scopes%define(var_name, scheme)
                end if
            class default
                error stop "Assignment target must be identifier"
            end select
        end if
    end function infer_assignment

    ! Main type inference function (Algorithm W)
    recursive function infer_type(this, arena, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ

        if (expr_index <= 0 .or. expr_index > arena%size) then
            typ = create_mono_type(TREAL)
            return
        end if
        if (.not. allocated(arena%entries(expr_index)%node)) then
            typ = create_mono_type(TREAL)
            return
        end if

        select type (expr => arena%entries(expr_index)%node)
        type is (literal_node)
            typ = infer_literal(this, expr)

        type is (identifier_node)
            typ = infer_identifier(this, expr)

        type is (binary_op_node)
            typ = infer_binary_op(this, arena, expr, expr_index)

        type is (call_or_subscript_node)
            ! Check if this is array subscripting or function call
            block
                use iso_fortran_env, only: error_unit
                integer :: i
                logical :: is_array_slice
                logical :: is_known_array
                type(poly_type_t), allocatable :: sym_scheme

                ! Simple heuristic: if any argument contains a colon operator,
                ! it's array slicing
                is_array_slice = .false.

                ! Check for array slicing pattern (contains : operator)
                if (allocated(expr%arg_indices)) then
                    do i = 1, size(expr%arg_indices)
                        if (expr%arg_indices(i) > 0 .and. &
                            expr%arg_indices(i) <= arena%size) then
                            if (allocated(arena%entries(expr%arg_indices(i))%node)) then
                                select type (arg_node => &
                                    arena%entries(expr%arg_indices(i))%node)
                                type is (range_expression_node)
                                    is_array_slice = .true.
                                    exit
                                end select
                            end if
                        end if
                    end do
                end if

                ! Check if the identifier is a known array in the symbol table
                is_known_array = .false.
                call this%scopes%lookup(expr%name, sym_scheme)
                if (allocated(sym_scheme)) then
                    ! Check if the type is an array type
                    ! Safety check: ensure mono type is properly initialized
                    if (sym_scheme%mono%kind > 0) then
                        select case (sym_scheme%mono%kind)
                        case (TARRAY)
                            is_known_array = .true.
                        end select
                    end if
                end if
                
                ! NEW LOGIC: Default to function call if not explicitly known as array
                ! This follows the principle that if it's not a declared array variable,
                ! it's likely a function call. Arrays must be explicitly declared.
                ! The previous heuristic about "all numeric arguments = array access" 
                ! was backwards because function calls like sin(1.0), factorial(5) 
                ! commonly have literal arguments.

                ! Set the disambiguation flag
                expr%is_array_access = is_array_slice .or. is_known_array
                
                ! Handle type inference based on what it is
                if (expr%is_array_access) then
                    ! For array access, return the element type
                    if (is_known_array .and. allocated(sym_scheme)) then
                        if (sym_scheme%mono%kind == TARRAY .and. &
                            allocated(sym_scheme%mono%args) .and. &
                            size(sym_scheme%mono%args) > 0) then
                            typ = sym_scheme%mono%args(1)  ! First arg is element type
                            ! (deep copy via assignment)
                        else
                            typ = create_mono_type(TVAR, var=this%fresh_type_var())
                        end if
                    else
                        ! Return a type variable for unknown arrays
                        typ = create_mono_type(TVAR, var=this%fresh_type_var())
                    end if
                else
                    ! It's a function call
                    typ = infer_function_call(this, arena, expr)
                end if
            end block

        type is (array_slice_node)
            ! Handle array slicing with bounds information
            typ = infer_array_slice(this, arena, expr)

        type is (subroutine_call_node)
            ! Subroutine calls don't return a value - shouldn't appear in expressions
            ! Return a type variable that will fail type checking
            typ = create_mono_type(TVAR, var=create_type_var(0, "error"))

        type is (assignment_node)
            typ = infer_assignment(this, arena, expr, expr_index)

        type is (array_literal_node)
            typ = infer_array_literal(this, arena, expr, expr_index)

        type is (do_loop_node)
            typ = infer_implied_do_loop(this, arena, expr, expr_index)

        class default
            ! Return real type as default for unsupported expressions
            typ = create_mono_type(TREAL)
        end select

        ! Apply current substitution
        typ = this%apply_subst_to_type(typ)

        ! Store the inferred type in the AST node
        if (.not. allocated(arena%entries(expr_index)%node%inferred_type)) then
            allocate (arena%entries(expr_index)%node%inferred_type)
        end if
        arena%entries(expr_index)%node%inferred_type = typ
    end function infer_type

    ! Infer type of literal
    function infer_literal(ctx, lit) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case (LITERAL_STRING)
            ! Calculate string length (subtract 2 for quotes)
            typ = create_mono_type(TCHAR, char_size=len_trim(lit%value) - 2)
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TLOGICAL)  ! Boolean as logical
        case default
            error stop "Unknown literal kind"
        end select
    end function infer_literal

    ! Infer type of identifier
    function infer_identifier(ctx, ident) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(identifier_node), intent(in) :: ident
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme

        ! Safety check: ensure identifier name is allocated and not empty
        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            return
        end if

        ! Look up identifier in hierarchical scopes
        call ctx%scopes%lookup(ident%name, scheme)

        if (allocated(scheme)) then
            ! Found in environment - instantiate the type scheme
            typ = ctx%instantiate(scheme)
        else
            ! Not found - create fresh type variable
            ! For undeclared variables, use a type variable that can be
            ! later constrained by usage context
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            
            ! Store this type variable in scope for consistency
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
            call ctx%scopes%define(ident%name, scheme)
        end if
    end function infer_identifier

    ! Infer type of binary operation
    function infer_binary_op(ctx, arena, binop, binop_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(inout) :: binop
        integer, intent(in) :: binop_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ, result_typ
        type(substitution_t) :: s1, s2, s3
        integer :: compat_level

        ! For character concatenation, pre-constrain operands
        ! Temporarily disabled to isolate segfault source
        ! if (trim(binop%operator) == "//") then
        !     ! Pre-constrain operands for character concatenation
        !     call constrain_for_character_operation(ctx, arena, binop%left_index)
        !     call constrain_for_character_operation(ctx, arena, binop%right_index)
        ! end if

        ! Infer left operand type
        left_typ = ctx%infer(arena, binop%left_index)

        ! Infer right operand type
        right_typ = ctx%infer(arena, binop%right_index)

        ! Determine result type based on operator
        select case (trim(binop%operator))
        case (":")
            ! Array range operator - for now, return integer type
            ! Note: Returns base element type - range/slice types are future enhancement
            typ = create_mono_type(TINT)
            return

        case ("+", "-", "*", "/", "**")
            ! Numeric operations: check compatibility
            if (is_compatible(left_typ, right_typ, compat_level)) then
                if (is_numeric_type(left_typ) .or. is_numeric_type(right_typ)) then
                    ! Get common type for numeric operations
                    result_typ = get_common_type(left_typ, right_typ)
                    
                    ! Propagate allocation attributes from operands
                    ! Result is allocatable if either operand is allocatable
                    result_typ%alloc_info%is_allocatable = &
                        left_typ%alloc_info%is_allocatable .or. &
                        right_typ%alloc_info%is_allocatable
                    ! Result involves pointers if either operand is a pointer
                    result_typ%alloc_info%is_pointer = &
                        left_typ%alloc_info%is_pointer .or. &
                        right_typ%alloc_info%is_pointer
                    ! Result needs allocation check if either operand does
                    result_typ%alloc_info%needs_allocation_check = &
                        left_typ%alloc_info%is_allocatable .or. &
                        right_typ%alloc_info%is_allocatable .or. &
                        left_typ%alloc_info%is_pointer .or. &
                        right_typ%alloc_info%is_pointer
                else
                    ! For type variables, unify as before
                    result_typ = create_mono_type(TVAR, var=ctx%fresh_type_var())

                    ! Unify left with result
                    s1 = ctx%unify(left_typ, result_typ)
                    call ctx%compose_with_subst(s1)

                    ! Apply s1 to right_typ before unifying
                    call s1%apply(right_typ, right_typ)
                    call s1%apply(result_typ, result_typ)

                    ! Unify right with result
                    s2 = ctx%unify(right_typ, result_typ)
                    call ctx%compose_with_subst(s2)

                    ! Final result type
                    call s2%apply(result_typ, result_typ)
                end if
            else
                ! Type error - for now, return real as default
                result_typ = create_mono_type(TREAL)
            end if

        case ("<", ">", "<=", ">=", "==", "/=")
            ! Comparison operations: operands must be compatible
            if (is_compatible(left_typ, right_typ, compat_level)) then
                result_typ = create_mono_type(TLOGICAL)  ! Boolean as logical
            else
                ! Type error - still return boolean
                result_typ = create_mono_type(TLOGICAL)
            end if

        case (".and.", ".or.")
            ! Logical operations: all logical
            s1 = ctx%unify(left_typ, create_mono_type(TLOGICAL))
            call ctx%compose_with_subst(s1)
            s2 = ctx%unify(right_typ, create_mono_type(TLOGICAL))
            call ctx%compose_with_subst(s2)
            result_typ = create_mono_type(TLOGICAL)

        case ("//")
            ! String concatenation
            ! Both operands must be character types
            ! The result will have combined length
            if (left_typ%kind == TCHAR) then
                ! Left is already character type
            else
                ! Unify with character type (including TVAR)
                s1 = ctx%unify(left_typ, create_mono_type(TCHAR))
                call ctx%compose_with_subst(s1)
            end if

            if (right_typ%kind == TCHAR) then
                ! Right is already character type
            else
                ! Unify with character type (including TVAR)
                s2 = ctx%unify(right_typ, create_mono_type(TCHAR))
                call ctx%compose_with_subst(s2)
            end if

            ! Result is a character type with combined length
            ! Calculate combined string length for concatenation
            block
                integer :: left_size, right_size, total_size
                
                left_size = 1  ! Default for unknown size
                right_size = 1  ! Default for unknown size

                ! Get left operand size if it's a character type
                if (left_typ%kind == TCHAR) then
                    left_size = left_typ%size
                end if

                ! Get right operand size if it's a character type  
                if (right_typ%kind == TCHAR) then
                    right_size = right_typ%size
                end if

                ! Combined length for concatenation
                total_size = left_size + right_size
                result_typ = create_mono_type(TCHAR, char_size=total_size)
            end block

        case default
            error stop "Unknown binary operator: "//trim(binop%operator)
        end select

        typ = ctx%apply_subst_to_type(result_typ)
        
        ! Track temporary for this binary operation if needed
        ! Complex expressions like (a + b) * (c + d) need temporaries
        if (needs_temporary(binop%operator)) then
            block
                integer :: temp_id
                character(len=32) :: type_str
                integer :: size_bytes
                
                ! Determine type string and size
                select case (result_typ%kind)
                case (TINT)
                    type_str = "integer"
                    size_bytes = 4
                case (TREAL)
                    type_str = "real"
                    size_bytes = 4
                case (TLOGICAL)
                    type_str = "logical"
                    size_bytes = 4
                case (TCHAR)
                    type_str = "character"
                    size_bytes = result_typ%size
                case default
                    type_str = "unknown"
                    size_bytes = 4
                end select
                
                ! Allocate temporary
                temp_id = ctx%temp_tracker%allocate_temp(type_str, size_bytes, &
                                                        binop_index)
                
                ! Mark this expression as using this temporary
                call ctx%temp_tracker%mark_expr_temps(binop_index, [temp_id])
            end block
        end if
    end function infer_binary_op
    
    ! Check if an operator needs a temporary variable
    function needs_temporary(operator) result(needs_temp)
        character(len=*), intent(in) :: operator
        logical :: needs_temp
        
        ! Most arithmetic operations need temporaries for intermediate results
        ! Note: This is intentionally conservative - we track all potential
        ! temporaries to enable optimization analysis. Later optimization
        ! passes can eliminate unnecessary temporaries based on context.
        select case (trim(operator))
        case ("+", "-", "*", "/", "**", "//")
            needs_temp = .true.
        case default
            needs_temp = .false.
        end select
    end function needs_temporary

    ! Constrain identifiers for character operations with depth limiting
    subroutine constrain_for_character_operation(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        integer, parameter :: MAX_DEPTH = 50
        
        call constrain_char_op_limited(ctx, arena, node_index, 0, MAX_DEPTH)
    end subroutine constrain_for_character_operation

    ! Implementation with depth limiting to prevent infinite recursion
    recursive subroutine constrain_char_op_limited(ctx, arena, node_index, &
                                                  current_depth, max_depth)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index, current_depth, max_depth
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: char_type
        
        ! Depth limit to prevent stack overflow
        if (current_depth >= max_depth) return
        
        ! Enhanced bounds checking
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Additional safety check for context
        if (ctx%scopes%depth <= 0 .or. .not. allocated(ctx%scopes%scopes)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            ! Enhanced identifier validation and constraint
            if (allocated(node%name) .and. len_trim(node%name) > 0) then
                call ctx%scopes%lookup(node%name, scheme)
                
                if (.not. allocated(scheme)) then
                    ! Undeclared identifier - pre-define as character
                    char_type = create_mono_type(TCHAR, char_size=1)
                    scheme = create_poly_type(forall_vars=[type_var_t::], &
                                            mono=char_type)
                    call ctx%scopes%define(node%name, scheme)
                end if
            end if
        type is (binary_op_node)
            ! Safe recursive constraining with depth tracking
            if (allocated(node%operator) .and. trim(node%operator) == "//") then
                ! Validate child indices before recursion
                if (node%left_index > 0 .and. node%left_index <= arena%size) then
                    call constrain_char_op_limited(ctx, arena, node%left_index, &
                                                  current_depth + 1, max_depth)
                end if
                if (node%right_index > 0 .and. node%right_index <= arena%size) then
                    call constrain_char_op_limited(ctx, arena, node%right_index, &
                                                  current_depth + 1, max_depth)
                end if
            end if
        end select
    end subroutine constrain_char_op_limited

    ! Infer type for polymorphic min/max calls with proper type promotion
    function infer_min_max_call(ctx, arena, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(mono_type_t) :: typ
        type(mono_type_t), allocatable :: arg_types(:)
        logical :: has_real, has_integer
        integer :: i

        ! Must have at least 2 arguments
        if (.not. allocated(call_node%arg_indices) .or. size(call_node%arg_indices) < 2) then
            ! Default to real type if insufficient arguments
            typ = create_mono_type(TREAL)
            return
        end if

        ! Infer all argument types
        allocate(arg_types(size(call_node%arg_indices)))
        has_real = .false.
        has_integer = .false.

        do i = 1, size(call_node%arg_indices)
            if (call_node%arg_indices(i) > 0 .and. call_node%arg_indices(i) <= arena%size) then
                arg_types(i) = infer_type(ctx, arena, call_node%arg_indices(i))
                
                ! Track types for promotion rule
                if (arg_types(i)%kind == TREAL) then
                    has_real = .true.
                else if (arg_types(i)%kind == TINT) then
                    has_integer = .true.
                end if
            else
                ! Invalid argument - default to integer
                arg_types(i) = create_mono_type(TINT)
                has_integer = .true.
            end if
        end do

        ! Apply Fortran type promotion rules:
        ! - All integers → integer result  
        ! - Any real → real result (promotes integers to real)
        if (has_real) then
            typ = create_mono_type(TREAL)
            
            ! NOTE: Type promotion logic handled in codegen_core.f90
            ! based on inferred types, not requiring intrinsic_signature modification
        else if (has_integer) then
            typ = create_mono_type(TINT)
        else
            ! Fallback to real if no clear types
            typ = create_mono_type(TREAL)
        end if
    end function infer_min_max_call

    ! Infer type of function call
    function infer_function_call(ctx, arena, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(mono_type_t) :: typ
        type(mono_type_t) :: fun_typ, arg_typ, result_typ
        type(mono_type_t), allocatable :: arg_types(:)
        type(substitution_t) :: s
        integer :: i

        ! Handle polymorphic min/max functions specially
        if (trim(call_node%name) == "min" .or. trim(call_node%name) == "max") then
            typ = infer_min_max_call(ctx, arena, call_node)
            return
        end if

        ! Get function type
        fun_typ = ctx%get_builtin_function_type(call_node%name)

        if (fun_typ%kind == 0) then
            ! Unknown function - look up in environment
            block
                type(identifier_node) :: fun_ident
                fun_ident = create_identifier(call_node%name, &
                                              call_node%line, call_node%column)
                fun_typ = infer_identifier(ctx, fun_ident)

                ! If this is an array type, we're doing array subscripting
                if (fun_typ%kind == TARRAY) then
                    ! Array subscripting - return element type
                    ! Note: Basic array access - multi-dimensional slicing is future enhancement
                    if (allocated(fun_typ%args)) then
                        ! For array types, args(1) is the element type
                        if (size(fun_typ%args) > 0) then
                            typ = fun_typ%args(1)
                        else
                            typ = create_mono_type(TINT)
                        end if
                    else
                        ! Default to integer for now
                        typ = create_mono_type(TINT)
                    end if
                    return
                end if
            end block
        end if

        ! Process arguments
        if (allocated(call_node%arg_indices)) then
            allocate (arg_types(size(call_node%arg_indices)))

            ! Infer all argument types
            do i = 1, size(call_node%arg_indices)
     if (call_node%arg_indices(i) > 0 .and. call_node%arg_indices(i) <= arena%size) then
                    arg_types(i) = infer_type(ctx, arena, call_node%arg_indices(i))

                    ! Check if we got a valid type
                    if (arg_types(i)%kind < TVAR .or. arg_types(i)%kind > TARRAY) then
                        print *, "WARNING: Invalid type inferred for argument ", i
                        print *, "  Type kind: ", arg_types(i)%kind
                        ! Create a type variable as fallback
                        arg_types(i) = create_mono_type(TVAR, var=ctx%fresh_type_var())
                    end if
                else
                  print *, "WARNING: Invalid argument index: ", call_node%arg_indices(i)
                    arg_types(i) = create_mono_type(TVAR, var=ctx%fresh_type_var())
                end if
            end do

            ! Check if we're dealing with an array (already handled above)
            if (fun_typ%kind == TARRAY) then
                ! This case should have been handled above
                print *, "WARNING: Array type in function call unification"
                typ = create_mono_type(TINT)
                return
            end if

            ! Unify with function type
            result_typ = fun_typ
            do i = 1, size(arg_types)
                ! Create expected function type: arg -> result
                block
                    type(type_var_t) :: tv
                    type(mono_type_t) :: expected_fun_type, new_result_typ

                    tv = ctx%fresh_type_var()
                    new_result_typ = create_mono_type(TVAR, var=tv)
                    expected_fun_type = create_fun_type(arg_types(i), new_result_typ)

                    ! Unify current function type with expected
                    ! Check if we're trying to unify incompatible types
                  if (result_typ%kind /= TFUN .and. expected_fun_type%kind == TFUN) then
                        ! Create a type variable as result
                        tv = ctx%fresh_type_var()
                        result_typ = create_mono_type(TVAR, var=tv)
                        cycle
                    end if

                    ! Try to unify, but handle type mismatch gracefully
                    block
                        type(substitution_t) :: temp_s
                        temp_s = ctx%unify(result_typ, expected_fun_type)
                        call ctx%compose_with_subst(temp_s)
                        s = temp_s
                    end block

                    ! Update result type
                    call s%apply(new_result_typ, result_typ)
                end block
            end do

            typ = result_typ
        else
            ! No arguments - function type is the result
            if (fun_typ%kind == TFUN .and. allocated(fun_typ%args)) then
                typ = fun_typ%args(size(fun_typ%args))  ! Last element is return type
            else
                typ = fun_typ
            end if
        end if

        typ = ctx%apply_subst_to_type(typ)
    end function infer_function_call

    ! Type unification
    recursive function unify_types(this, t1, t2) result(subst)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2
        type(substitution_t) :: subst
        type(mono_type_t) :: t1_subst, t2_subst

        ! Apply current substitution first
        t1_subst = this%apply_subst_to_type(t1)
        t2_subst = this%apply_subst_to_type(t2)

        ! Initialize empty substitution
        subst%count = 0

        ! Handle type variables
        if (t1_subst%kind == TVAR) then
            if (t2_subst%kind == TVAR .and. t1_subst%var%id == t2_subst%var%id) then
                ! Same variable - empty substitution
                return
            else if (occurs_check(t1_subst%var, t2_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t1_subst%var, t2_subst)
            end if
            return
        else if (t2_subst%kind == TVAR) then
            if (occurs_check(t2_subst%var, t1_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t2_subst%var, t1_subst)
            end if
            return
        end if

        ! Both are concrete types
        if (t1_subst%kind /= t2_subst%kind) then

            ! Special case: trying to unify integer with function type
            ! likely means array subscripting
            if ((t1_subst%kind == TINT .and. t2_subst%kind == TFUN) .or. &
                (t1_subst%kind == TFUN .and. t2_subst%kind == TINT)) then
                ! Return empty substitution to continue
                subst%count = 0
                allocate (subst%vars(0))
                allocate (subst%types(0))
                return
            end if
            
            ! Special case: trying to unify real with character type
            ! This can happen when literals are incorrectly typed or in &
            ! complex expressions
            if ((t1_subst%kind == TREAL .and. t2_subst%kind == TCHAR) .or. &
                (t1_subst%kind == TCHAR .and. t2_subst%kind == TREAL)) then
                ! For mathematical contexts, try to coerce character to real &
                ! if it looks numeric
                subst%count = 0
                allocate (subst%vars(0))
                allocate (subst%types(0))
                return  ! Allow the unification by returning empty substitution
            end if
            
            ! Special case: Allow integer to real coercion in mathematical contexts
            if ((t1_subst%kind == TINT .and. t2_subst%kind == TREAL) .or. &
                (t1_subst%kind == TREAL .and. t2_subst%kind == TINT)) then
                ! In Fortran, integer values can be promoted to real in &
                ! mixed expressions
                subst%count = 0
                allocate (subst%vars(0))
                allocate (subst%types(0))
                return  ! Allow the unification
            end if
            
            ! Special case: Allow base type to unify with array of that type
            ! This handles cases like "integer :: arr = [1,2,3]"
            if ((t1_subst%kind == TINT .and. t2_subst%kind == TARRAY) .or. &
                (t1_subst%kind == TARRAY .and. t2_subst%kind == TINT)) then
                ! Check that array element type matches base type
                if (t1_subst%kind == TARRAY) then
                    if (allocated(t1_subst%args) .and. size(t1_subst%args) >= 1 .and. &
                        t1_subst%args(1)%kind == TINT) then
                        subst%count = 0
                        allocate (subst%vars(0))
                        allocate (subst%types(0))
                        return  ! Allow the unification
                    end if
                else  ! t2_subst%kind == TARRAY
                    if (allocated(t2_subst%args) .and. size(t2_subst%args) >= 1 .and. &
                        t2_subst%args(1)%kind == TINT) then
                        subst%count = 0
                        allocate (subst%vars(0))
                        allocate (subst%types(0))
                        return  ! Allow the unification
                    end if
                end if
            end if
            
            ! Special case: Allow real base type to unify with real array type
            if ((t1_subst%kind == TREAL .and. t2_subst%kind == TARRAY) .or. &
                (t1_subst%kind == TARRAY .and. t2_subst%kind == TREAL)) then
                ! Check that array element type matches base type
                if (t1_subst%kind == TARRAY) then
                    if (allocated(t1_subst%args) .and. size(t1_subst%args) >= 1 .and. &
                        t1_subst%args(1)%kind == TREAL) then
                        subst%count = 0
                        allocate (subst%vars(0))
                        allocate (subst%types(0))
                        return  ! Allow the unification
                    end if
                else  ! t2_subst%kind == TARRAY
                    if (allocated(t2_subst%args) .and. size(t2_subst%args) >= 1 .and. &
                        t2_subst%args(1)%kind == TREAL) then
                        subst%count = 0
                        allocate (subst%vars(0))
                        allocate (subst%types(0))
                        return  ! Allow the unification
                    end if
                end if
            end if

            ! Check if we have valid types before calling to_string
            if (t1_subst%kind >= TVAR .and. t1_subst%kind <= TARRAY .and. &
                t2_subst%kind >= TVAR .and. t2_subst%kind <= TARRAY) then
                error stop "Type mismatch: cannot unify "// &
                    t1_subst%to_string()//" with "//t2_subst%to_string()
            else
                print *, "ERROR: Invalid type kinds in unify_types: ", &
                    t1_subst%kind, " and ", t2_subst%kind
                error stop "Type mismatch: invalid type kinds"
            end if
        end if

        select case (t1_subst%kind)
        case (TINT, TREAL, TLOGICAL)
            ! Base types unify if equal (already checked kind)

        case (TCHAR)
            ! Unify character types
            ! Different character lengths can be unified but may require allocatable
            if (t1_subst%size /= t2_subst%size) then
                ! Character types with different lengths
                ! Mark both as needing allocatable for dynamic length handling
                t1_subst%alloc_info%needs_allocatable_string = .true.
                t2_subst%alloc_info%needs_allocatable_string = .true.
            end if
            ! Character types with same or adjustable lengths can be unified
            subst%count = 0
            allocate(subst%vars(0))
            allocate(subst%types(0))

        case (TFUN)
            ! Handle function types defensively due to incomplete inferred_type copying
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                ! This occurs because AST node copying doesn't preserve &
                ! full type information
                ! TODO: Remove this workaround when proper inferred_type &
                ! copying is implemented
                subst%count = 0
                allocate(subst%vars(0))
                allocate(subst%types(0))
                return
            end if
            if (size(t1_subst%args) /= size(t2_subst%args)) then
                error stop "Function arity mismatch"
            end if

            ! Unify arguments pairwise
            block
                integer :: i
                type(substitution_t) :: s

                do i = 1, size(t1_subst%args)
                    s = this%unify(this%apply_subst_to_type(t1_subst%args(i)), &
                                   this%apply_subst_to_type(t2_subst%args(i)))
                    subst = compose_substitutions(s, subst)
                    call this%compose_with_subst(s)
                end do
            end block

        case (TARRAY)
            ! Handle case where array types don't have properly allocated args
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                ! If one array has no args, try to create minimal args structure
                if (.not. allocated(t1_subst%args)) then
                    allocate(t1_subst%args(1))
                    t1_subst%args(1) = create_mono_type(TVAR, var=this%fresh_type_var())
                end if
                if (.not. allocated(t2_subst%args)) then
                    allocate(t2_subst%args(1))
                    t2_subst%args(1) = create_mono_type(TVAR, var=this%fresh_type_var())
                end if
            end if

            ! Unify element types
            subst = this%unify(t1_subst%args(1), t2_subst%args(1))

            ! Check sizes if known
            if (t1_subst%size > 0 .and. t2_subst%size > 0) then
                if (t1_subst%size /= t2_subst%size) then
                    error stop "Cannot unify arrays of different sizes"
                end if
            end if

        case default
            if (t1_subst%kind == 0 .or. t2_subst%kind == 0) then
                ! Return empty substitution for uninitialized types
                ! This can happen with undefined functions
                return
            else
                ! For unknown type kinds, return empty substitution
                return
            end if
        end select
    end function unify_types

    ! Instantiate a type scheme
    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ
        type(substitution_t) :: subst
        integer :: i

        ! Create fresh type variables for all quantified variables
        subst%count = 0
        if (allocated(scheme%forall)) then
            do i = 1, size(scheme%forall)
                call subst%add(scheme%forall(i), &
                               create_mono_type(TVAR, var=this%fresh_type_var()))
            end do
        end if

        ! Apply substitution to get instance
        call subst%apply(scheme%mono, typ)
    end function instantiate_type_scheme

    ! Generalize a type to a type scheme
    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        type(type_var_t), allocatable :: free_vars(:), env_vars(:), gen_vars(:)
        integer :: i, j, count
        logical :: in_env

        ! Get free variables in type
        call free_type_vars(typ, free_vars)

        if (size(free_vars) == 0) then
            ! No free variables - monomorphic type
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
            return
        end if

        ! Get free variables in environment
        call get_env_free_vars(this%env, env_vars)

        ! Find variables to generalize (in type but not in env)
        allocate (gen_vars(size(free_vars)))
        count = 0

        do i = 1, size(free_vars)
            in_env = .false.
            do j = 1, size(env_vars)
                if (free_vars(i)%id == env_vars(j)%id) then
                    in_env = .true.
                    exit
                end if
            end do

            if (.not. in_env) then
                count = count + 1
                gen_vars(count) = free_vars(i)
            end if
        end do

        ! Create type scheme
        if (count > 0) then
            scheme = create_poly_type(forall_vars=gen_vars(1:count), mono=typ)
        else
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
        end if
    end function generalize_type

    ! Generate fresh type variable
    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv

        this%next_var_id = this%next_var_id + 1
        tv = create_type_var(this%next_var_id)
    end function generate_fresh_type_var

    ! Apply current substitution to a type
    function apply_current_substitution(this, typ) result(result_typ)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_typ

        call this%subst%apply(typ, result_typ)
    end function apply_current_substitution

    ! Compose a substitution with the current one
    subroutine compose_with_subst(this, s)
        class(semantic_context_t), intent(inout) :: this
        type(substitution_t), intent(in) :: s

        this%subst = compose_substitutions(s, this%subst)
    end subroutine compose_with_subst

    ! Get builtin function type
    function get_builtin_function_type(this, name) result(typ)
        class(semantic_context_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(mono_type_t) :: typ
        type(mono_type_t) :: int_type, real_type

        ! Safety check: ensure name is not empty
        if (len_trim(name) == 0) then
            typ%kind = 0
            return
        end if

        ! Create basic types
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)

        ! Return appropriate function types for intrinsics
        select case (trim(name))
            ! Real -> Real functions
        case ("sqrt", "sin", "cos", "tan", "exp", "log", "asin", "acos", "atan", &
              "sinh", "cosh", "tanh", "asinh", "acosh", "atanh")
            typ = create_fun_type(real_type, real_type)

            ! Abs can take integer or real (polymorphic - for now just real)
        case ("abs")
            typ = create_fun_type(real_type, real_type)

            ! Integer functions
        case ("int", "floor", "ceiling", "nint")
            typ = create_fun_type(real_type, int_type)

            ! Real conversion
        case ("real", "float")
            typ = create_fun_type(int_type, real_type)

            ! Min/max - polymorphic functions with type promotion
        case ("min", "max")
            ! Create a polymorphic type that promotes to the appropriate result type
            ! This will be resolved during function call inference based on arguments
            typ = create_mono_type(TVAR, var=this%fresh_type_var())

            ! Mod function
        case ("mod", "modulo")
            ! Two arguments - for now simplified as real -> real
            typ = create_fun_type(real_type, real_type)

            ! Precision inquiry function (real -> integer)
        case ("precision")
            typ = create_fun_type(real_type, int_type)

            ! Array intrinsic functions
        case ("size")
            ! size(array) -> integer
            ! For now, create a polymorphic array type
            block
                type(mono_type_t) :: array_type, elem_var
                type(mono_type_t), allocatable :: array_args(:)

                elem_var = create_mono_type(TVAR, var=this%fresh_type_var())
                allocate (array_args(1))
                array_args(1) = elem_var
                array_type = create_mono_type(TARRAY, args=array_args)
                typ = create_fun_type(array_type, int_type)
            end block

        case ("sum")
            ! sum(array) -> element_type (numeric)
            ! For now, handle integer and real arrays
            block
                type(mono_type_t) :: array_type
                type(mono_type_t), allocatable :: array_args(:)

                allocate (array_args(1))
                array_args(1) = int_type
                array_type = create_mono_type(TARRAY, args=array_args)
                typ = create_fun_type(array_type, int_type)
            end block

        case ("shape")
            ! shape(array) -> integer array
            block
                type(mono_type_t) :: array_type, result_type, elem_var
                type(mono_type_t), allocatable :: array_args(:), result_args(:)

                ! Input: array of any type
                elem_var = create_mono_type(TVAR, var=this%fresh_type_var())
                allocate (array_args(1))
                array_args(1) = elem_var
                array_type = create_mono_type(TARRAY, args=array_args)

                ! Output: integer array
                allocate (result_args(1))
                result_args(1) = int_type
                result_type = create_mono_type(TARRAY, args=result_args)

                typ = create_fun_type(array_type, result_type)
            end block

            ! String intrinsic functions
        case ("len")
            ! len(string) -> integer
            block
                type(mono_type_t) :: char_type
                char_type = create_mono_type(TCHAR)
                typ = create_fun_type(char_type, int_type)
            end block

        case ("len_trim")
            ! len_trim(string) -> integer
            block
                type(mono_type_t) :: char_type
                char_type = create_mono_type(TCHAR)
                typ = create_fun_type(char_type, int_type)
            end block

        case ("trim")
            ! trim(string) -> string
            block
                type(mono_type_t) :: char_type
                char_type = create_mono_type(TCHAR)
                typ = create_fun_type(char_type, char_type)
            end block

        case ("adjustl", "adjustr")
            ! adjustl/adjustr(string) -> string
            block
                type(mono_type_t) :: char_type
                char_type = create_mono_type(TCHAR)
                typ = create_fun_type(char_type, char_type)
            end block

        case ("index")
            ! index(string, substring) -> integer
            ! For now, simplified as string -> integer
            block
                type(mono_type_t) :: char_type
                char_type = create_mono_type(TCHAR)
                typ = create_fun_type(char_type, int_type)
            end block

        case ("present")
            ! present(optional_param) -> logical
            ! Takes any type and returns logical
            block
                type(mono_type_t) :: param_type, logical_type
                param_type = create_mono_type(TVAR, var=this%fresh_type_var())
                logical_type = create_mono_type(TLOGICAL)
                typ = create_fun_type(param_type, logical_type)
            end block

        case default
            ! Return empty type to indicate not found
            typ%kind = 0
        end select
    end function get_builtin_function_type

    ! Get free type variables in environment
    subroutine get_env_free_vars(env, vars)
        type(type_env_t), intent(in) :: env
        type(type_var_t), allocatable, intent(out) :: vars(:)
        type(type_var_t), allocatable :: temp_vars(:), scheme_vars(:)
        integer :: i, j, k, count
        logical :: found

        allocate (temp_vars(1000))  ! Temporary storage
        count = 0

        ! Collect all free variables from all schemes
        do i = 1, env%count
            call get_scheme_free_vars(env%schemes(i), scheme_vars)

            do j = 1, size(scheme_vars)
                ! Check if already collected
                found = .false.
                do k = 1, count
                    if (temp_vars(k)%id == scheme_vars(j)%id) then
                        found = .true.
                        exit
                    end if
                end do

                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = scheme_vars(j)
                end if
            end do
        end do

        ! Return exact size array
        if (count > 0) then
            allocate (vars(count))
            vars = temp_vars(1:count)
        else
            allocate (vars(0))
        end if
    end subroutine get_env_free_vars

    ! Get free variables in a type scheme
    subroutine get_scheme_free_vars(scheme, vars)
        type(poly_type_t), intent(in) :: scheme
        type(type_var_t), allocatable, intent(out) :: vars(:)
        type(type_var_t), allocatable :: mono_vars(:)
        integer :: i, j, count
        logical :: quantified

        ! Get free variables in monotype
        call free_type_vars(scheme%mono, mono_vars)

        if (.not. allocated(scheme%forall) .or. size(scheme%forall) == 0) then
            vars = mono_vars
            return
        end if

        ! Filter out quantified variables (safe allocation)
        allocate (vars(size(mono_vars)))
        count = 0

        do i = 1, size(mono_vars)
            quantified = .false.
            do j = 1, size(scheme%forall)
                if (mono_vars(i)%id == scheme%forall(j)%id) then
                    quantified = .true.
                    exit
                end if
            end do

            if (.not. quantified) then
                count = count + 1
                vars(count) = mono_vars(i)
            end if
        end do

        ! Return exact size (safe array resizing)
        if (count > 0) then
            block
                type(type_var_t), allocatable :: temp(:)
                allocate (temp(count))
                temp = vars(1:count)
                deallocate (vars)
                allocate (vars(count))
                vars = temp
            end block
        else
            ! Deallocate and allocate empty array
            deallocate (vars)
            allocate (vars(0))
        end if
    end subroutine get_scheme_free_vars

    ! Analyze module node
    function analyze_module(ctx, arena, mod_node, mod_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(module_node), intent(inout) :: mod_node
        integer, intent(in) :: mod_index
        type(mono_type_t) :: typ
        integer :: i

        ! Enter module scope
        call ctx%scopes%enter_module(mod_node%name)

        ! Analyze module declarations
        if (allocated(mod_node%declaration_indices)) then
            do i = 1, size(mod_node%declaration_indices)
                if (mod_node%declaration_indices(i) > 0) then
                    ! Use infer_and_store_type instead of creating temporary variables
                    call infer_and_store_type(ctx, arena, mod_node%declaration_indices(i))
                end if
            end do
        end if

        ! Analyze module procedures
        if (allocated(mod_node%procedure_indices)) then
            do i = 1, size(mod_node%procedure_indices)
                if (mod_node%procedure_indices(i) > 0) then
                    ! Use infer_and_store_type instead of creating temporary variables
                    call infer_and_store_type(ctx, arena, mod_node%procedure_indices(i))
                end if
            end do
        end if

        ! Leave module scope
        call ctx%scopes%leave_scope()

        ! Modules don't have a type value
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_module

    ! Analyze declaration node
    function analyze_declaration(ctx, arena, decl, decl_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_node), intent(inout) :: decl
        integer, intent(in) :: decl_index
        type(mono_type_t) :: typ
        type(poly_type_t) :: var_scheme

        ! Determine type from declaration
        select case (trim(decl%type_name))
        case ('integer', 'integer(kind=4)', 'integer(4)')
            typ = create_mono_type(TINT)
        case ('real', 'real(kind=4)', 'real(4)')
            typ = create_mono_type(TREAL)
        case ('real(kind=8)', 'real(8)', 'double precision')
            typ = create_mono_type(TREAL)  ! Uses default Fortran real precision
        case ('character')
            typ = create_mono_type(TCHAR, char_size=1)  ! Basic character support
        case default
            ! Unknown type - use type variable
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
        end select
        
        ! Set allocation attributes
        typ%alloc_info%is_allocatable = decl%is_allocatable
        typ%alloc_info%is_pointer = decl%is_pointer

        ! If this is an array declaration, wrap the type in an array type
        if (decl%is_array) then
            block
                type(mono_type_t), allocatable :: array_args(:)
                type(allocation_info_t) :: saved_alloc_info
                
                ! Save allocation info before wrapping in array type
                saved_alloc_info = typ%alloc_info
                
                allocate (array_args(1))
                array_args(1) = typ  ! Element type
                typ = create_mono_type(TARRAY, args=array_args)
                
                ! Restore allocation info to the array type
                typ%alloc_info = saved_alloc_info
                ! TODO: Set array size from dimension_indices
            end block
        end if

        ! Check if this variable already exists in scope
        block
            type(poly_type_t), allocatable :: existing_scheme

            call ctx%scopes%lookup(decl%var_name, existing_scheme)

            if (allocated(existing_scheme)) then
                ! Variable already declared - this might be a redeclaration
                ! of a parameter
                ! For explicit function parameters, we need to unify the types
                block
                    type(mono_type_t) :: existing_typ
                    type(substitution_t) :: s

                    ! Instantiate the existing scheme
                    existing_typ = ctx%instantiate(existing_scheme)

                    ! Unify with the declared type
                    s = ctx%unify(existing_typ, typ)
                    call ctx%compose_with_subst(s)
                end block
            else
                ! New variable declaration
                var_scheme = ctx%generalize(typ)
                call ctx%scopes%define(decl%var_name, var_scheme)
            end if
        end block

        ! If there's an initializer, check type compatibility
        if (decl%has_initializer .and. decl%initializer_index > 0) then
            block
                type(mono_type_t) :: init_typ
                type(substitution_t) :: s

                init_typ = ctx%infer(arena, decl%initializer_index)
                s = ctx%unify(typ, init_typ)
                call ctx%compose_with_subst(s)
            end block
        end if

    end function analyze_declaration

    ! Analyze function definition
    function analyze_function_def(ctx, arena, func_def, func_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(mono_type_t) :: typ, param_type, return_type
        type(mono_type_t), allocatable :: param_types(:)
        type(poly_type_t) :: func_scheme
        integer :: i
        logical :: has_character_operations

        ! Enter function scope
        call ctx%scopes%enter_function(func_def%name)

        ! Clear parameter tracker for new function
        call ctx%param_tracker%clear()

        ! First pass: check if function body has character operations
        has_character_operations = check_for_character_operations(arena, func_def)

        ! Process parameters and add to local scope
        if (allocated(func_def%param_indices)) then
            allocate (param_types(size(func_def%param_indices)))
            do i = 1, size(func_def%param_indices)
                ! Check if parameter is used in character operations
                if (has_character_operations .and. &
                    param_used_in_character_op(arena, func_def, i)) then
                    ! Pre-constrain to character type
                    param_types(i) = create_mono_type(TCHAR, char_size=1)
                else
                    ! Assign fresh type variable
                    param_types(i) = create_mono_type(TVAR, var=ctx%fresh_type_var())
                end if

                ! Add parameter to local scope - get from arena
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    ! Store inferred type on the parameter node itself
                    if (.not. allocated(arena%entries(func_def%param_indices(i))% &
                                       node%inferred_type)) then
                        allocate(arena%entries(func_def%param_indices(i))% &
                                node%inferred_type)
                    end if
                    arena%entries(func_def%param_indices(i))%node%inferred_type = &
                        param_types(i)
                    
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        call ctx%scopes%define(param%name, &
                      create_poly_type(forall_vars=[type_var_t::], mono=param_types(i)))
                    type is (parameter_declaration_node)
                        ! Track parameter with intent
                        call ctx%param_tracker%add_parameter(param%name, &
                            intent_type_to_string(param%intent_type), &
                            param%is_optional)
                        call ctx%scopes%define(param%name, &
                      create_poly_type(forall_vars=[type_var_t::], mono=param_types(i)))
                    end select
                end if
            end do
        else
            allocate (param_types(0))
        end if

        ! Analyze function body statements
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                ! Analyze body statement using arena indexing
                call infer_and_store_type(ctx, arena, func_def%body_indices(i))
            end do
        end if

        ! Determine return type
      if (allocated(func_def%return_type) .and. len_trim(func_def%return_type) > 0) then
            ! Use explicit return type
            select case (trim(func_def%return_type))
            case ('integer', 'integer(kind=4)', 'integer(4)')
                return_type = create_mono_type(TINT)
            case ('real', 'real(kind=4)', 'real(4)')
                return_type = create_mono_type(TREAL)
            case ('real(kind=8)', 'real(8)', 'double precision')
                return_type = create_mono_type(TREAL)  ! Uses default Fortran real precision
            case ('character')
                return_type = create_mono_type(TCHAR, char_size=1)
            case default
                ! Unknown type - use type variable
                return_type = create_mono_type(TVAR, var=ctx%fresh_type_var())
            end select
        else
            ! Infer return type - check if function returns character values
            if (has_character_operations .and. &
                function_returns_character(arena, func_def)) then
                return_type = create_mono_type(TCHAR, char_size=1)
            else
                ! Use a fresh type variable
                return_type = create_mono_type(TVAR, var=ctx%fresh_type_var())
            end if
        end if

        ! Apply current substitutions to parameter types
        ! This ensures parameter types are updated with constraints from body
        if (allocated(param_types)) then
            do i = 1, size(param_types)
                param_types(i) = ctx%apply_subst_to_type(param_types(i))
                
                ! If still a type variable and used in character operations,
                ! set to character type
                if (param_types(i)%kind == TVAR .and. has_character_operations &
                    .and. param_used_in_character_op(arena, func_def, i)) then
                    param_types(i) = create_mono_type(TCHAR, char_size=1)
                end if
                
                ! Update the inferred type on the parameter node
                if (func_def%param_indices(i) > 0 .and. &
                    func_def%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                        if (.not. allocated(arena%entries(func_def%param_indices(i))% &
                                           node%inferred_type)) then
                            allocate(arena%entries(func_def%param_indices(i))% &
                                    node%inferred_type)
                        end if
                        arena%entries(func_def%param_indices(i))%node%inferred_type = &
                            param_types(i)
                    end if
                end if
            end do
        end if

        ! Build function type
        if (size(param_types) == 0) then
            typ = return_type
        else if (size(param_types) == 1) then
            typ = create_fun_type(param_types(1), return_type)
        else
            ! Multi-argument function - curry from right to left
            typ = return_type
            do i = size(param_types), 1, -1
                typ = create_fun_type(param_types(i), typ)
            end do
        end if

        ! Leave function scope
        call ctx%scopes%leave_scope()

        ! Add function to parent scope
        func_scheme = ctx%generalize(typ)
        call ctx%scopes%define(func_def%name, func_scheme)

    end function analyze_function_def

    ! Analyze subroutine definition
    function analyze_subroutine_def(ctx, arena, sub_def, sub_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        type(mono_type_t) :: typ
        type(poly_type_t) :: sub_scheme
        integer :: i

        ! Enter subroutine scope
        call ctx%scopes%enter_subroutine(sub_def%name)

        ! Clear parameter tracker for new subroutine
        call ctx%param_tracker%clear()

        ! Process parameters and add to local scope
        if (allocated(sub_def%param_indices)) then
            do i = 1, size(sub_def%param_indices)
                ! For now, assign fresh type variables to parameters
                if (allocated(arena%entries(sub_def%param_indices(i))%node)) then
                    select type (param => arena%entries(sub_def%param_indices(i))%node)
                    type is (identifier_node)
                        call ctx%scopes%define(param%name, &
                                          create_poly_type(forall_vars=[type_var_t::], &
                                 mono=create_mono_type(TVAR, var=ctx%fresh_type_var())))
                    type is (parameter_declaration_node)
                        ! Track parameter with intent
                        call ctx%param_tracker%add_parameter(param%name, &
                            intent_type_to_string(param%intent_type), &
                            param%is_optional)
                        call ctx%scopes%define(param%name, &
                                          create_poly_type(forall_vars=[type_var_t::], &
                                 mono=create_mono_type(TVAR, var=ctx%fresh_type_var())))
                    end select
                end if
            end do
        end if

        ! Analyze subroutine body statements
        if (allocated(sub_def%body_indices)) then
            do i = 1, size(sub_def%body_indices)
                ! Analyze body statement using arena indexing
                call infer_and_store_type(ctx, arena, sub_def%body_indices(i))
            end do
        end if

        ! Leave subroutine scope
        call ctx%scopes%leave_scope()

        ! Subroutines have unit type
        typ = create_mono_type(TINT)  ! Unit type

        ! Add subroutine to parent scope
        sub_scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
        call ctx%scopes%define(sub_def%name, sub_scheme)

    end function analyze_subroutine_def

    ! Check if function body contains character operations
    function check_for_character_operations(arena, func_def) result(has_char_ops)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        logical :: has_char_ops
        integer :: i
        
        has_char_ops = .false.
        
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                if (node_has_character_op(arena, func_def%body_indices(i))) then
                    has_char_ops = .true.
                    return
                end if
            end do
        end if
    end function check_for_character_operations
    
    ! Check if a node contains character operations with depth limiting
    function node_has_character_op(arena, node_index) result(has_char)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: has_char
        integer, parameter :: MAX_DEPTH = 50
        
        has_char = node_has_char_op_limited(arena, node_index, 0, MAX_DEPTH)
    end function node_has_character_op
    
    ! Implementation with depth limiting to prevent infinite recursion
    recursive function node_has_char_op_limited(arena, node_index, current_depth, max_depth) result(has_char)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, current_depth, max_depth
        logical :: has_char
        
        has_char = .false.
        
        ! Depth limit to prevent stack overflow
        if (current_depth >= max_depth) return
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (trim(node%operator) == "//") then
                has_char = .true.
                return
            end if
            ! Check operands with depth tracking
            has_char = node_has_char_op_limited(arena, node%left_index, current_depth + 1, max_depth) .or. &
                      node_has_char_op_limited(arena, node%right_index, current_depth + 1, max_depth)
        type is (assignment_node)
            has_char = node_has_char_op_limited(arena, node%value_index, current_depth + 1, max_depth)
        type is (literal_node)
            has_char = (node%literal_kind == LITERAL_STRING)
        end select
    end function node_has_char_op_limited
    
    ! Check if a parameter is used in character operations
    function param_used_in_character_op(arena, func_def, param_idx) result(used)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        integer, intent(in) :: param_idx
        logical :: used
        character(len=:), allocatable :: param_name
        integer :: i
        
        used = .false.
        
        ! Get parameter name
        if (param_idx <= 0 .or. param_idx > size(func_def%param_indices)) return
        if (func_def%param_indices(param_idx) <= 0 .or. &
            func_def%param_indices(param_idx) > arena%size) return
        if (.not. allocated(arena%entries(func_def%param_indices(param_idx))%node)) &
            return
        
        select type (param => arena%entries(func_def%param_indices(param_idx))%node)
        type is (identifier_node)
            param_name = param%name
        type is (parameter_declaration_node)
            param_name = param%name
        class default
            return
        end select
        
        if (.not. allocated(param_name)) return
        
        ! Check if parameter is used in character operations in body
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                if (identifier_in_char_op(arena, func_def%body_indices(i), &
                                         param_name)) then
                    used = .true.
                    return
                end if
            end do
        end if
    end function param_used_in_character_op
    
    ! Check if an identifier is used in character operations with depth limiting
    function identifier_in_char_op(arena, node_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name
        logical :: found
        integer, parameter :: MAX_DEPTH = 50
        
        found = identifier_in_char_op_limited(arena, node_index, name, 0, MAX_DEPTH)
    end function identifier_in_char_op
    
    ! Implementation with depth limiting to prevent infinite recursion
    recursive function identifier_in_char_op_limited(arena, node_index, name, current_depth, max_depth) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, current_depth, max_depth
        character(len=*), intent(in) :: name
        logical :: found
        
        found = .false.
        
        ! Depth limit to prevent stack overflow
        if (current_depth >= max_depth) return
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (trim(node%operator) == "//") then
                ! Check if either operand is our identifier
                if (is_identifier_named(arena, node%left_index, name) .or. &
                    is_identifier_named(arena, node%right_index, name)) then
                    found = .true.
                    return
                end if
            end if
            ! Recursively check operands with depth tracking
            found = identifier_in_char_op_limited(arena, node%left_index, name, current_depth + 1, max_depth) .or. &
                   identifier_in_char_op_limited(arena, node%right_index, name, current_depth + 1, max_depth)
        type is (assignment_node)
            ! Check if identifier is the assignment target of a character expression
            if (is_identifier_named(arena, node%target_index, name)) then
                found = node_has_character_op(arena, node%value_index)
            else
                ! Check if identifier is used in the assignment value
                found = identifier_in_char_op_limited(arena, node%value_index, name, current_depth + 1, max_depth)
            end if
        end select
    end function identifier_in_char_op_limited
    
    ! Check if a node is an identifier with a specific name
    function is_identifier_named(arena, node_index, name) result(is_match)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name
        logical :: is_match
        
        is_match = .false.
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) then
                is_match = (trim(node%name) == trim(name))
            end if
        end select
    end function is_identifier_named
    
    ! Check if a function returns character values
    function function_returns_character(arena, func_def) result(returns_char)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        logical :: returns_char
        integer :: i
        
        returns_char = .false.
        
        ! Check if any assignment to function name involves character operations
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                if (assignment_to_function_is_char(arena, &
                                                  func_def%body_indices(i), &
                                                  func_def%name)) then
                    returns_char = .true.
                    return
                end if
            end do
        end if
    end function function_returns_character
    
    ! Check if assignment to function name involves character operations
    function assignment_to_function_is_char(arena, node_index, func_name) &
             result(is_char)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: func_name
        logical :: is_char
        
        is_char = .false.
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Check if target is the function name
            if (is_identifier_named(arena, node%target_index, func_name)) then
                ! Check if value is character expression
                is_char = node_has_character_op(arena, node%value_index)
            end if
        end select
    end function assignment_to_function_is_char

    ! Analyze if node with block scopes
    function analyze_if_node(ctx, arena, if_stmt, if_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(inout) :: if_stmt
        integer, intent(in) :: if_index
        type(mono_type_t) :: typ
        integer :: i, j

        ! Analyze condition
        if (if_stmt%condition_index > 0) then
            block
                type(mono_type_t) :: cond_type
                cond_type = ctx%infer(arena, if_stmt%condition_index)
                ! Condition should be logical type
            end block
        end if

        ! Enter then block scope
        call ctx%scopes%enter_block()

        ! Analyze then body statements
        if (allocated(if_stmt%then_body_indices)) then
            do i = 1, size(if_stmt%then_body_indices)
                if (if_stmt%then_body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, if_stmt%then_body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave then block scope
        call ctx%scopes%leave_scope()

        ! Analyze elseif blocks
        if (allocated(if_stmt%elseif_blocks)) then
            do i = 1, size(if_stmt%elseif_blocks)
                call ctx%scopes%enter_block()

                ! Analyze elseif condition
                if (if_stmt%elseif_blocks(i)%condition_index > 0) then
                    block
                        type(mono_type_t) :: cond_type
                  cond_type = ctx%infer(arena, if_stmt%elseif_blocks(i)%condition_index)
                    end block
                end if

                ! Analyze elseif body
                if (allocated(if_stmt%elseif_blocks(i)%body_indices)) then
                    do j = 1, size(if_stmt%elseif_blocks(i)%body_indices)
                        if (if_stmt%elseif_blocks(i)%body_indices(j) > 0) then
                            block
                                type(mono_type_t) :: stmt_type
                  stmt_type = ctx%infer(arena, if_stmt%elseif_blocks(i)%body_indices(j))
                            end block
                        end if
                    end do
                end if

                call ctx%scopes%leave_scope()
            end do
        end if

        ! Analyze else block
        if (allocated(if_stmt%else_body_indices)) then
            call ctx%scopes%enter_block()
            do i = 1, size(if_stmt%else_body_indices)
                if (if_stmt%else_body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, if_stmt%else_body_indices(i))
                    end block
                end if
            end do
            call ctx%scopes%leave_scope()
        end if

        ! If statements have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_if_node

    ! Analyze do loop with block scope
    function analyze_do_loop(ctx, arena, do_stmt, do_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(inout) :: do_stmt
        integer, intent(in) :: do_index
        type(mono_type_t) :: typ, loop_var_type
        type(poly_type_t) :: loop_var_scheme
        integer :: i

        ! Enter loop block scope
        call ctx%scopes%enter_block()

        ! Add loop variable to scope
        loop_var_type = create_mono_type(TINT)  ! Loop variables are integers
      loop_var_scheme = create_poly_type(forall_vars=[type_var_t::], mono=loop_var_type)
        call ctx%scopes%define(do_stmt%var_name, loop_var_scheme)

        ! Analyze loop bounds
        if (do_stmt%start_expr_index > 0) then
            block
                type(mono_type_t) :: start_type
                start_type = ctx%infer(arena, do_stmt%start_expr_index)
            end block
        end if
        if (do_stmt%end_expr_index > 0) then
            block
                type(mono_type_t) :: end_type
                end_type = ctx%infer(arena, do_stmt%end_expr_index)
            end block
        end if
        if (do_stmt%step_expr_index > 0) then
            block
                type(mono_type_t) :: step_type
                step_type = ctx%infer(arena, do_stmt%step_expr_index)
            end block
        end if

        ! Analyze loop body
        if (allocated(do_stmt%body_indices)) then
            do i = 1, size(do_stmt%body_indices)
                if (do_stmt%body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, do_stmt%body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave loop block scope
        call ctx%scopes%leave_scope()

        ! Do loops have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_do_loop

    ! Analyze do while loop with block scope
    function analyze_do_while(ctx, arena, do_while_stmt, do_while_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_while_node), intent(inout) :: do_while_stmt
        integer, intent(in) :: do_while_index
        type(mono_type_t) :: typ
        integer :: i

        ! Enter loop block scope
        call ctx%scopes%enter_block()

        ! Analyze condition
        if (do_while_stmt%condition_index > 0) then
            block
                type(mono_type_t) :: cond_type
                cond_type = ctx%infer(arena, do_while_stmt%condition_index)
            end block
        end if

        ! Analyze loop body
        if (allocated(do_while_stmt%body_indices)) then
            do i = 1, size(do_while_stmt%body_indices)
                if (do_while_stmt%body_indices(i) > 0) then
                    block
                        type(mono_type_t) :: stmt_type
                        stmt_type = ctx%infer(arena, do_while_stmt%body_indices(i))
                    end block
                end if
            end do
        end if

        ! Leave loop block scope
        call ctx%scopes%leave_scope()

        ! Do while loops have unit type
        typ = create_mono_type(TINT)  ! Unit type
    end function analyze_do_while

    ! Deep copy procedures for semantic_context_t
    function semantic_context_deep_copy(this) result(copy)
        class(semantic_context_t), intent(in) :: this
        type(semantic_context_t) :: copy

        copy%env = this%env              ! Uses type_env_t assignment (deep copy)
        copy%scopes = this%scopes        ! Uses scope_stack_t assignment (deep copy)
        copy%next_var_id = this%next_var_id
        copy%subst = this%subst          ! Uses substitution_t assignment (deep copy)
        ! Uses temp_tracker_t assignment (deep copy)
        copy%temp_tracker = this%temp_tracker
    end function semantic_context_deep_copy

    subroutine semantic_context_assign(lhs, rhs)
        class(semantic_context_t), intent(out) :: lhs
        type(semantic_context_t), intent(in) :: rhs

        lhs%env = rhs%env                ! Uses type_env_t assignment (deep copy)
        lhs%scopes = rhs%scopes          ! Uses scope_stack_t assignment (deep copy)
        lhs%next_var_id = rhs%next_var_id
        lhs%subst = rhs%subst            ! Uses substitution_t assignment (deep copy)
        ! Uses temp_tracker_t assignment (deep copy)
        lhs%temp_tracker = rhs%temp_tracker
    end subroutine semantic_context_assign

    ! Infer type of array literal
    function infer_array_literal(this, arena, arr_node, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: arr_node
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: elem_type, current_type
        type(mono_type_t), allocatable :: array_args(:)
        integer :: i
        logical :: all_same_type

        ! If no elements, default to integer array
        if (.not. allocated(arr_node%element_indices) .or. &
            size(arr_node%element_indices) == 0) then
            allocate (array_args(1))
            array_args(1) = create_mono_type(TINT)
            typ = create_mono_type(TARRAY, args=array_args)
            typ%size = 0
            return
        end if

        ! Infer type of first element
        elem_type = this%infer(arena, arr_node%element_indices(1))
        all_same_type = .true.

        ! Check if all elements have the same type
        do i = 2, size(arr_node%element_indices)
            current_type = this%infer(arena, arr_node%element_indices(i))

            ! If types differ, we need to find common type
            if (current_type%kind /= elem_type%kind) then
                all_same_type = .false.
                ! Promote to real if mixing integer and real
                if ((elem_type%kind == TINT .and. current_type%kind == TREAL) .or. &
                    (elem_type%kind == TREAL .and. current_type%kind == TINT)) then
                    elem_type = create_mono_type(TREAL)
                    elem_type%size = 8  ! real(8)
                end if
            else if (current_type%kind == TCHAR .and. elem_type%kind == TCHAR) then
                ! For character types, find maximum length
                if (current_type%size > elem_type%size) then
                    elem_type%size = current_type%size
                    all_same_type = .false.  ! Different lengths
                else if (current_type%size /= elem_type%size) then
                    all_same_type = .false.  ! Different lengths
                end if
            end if
        end do

        ! Create array type with element type in args(1)
        allocate (array_args(1))
        array_args(1) = elem_type
        typ = create_mono_type(TARRAY, args=array_args)
        typ%size = size(arr_node%element_indices)

    end function infer_array_literal

    ! Infer type of implied DO loop in array constructor
    function infer_implied_do_loop(this, arena, do_node, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(in) :: do_node
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: elem_type, start_type, end_type, step_type
        type(mono_type_t), allocatable :: array_args(:)
        type(poly_type_t) :: loop_var_scheme

        ! Enter a new scope for the implied DO loop
        call this%scopes%enter_block()

        ! Add loop variable to scope as integer
        loop_var_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                           mono=create_mono_type(TINT))
        call this%scopes%define(do_node%var_name, loop_var_scheme)

        ! Infer types of bounds
        if (do_node%start_expr_index > 0) then
            start_type = this%infer(arena, do_node%start_expr_index)
        end if

        if (do_node%end_expr_index > 0) then
            end_type = this%infer(arena, do_node%end_expr_index)
        end if

        if (do_node%step_expr_index > 0) then
            step_type = this%infer(arena, do_node%step_expr_index)
        end if

        ! Infer type of the body expression
        if (allocated(do_node%body_indices) .and. size(do_node%body_indices) > 0) then
            elem_type = this%infer(arena, do_node%body_indices(1))
        else
            ! Default to integer if no body
            elem_type = create_mono_type(TINT)
        end if

        ! Leave the implied DO scope
        call this%scopes%leave_scope()

        ! Return array type with element type
        allocate (array_args(1))
        array_args(1) = elem_type
        typ = create_mono_type(TARRAY, args=array_args)
        ! Size is not known at compile time for implied DO
        typ%size = -1

    end function infer_implied_do_loop

    ! Infer type of array slice with bounds checking
    function infer_array_slice(ctx, arena, slice_node) result(typ)
        class(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(inout) :: slice_node
        type(mono_type_t) :: typ
        type(mono_type_t) :: array_type, element_type
        type(mono_type_t), allocatable :: array_args(:)
        integer :: i
        
        ! Get the type of the array being sliced
        if (slice_node%array_index > 0 .and. slice_node%array_index <= arena%size) then
            array_type = ctx%infer(arena, slice_node%array_index)
            
            ! Check if this is actually a character substring operation
            if (array_type%kind == TCHAR) then
                ! This is a character substring, not an array slice!
                ! Set the flag to indicate this is a character substring
                call set_character_substring_flag(arena, slice_node)
                typ = create_mono_type(TCHAR)  ! Result is also character
                return
            end if
            
            ! Validate this is actually an array type
            if (array_type%kind == TARRAY .and. allocated(array_type%args)) then
                element_type = array_type%args(1)
                
                ! Validate array bounds for each dimension
                call ctx%validate_bounds(arena, slice_node)
                
                ! For array slicing, result type depends on the slice:
                ! - Single index: returns element type
                ! - Range (start:end): returns array of same element type
                ! For now, assume it returns an array of the element type
                allocate(array_args(1))
                array_args(1) = element_type
                typ = create_mono_type(TARRAY, args=array_args)
                typ%size = -1  ! Size not known at compile time for slices
            else
                ! Not an array - this is an error
                print *, "ERROR: Attempting to slice non-array type"
                typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            end if
        else
            ! Invalid array index
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
        end if
    end function infer_array_slice

    ! Validate array access bounds
    subroutine validate_array_access_bounds(ctx, arena, slice_node)
        class(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        integer :: i
        
        ! For each dimension, validate that the bounds are reasonable
        do i = 1, slice_node%num_dimensions
            if (slice_node%bounds_indices(i) > 0) then
                ! TODO: For full bounds checking, we would need to:
                ! 1. Evaluate the bound expressions at compile time if possible
                ! 2. Generate runtime checks for dynamic bounds
                ! 3. Validate that lower <= upper and stride != 0
                ! For now, just mark that we've seen a bounds check
                continue
            end if
        end do
        
        ! Basic bounds validation - comprehensive checking could be added here
        ! This could include:
        ! - Compile-time constant bounds checking
        ! - Runtime bounds check code generation
        ! - Warning generation for potentially unsafe accesses
    end subroutine validate_array_access_bounds

    ! Check shape conformance between arrays
    logical function check_array_shape_conformance(ctx, spec1, spec2) &
        result(conformable)
        class(semantic_context_t), intent(inout) :: ctx
        type(array_spec_t), intent(in) :: spec1, spec2
        integer :: i
        
        conformable = .false.
        
        ! Arrays must have the same rank to be conformable
        if (spec1%rank /= spec2%rank) return
        
        ! Scalar arrays are always conformable
        if (spec1%rank == 0) then
            conformable = .true.
            return
        end if
        
        ! Check each dimension for conformability
        if (allocated(spec1%bounds) .and. allocated(spec2%bounds)) then
            do i = 1, spec1%rank
                ! For compile-time constant bounds, check exact sizes
                if (spec1%bounds(i)%is_constant_lower .and. &
                    spec1%bounds(i)%is_constant_upper .and. &
                    spec2%bounds(i)%is_constant_lower .and. &
                    spec2%bounds(i)%is_constant_upper) then
                    
                    if ((spec1%bounds(i)%const_upper - spec1%bounds(i)%const_lower) /= &
                        (spec2%bounds(i)%const_upper - &
                         spec2%bounds(i)%const_lower)) then
                        return  ! Different sizes, not conformable
                    end if
                else
                    ! For dynamic bounds, assume conformable (runtime check needed)
                    continue
                end if
            end do
        end if
        
        conformable = .true.
    end function check_array_shape_conformance

    ! Public interface for array bounds validation
    subroutine validate_array_bounds(arena, node_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: error_msg
        type(array_slice_node), pointer :: slice_ptr
        
        error_msg = ""
        
        if (node_index <= 0 .or. node_index > arena%size) then
            error_msg = "Invalid node index for bounds validation"
            return
        end if
        
        slice_ptr => get_array_slice_node(arena, node_index)
        if (associated(slice_ptr)) then
            ! TODO: Implement comprehensive bounds validation
            ! For now, just check basic structure
            if (slice_ptr%num_dimensions < 0) then
                error_msg = "Invalid number of dimensions in array slice"
            else if (slice_ptr%num_dimensions > 10) then
                error_msg = "Too many dimensions in array slice (max 10)"
            end if
        else
            error_msg = "Node is not an array slice node"
        end if
    end subroutine validate_array_bounds

    ! Public interface for shape conformance checking
    logical function check_shape_conformance(spec1, spec2) result(conformable)
        type(array_spec_t), intent(in) :: spec1, spec2
        type(semantic_context_t) :: dummy_ctx
        
        ! Create dummy context for the method call
        dummy_ctx = create_semantic_context()
        conformable = dummy_ctx%check_conformance(spec1, spec2)
    end function check_shape_conformance
    
    ! Set character substring flag on array_slice_node
    subroutine set_character_substring_flag(arena, slice_node)
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        integer :: node_index, i
        
        ! Find the index of this node in the arena
        node_index = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (array_slice_node)
                    if (node%array_index == slice_node%array_index .and. &
                        node%num_dimensions == slice_node%num_dimensions) then
                        node_index = i
                        exit
                    end if
                end select
            end if
        end do
        
        if (node_index > 0) then
            ! Set the character substring flag
            select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
                node%is_character_substring = .true.
            end select
        end if
    end subroutine set_character_substring_flag

    ! Assignment operator for undeclared_variable_t
    subroutine undeclared_variable_assign(lhs, rhs)
        class(undeclared_variable_t), intent(inout) :: lhs
        class(undeclared_variable_t), intent(in) :: rhs
        
        if (allocated(rhs%name)) lhs%name = rhs%name
        lhs%usage_type = rhs%usage_type
        lhs%first_usage_node = rhs%first_usage_node
        lhs%is_read = rhs%is_read
        lhs%is_written = rhs%is_written
        lhs%is_array_access = rhs%is_array_access
        lhs%inferred_type = rhs%inferred_type
        lhs%inferred_intent = rhs%inferred_intent
        lhs%usage_pattern = rhs%usage_pattern
    end subroutine undeclared_variable_assign

    ! Collect undeclared variables in a function or subroutine scope
    subroutine collect_undeclared_variables(this, arena, scope_index, &
                                           undeclared_vars)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(undeclared_variable_t), allocatable, intent(out) :: &
            undeclared_vars(:)
        
        type(undeclared_variable_t), allocatable :: temp_vars(:)
        integer :: temp_count
        integer :: i
        
        ! Initialize temporary array for collecting variables
        allocate(temp_vars(100))  ! Start with reasonable capacity
        temp_count = 0
        
        if (scope_index <= 0 .or. scope_index > arena%size) then
            allocate(undeclared_vars(0))
            return
        end if
        
        if (.not. allocated(arena%entries(scope_index)%node)) then
            allocate(undeclared_vars(0))
            return
        end if
        
        ! Process function or subroutine node
        select type (node => arena%entries(scope_index)%node)
        type is (function_def_node)
            call process_function_scope(this, arena, node, temp_vars, temp_count)
        type is (subroutine_def_node)
            call process_subroutine_scope(this, arena, node, temp_vars, &
                                        temp_count)
        class default
            allocate(undeclared_vars(0))
            return
        end select
        
        ! Copy results to output array
        allocate(undeclared_vars(temp_count))
        do i = 1, temp_count
            undeclared_vars(i) = temp_vars(i)
        end do
        
        ! Perform intent inference for parameter variables
        do i = 1, temp_count
            if (undeclared_vars(i)%usage_type == USAGE_PARAMETER) then
                ! Collect detailed usage pattern for this variable
                call collect_variable_usage_pattern(this, arena, scope_index, &
                                                   undeclared_vars(i)%name, &
                                                   undeclared_vars(i)%usage_pattern)
                
                ! Determine intent from the collected pattern
                call determine_intent_from_pattern(undeclared_vars(i)%usage_pattern, &
                                                  undeclared_vars(i)%inferred_intent)
            end if
        end do
    end subroutine collect_undeclared_variables

    ! Process function scope for undeclared variables
    subroutine process_function_scope(ctx, arena, func_node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i
        
        ! Enter function scope
        call ctx%scopes%enter_function(func_node%name)
        
        ! Add function parameters to scope
        if (allocated(func_node%param_indices)) then
            do i = 1, size(func_node%param_indices)
                call add_parameter_to_scope(ctx, arena, &
                                          func_node%param_indices(i))
            end do
        end if
        
        ! Add result variable if specified
        if (allocated(func_node%result_variable)) then
            ! Result variable is implicitly declared
            call add_implicit_variable_to_scope(ctx, func_node%result_variable, &
                                              func_node%return_type)
        else
            ! Function name is the result variable
            call add_implicit_variable_to_scope(ctx, func_node%name, &
                                              func_node%return_type)
        end if
        
        ! Process function body to find all variable references
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                call collect_variables_from_node(ctx, arena, &
                                                func_node%body_indices(i), &
                                                vars, count)
            end do
        end if
        
        ! Leave function scope
        call ctx%scopes%leave_scope()
    end subroutine process_function_scope

    ! Process subroutine scope for undeclared variables
    subroutine process_subroutine_scope(ctx, arena, sub_node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i
        
        ! Enter subroutine scope
        call ctx%scopes%enter_subroutine(sub_node%name)
        
        ! Add subroutine parameters to scope
        if (allocated(sub_node%param_indices)) then
            do i = 1, size(sub_node%param_indices)
                call add_parameter_to_scope(ctx, arena, &
                                          sub_node%param_indices(i))
            end do
        end if
        
        ! Process subroutine body to find all variable references
        if (allocated(sub_node%body_indices)) then
            do i = 1, size(sub_node%body_indices)
                call collect_variables_from_node(ctx, arena, &
                                                sub_node%body_indices(i), &
                                                vars, count)
            end do
        end if
        
        ! Leave subroutine scope
        call ctx%scopes%leave_scope()
    end subroutine process_subroutine_scope

    ! Add parameter declaration to current scope
    subroutine add_parameter_to_scope(ctx, arena, param_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        
        type(poly_type_t) :: param_scheme
        type(mono_type_t) :: param_type
        
        if (param_index <= 0 .or. param_index > arena%size) return
        if (.not. allocated(arena%entries(param_index)%node)) return
        
        select type (node => arena%entries(param_index)%node)
        type is (parameter_declaration_node)
            ! Create type for parameter based on declaration
            param_type = create_parameter_type(node)
            param_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                          mono=param_type)
            call ctx%scopes%define(node%name, param_scheme)
        type is (declaration_node)
            ! Handle regular declaration as parameter
            if (allocated(node%var_name)) then
                param_type = create_declaration_type(node)
                param_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                              mono=param_type)
                call ctx%scopes%define(node%var_name, param_scheme)
            end if
        end select
    end subroutine add_parameter_to_scope

    ! Add implicit variable (like result variable) to scope
    subroutine add_implicit_variable_to_scope(ctx, var_name, type_name)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in), optional :: type_name
        
        type(poly_type_t) :: var_scheme
        type(mono_type_t) :: var_type
        
        if (present(type_name)) then
            var_type = create_type_from_name(type_name)
        else
            var_type = create_mono_type(TREAL)  ! Default type
        end if
        
        var_scheme = create_poly_type(forall_vars=[type_var_t::], mono=var_type)
        call ctx%scopes%define(var_name, var_scheme)
    end subroutine add_implicit_variable_to_scope

    ! Recursively collect variables from AST node
    recursive subroutine collect_variables_from_node(ctx, arena, node_index, &
                                                    vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            ! Add declared variables to scope
            call add_declaration_to_scope(ctx, node)
        type is (assignment_node)
            call process_assignment_node(ctx, arena, node, vars, count)
        type is (identifier_node)
            call process_identifier_node(ctx, arena, node, node_index, vars, &
                                        count, .true., .false.)
        type is (binary_op_node)
            call collect_variables_from_node(ctx, arena, node%left_index, &
                                            vars, count)
            call collect_variables_from_node(ctx, arena, node%right_index, &
                                            vars, count)
        type is (call_or_subscript_node)
            call process_call_or_subscript_node(ctx, arena, node, node_index, &
                                               vars, count)
        type is (array_slice_node)
            call process_array_slice_node(ctx, arena, node, vars, count)
        type is (if_node)
            call process_if_node(ctx, arena, node, vars, count)
        type is (do_loop_node)
            call process_do_loop_node(ctx, arena, node, vars, count)
        class default
            ! For other node types, try to traverse children if they exist
            call traverse_generic_node_children(ctx, arena, node_index, vars, &
                                               count)
        end select
    end subroutine collect_variables_from_node

    ! Process assignment node to track variable usage
    subroutine process_assignment_node(ctx, arena, node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        ! Target is being written to
        call mark_node_as_written(ctx, arena, node%target_index, vars, count)
        
        ! Value expression is being read from
        call collect_variables_from_node(ctx, arena, node%value_index, vars, &
                                        count)
    end subroutine process_assignment_node

    ! Process identifier node to check if it's undeclared
    subroutine process_identifier_node(ctx, arena, node, node_index, vars, &
                                     count, is_read, is_written)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(identifier_node), intent(in) :: node
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        logical, intent(in) :: is_read, is_written
        
        type(poly_type_t), allocatable :: scheme
        integer :: var_index
        
        if (.not. allocated(node%name)) return
        
        ! Check if variable is already declared in current scope
        call ctx%scopes%lookup(node%name, scheme)
        
        if (.not. allocated(scheme)) then
            ! Variable is undeclared - add it to the list
            var_index = find_or_add_undeclared_variable(vars, count, node%name, &
                                                       node_index)
            
            ! Update usage flags
            if (is_read) vars(var_index)%is_read = .true.
            if (is_written) vars(var_index)%is_written = .true.
            
            ! Set usage type based on context
            if (vars(var_index)%usage_type == 0) then
                vars(var_index)%usage_type = USAGE_LOCAL_VAR  ! Default
            end if
        end if
    end subroutine process_identifier_node

    ! Process call or subscript node
    subroutine process_call_or_subscript_node(ctx, arena, node, node_index, &
                                             vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i, var_index
        type(poly_type_t), allocatable :: scheme
        
        ! Check if the function/array name is undeclared
        if (allocated(node%name)) then
            call ctx%scopes%lookup(node%name, scheme)
            
            if (.not. allocated(scheme)) then
                var_index = find_or_add_undeclared_variable(vars, count, &
                                                           node%name, &
                                                           node_index)
                vars(var_index)%is_read = .true.
                vars(var_index)%is_array_access = .true.
                
                if (vars(var_index)%usage_type == 0) then
                    vars(var_index)%usage_type = USAGE_LOCAL_VAR
                end if
            end if
        end if
        
        ! Process arguments/subscripts
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (node%arg_indices(i) > 0) then
                    call collect_variables_from_node(ctx, arena, &
                                                    node%arg_indices(i), &
                                                    vars, count)
                end if
            end do
        end if
    end subroutine process_call_or_subscript_node

    ! Process array slice node
    subroutine process_array_slice_node(ctx, arena, node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i
        
        ! Process array name
        if (node%array_index > 0) then
            call mark_node_as_array_access(ctx, arena, node%array_index, vars, &
                                          count)
        end if
        
        ! Process slice bounds
        do i = 1, node%num_dimensions
            if (node%bounds_indices(i) > 0) then
                call collect_variables_from_node(ctx, arena, &
                                                node%bounds_indices(i), &
                                                vars, count)
            end if
        end do
    end subroutine process_array_slice_node

    ! Process if node
    subroutine process_if_node(ctx, arena, node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i, j
        
        ! Process condition
        if (node%condition_index > 0) then
            call collect_variables_from_node(ctx, arena, node%condition_index, &
                                            vars, count)
        end if
        
        ! Process then body
        if (allocated(node%then_body_indices)) then
            do i = 1, size(node%then_body_indices)
                if (node%then_body_indices(i) > 0) then
                    call collect_variables_from_node(ctx, arena, &
                                                    node%then_body_indices(i), &
                                                    vars, count)
                end if
            end do
        end if
        
        ! Process elseif blocks
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                if (node%elseif_blocks(i)%condition_index > 0) then
                    call collect_variables_from_node(ctx, arena, &
                                          node%elseif_blocks(i)%condition_index, &
                                                    vars, count)
                end if
                
                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    do j = 1, size(node%elseif_blocks(i)%body_indices)
                        if (node%elseif_blocks(i)%body_indices(j) > 0) then
                            call collect_variables_from_node(ctx, arena, &
                                      node%elseif_blocks(i)%body_indices(j), &
                                                            vars, count)
                        end if
                    end do
                end if
            end do
        end if
        
        ! Process else body
        if (allocated(node%else_body_indices)) then
            do i = 1, size(node%else_body_indices)
                if (node%else_body_indices(i) > 0) then
                    call collect_variables_from_node(ctx, arena, &
                                                    node%else_body_indices(i), &
                                                    vars, count)
                end if
            end do
        end if
    end subroutine process_if_node

    ! Process do loop node
    subroutine process_do_loop_node(ctx, arena, node, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        integer :: i, var_index
        type(poly_type_t), allocatable :: scheme
        
        ! Process loop variable (if present)
        if (allocated(node%var_name)) then
            call ctx%scopes%lookup(node%var_name, scheme)
            
            if (.not. allocated(scheme)) then
                ! Loop variable is undeclared - add it as local variable
                var_index = find_or_add_undeclared_variable(vars, count, &
                                                           node%var_name, 0)
                vars(var_index)%is_written = .true.
                vars(var_index)%usage_type = USAGE_LOCAL_VAR
            end if
        end if
        
        ! Process loop bounds
        if (node%start_expr_index > 0) then
            call collect_variables_from_node(ctx, arena, node%start_expr_index, &
                                            vars, count)
        end if
        
        if (node%end_expr_index > 0) then
            call collect_variables_from_node(ctx, arena, node%end_expr_index, &
                                            vars, count)
        end if
        
        if (node%step_expr_index > 0) then
            call collect_variables_from_node(ctx, arena, node%step_expr_index, &
                                            vars, count)
        end if
        
        ! Process loop body
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) > 0) then
                    call collect_variables_from_node(ctx, arena, &
                                                    node%body_indices(i), &
                                                    vars, count)
                end if
            end do
        end if
    end subroutine process_do_loop_node

    ! Helper subroutines for variable processing

    ! Mark a node as being written to
    subroutine mark_node_as_written(ctx, arena, node_index, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            call process_identifier_node(ctx, arena, node, node_index, vars, &
                                        count, .false., .true.)
        class default
            ! For other node types, recurse into children
            call collect_variables_from_node(ctx, arena, node_index, vars, &
                                            count)
        end select
    end subroutine mark_node_as_written

    ! Mark a node as being accessed as an array
    subroutine mark_node_as_array_access(ctx, arena, node_index, vars, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            call process_identifier_node(ctx, arena, node, node_index, vars, &
                                        count, .true., .false.)
            ! Mark last added variable as array access
            if (count > 0) then
                vars(count)%is_array_access = .true.
            end if
        end select
    end subroutine mark_node_as_array_access

    ! Find existing undeclared variable or add new one
    function find_or_add_undeclared_variable(vars, count, var_name, &
                                           node_index) result(var_index)
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: node_index
        integer :: var_index
        
        integer :: i
        
        ! Search for existing variable
        do i = 1, count
            if (allocated(vars(i)%name) .and. vars(i)%name == var_name) then
                var_index = i
                return
            end if
        end do
        
        ! Add new variable
        count = count + 1
        var_index = count
        
        if (count <= size(vars)) then
            vars(count)%name = var_name
            vars(count)%first_usage_node = node_index
            vars(count)%usage_type = 0  ! Will be set by caller
            vars(count)%is_read = .false.
            vars(count)%is_written = .false.
            vars(count)%is_array_access = .false.
        end if
    end function find_or_add_undeclared_variable

    ! Add declaration to current scope
    subroutine add_declaration_to_scope(ctx, decl_node)
        type(semantic_context_t), intent(inout) :: ctx
        type(declaration_node), intent(in) :: decl_node
        
        type(poly_type_t) :: var_scheme
        type(mono_type_t) :: var_type
        integer :: i
        
        var_type = create_declaration_type(decl_node)
        var_scheme = create_poly_type(forall_vars=[type_var_t::], mono=var_type)
        
        if (decl_node%is_multi_declaration .and. &
            allocated(decl_node%var_names)) then
            do i = 1, size(decl_node%var_names)
                call ctx%scopes%define(decl_node%var_names(i), var_scheme)
            end do
        else if (allocated(decl_node%var_name)) then
            call ctx%scopes%define(decl_node%var_name, var_scheme)
        end if
    end subroutine add_declaration_to_scope

    ! Create type from parameter declaration
    function create_parameter_type(param_node) result(param_type)
        type(parameter_declaration_node), intent(in) :: param_node
        type(mono_type_t) :: param_type
        
        if (allocated(param_node%type_name)) then
            param_type = create_type_from_name(param_node%type_name)
        else
            param_type = create_mono_type(TREAL)  ! Default type
        end if
    end function create_parameter_type

    ! Create type from declaration node
    function create_declaration_type(decl_node) result(decl_type)
        type(declaration_node), intent(in) :: decl_node
        type(mono_type_t) :: decl_type
        
        if (allocated(decl_node%type_name)) then
            decl_type = create_type_from_name(decl_node%type_name)
        else
            decl_type = create_mono_type(TREAL)  ! Default type
        end if
    end function create_declaration_type

    ! Create type from string name
    function create_type_from_name(type_name) result(mono_type)
        character(len=*), intent(in) :: type_name
        type(mono_type_t) :: mono_type
        
        select case (trim(adjustl(type_name)))
        case ("integer")
            mono_type = create_mono_type(TINT)
        case ("real")
            mono_type = create_mono_type(TREAL)
        case ("character")
            mono_type = create_mono_type(TCHAR)
        case ("logical")
            mono_type = create_mono_type(TLOGICAL)
        case default
            mono_type = create_mono_type(TREAL)  ! Default
        end select
    end function create_type_from_name

    ! Traverse generic node children for unknown node types
    subroutine traverse_generic_node_children(ctx, arena, node_index, vars, &
                                             count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(undeclared_variable_t), intent(inout) :: vars(:)
        integer, intent(inout) :: count
        
        ! Placeholder for generic node traversal
        ! This would need specific implementation based on node structure
        ! For now, we'll skip unknown node types
        continue
    end subroutine traverse_generic_node_children

    ! Assignment operator for type_constraint_t (deep copy)
    subroutine type_constraint_assign(lhs, rhs)
        class(type_constraint_t), intent(inout) :: lhs
        type(type_constraint_t), intent(in) :: rhs
        
        if (allocated(lhs%variable_name)) deallocate(lhs%variable_name)
        
        if (allocated(rhs%variable_name)) then
            lhs%variable_name = rhs%variable_name
        end if
        lhs%required_type = rhs%required_type
        lhs%source_type = rhs%source_type
        lhs%source_node = rhs%source_node
        lhs%confidence = rhs%confidence
    end subroutine type_constraint_assign

    ! Multi-pass constraint-based type inference for undeclared variables
    subroutine infer_types_for_undeclared_variables(this, arena, &
                                                   undeclared_vars)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(undeclared_variable_t), intent(inout) :: undeclared_vars(:)
        
        type(type_constraint_t), allocatable :: constraints(:)
        integer :: i
        
        if (size(undeclared_vars) == 0) return
        
        ! Pass 1: Collect variable usages and operations
        call collect_type_constraints(this, arena, undeclared_vars, constraints)
        
        ! Pass 2: Apply type constraints from expressions
        call apply_expression_constraints(this, constraints, undeclared_vars)
        
        ! Pass 3: Resolve ambiguities with intelligent defaults
        call resolve_ambiguous_types(this, undeclared_vars)
        
        ! Pass 4: Validate type consistency
        call validate_type_consistency(this, arena, undeclared_vars)
    end subroutine infer_types_for_undeclared_variables

    ! Pass 1: Collect type constraints from AST analysis
    subroutine collect_type_constraints(ctx, arena, undeclared_vars, &
                                       constraints)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(undeclared_variable_t), intent(in) :: undeclared_vars(:)
        type(type_constraint_t), allocatable, intent(out) :: constraints(:)
        
        type(type_constraint_t), allocatable :: temp_constraints(:)
        integer :: constraint_count, i, j
        
        ! Initialize constraint collection
        allocate(temp_constraints(1000))  ! Start with large capacity
        constraint_count = 0
        
        ! Analyze each undeclared variable's usage context
        do i = 1, size(undeclared_vars)
            call analyze_variable_usage_context(ctx, arena, &
                undeclared_vars(i), temp_constraints, constraint_count)
        end do
        
        ! Copy to output array
        allocate(constraints(constraint_count))
        do j = 1, constraint_count
            constraints(j) = temp_constraints(j)
        end do
    end subroutine collect_type_constraints

    ! Analyze usage context for a single variable to extract constraints
    subroutine analyze_variable_usage_context(ctx, arena, var, constraints, &
                                             count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(undeclared_variable_t), intent(in) :: var
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        
        ! Traverse AST starting from variable's first usage node
        if (var%first_usage_node > 0 .and. &
            var%first_usage_node <= arena%size) then
            call traverse_for_constraints(ctx, arena, var%first_usage_node, &
                var%name, constraints, count)
        end if
    end subroutine analyze_variable_usage_context

    ! Traverse AST nodes to find type constraints for a variable
    subroutine traverse_for_constraints(ctx, arena, node_index, var_name, &
                                       constraints, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: var_name
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Analyze different node types for type constraints
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call analyze_assignment_constraints(ctx, arena, node, var_name, &
                constraints, count)
        type is (binary_op_node)
            call analyze_binary_op_constraints(ctx, arena, node, var_name, &
                constraints, count)
        type is (call_or_subscript_node)
            call analyze_call_constraints(ctx, arena, node, var_name, &
                constraints, count)
        end select
    end subroutine traverse_for_constraints

    ! Analyze assignment nodes for type constraints
    subroutine analyze_assignment_constraints(ctx, arena, assign_node, var_name, &
                                             constraints, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: assign_node
        character(len=*), intent(in) :: var_name
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        
        type(mono_type_t) :: constraint_type
        
        ! Check if this assignment involves our variable
        if (assign_node%target_index > 0 .and. &
            assign_node%target_index <= arena%size) then
            select type (target_node => &
                arena%entries(assign_node%target_index)%node)
            type is (identifier_node)
                if (allocated(target_node%name) .and. &
                    target_node%name == var_name) then
                    ! Variable is being assigned to - infer from RHS
                    constraint_type = infer_type_from_expression(ctx, arena, &
                        assign_node%value_index)
                    call add_constraint(constraints, count, var_name, &
                        constraint_type, CONSTRAINT_ASSIGNMENT, &
                        assign_node%target_index, 0.9)
                end if
            end select
        end if
    end subroutine analyze_assignment_constraints

    ! Analyze binary operations for type constraints
    subroutine analyze_binary_op_constraints(ctx, arena, binop_node, var_name, &
                                            constraints, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: binop_node
        character(len=*), intent(in) :: var_name
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        
        type(mono_type_t) :: constraint_type
        logical :: var_in_left, var_in_right
        
        ! Check if variable appears in this binary operation
        var_in_left = variable_appears_in_expression(arena, &
            binop_node%left_index, var_name)
        var_in_right = variable_appears_in_expression(arena, &
            binop_node%right_index, var_name)
        
        if (.not. (var_in_left .or. var_in_right)) return
        
        ! Determine type constraint based on operator
        if (allocated(binop_node%operator)) then
            select case (binop_node%operator)
            case ("+", "-", "*", "/", "**")
                ! Arithmetic operations require numeric types
                if (has_real_literal(arena, binop_node%left_index) .or. &
                    has_real_literal(arena, binop_node%right_index)) then
                    constraint_type = create_mono_type(TREAL)
                else
                    constraint_type = create_mono_type(TINT)
                end if
                call add_constraint(constraints, count, var_name, &
                    constraint_type, CONSTRAINT_BINARY_OP, &
                    binop_node%left_index, 0.8)
            case ("//")
                ! String concatenation requires character type
                constraint_type = create_mono_type(TCHAR)
                call add_constraint(constraints, count, var_name, &
                    constraint_type, CONSTRAINT_STRING_OP, &
                    binop_node%left_index, 0.9)
            case (".eq.", ".ne.", ".lt.", ".le.", ".gt.", ".ge.")
                ! Comparison operators - operands can be numeric
                constraint_type = create_mono_type(TREAL)
                call add_constraint(constraints, count, var_name, &
                    constraint_type, CONSTRAINT_BINARY_OP, &
                    binop_node%left_index, 0.7)
            end select
        end if
    end subroutine analyze_binary_op_constraints

    ! Analyze function calls for type constraints
    subroutine analyze_call_constraints(ctx, arena, call_node, var_name, &
                                       constraints, count)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        character(len=*), intent(in) :: var_name
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        
        type(mono_type_t) :: constraint_type
        
        ! Get function name from node
        if (.not. allocated(call_node%name)) return
        
        ! Check if variable appears in function arguments
        if (.not. variable_in_call_args(arena, call_node, var_name)) return
        
        ! Apply function-specific type constraints
        select case (call_node%name)
        case ("sin", "cos", "tan", "exp", "log", "sqrt")
            ! Math functions require real arguments
            constraint_type = create_mono_type(TREAL)
            call add_constraint(constraints, count, var_name, constraint_type, &
                CONSTRAINT_FUNCTION, 0, 0.9)
        case ("len", "trim", "adjustl", "adjustr")
            ! String functions require character arguments
            constraint_type = create_mono_type(TCHAR)
            call add_constraint(constraints, count, var_name, constraint_type, &
                CONSTRAINT_FUNCTION, 0, 0.9)
        case ("size", "lbound", "ubound")
            ! Array inquiry functions - variable could be array or integer
            if (call_node%is_array_access) then
                constraint_type = create_mono_type(TINT)
                call add_constraint(constraints, count, var_name, &
                    constraint_type, CONSTRAINT_ARRAY_INDEX, 0, 0.8)
            end if
        end select
    end subroutine analyze_call_constraints

    ! Pass 2: Apply expression constraints to variables
    subroutine apply_expression_constraints(ctx, constraints, undeclared_vars)
        type(semantic_context_t), intent(inout) :: ctx
        type(type_constraint_t), intent(in) :: constraints(:)
        type(undeclared_variable_t), intent(inout) :: undeclared_vars(:)
        
        integer :: i, j
        type(mono_type_t) :: unified_type
        
        ! Apply constraints to each variable
        do i = 1, size(undeclared_vars)
            call apply_constraints_to_variable(ctx, constraints, &
                undeclared_vars(i))
        end do
    end subroutine apply_expression_constraints

    ! Apply all relevant constraints to a single variable
    subroutine apply_constraints_to_variable(ctx, constraints, var)
        type(semantic_context_t), intent(inout) :: ctx
        type(type_constraint_t), intent(in) :: constraints(:)
        type(undeclared_variable_t), intent(inout) :: var
        
        type(mono_type_t), allocatable :: candidate_types(:)
        real, allocatable :: confidences(:)
        integer :: type_count, i, best_index
        real :: max_confidence
        
        ! Collect all constraints for this variable
        call collect_variable_constraints(constraints, var%name, &
            candidate_types, confidences, type_count)
        
        if (type_count == 0) then
            ! No constraints found - will be handled in Pass 3
            return
        end if
        
        ! Find highest confidence constraint
        max_confidence = 0.0
        best_index = 1
        do i = 1, type_count
            if (confidences(i) > max_confidence) then
                max_confidence = confidences(i)
                best_index = i
            end if
        end do
        
        ! Apply the best constraint
        var%inferred_type = candidate_types(best_index)
    end subroutine apply_constraints_to_variable

    ! Pass 3: Resolve ambiguous types with intelligent defaults
    subroutine resolve_ambiguous_types(ctx, undeclared_vars)
        type(semantic_context_t), intent(inout) :: ctx
        type(undeclared_variable_t), intent(inout) :: undeclared_vars(:)
        
        integer :: i
        
        do i = 1, size(undeclared_vars)
            ! If no type was inferred, apply intelligent defaults
            if (undeclared_vars(i)%inferred_type%kind == 0) then
                call apply_intelligent_default(undeclared_vars(i))
            end if
        end do
    end subroutine resolve_ambiguous_types

    ! Apply intelligent default types based on usage patterns
    subroutine apply_intelligent_default(var)
        type(undeclared_variable_t), intent(inout) :: var
        
        ! Default assignment strategy
        if (var%is_array_access) then
            ! Array indices are typically integers
            var%inferred_type = create_mono_type(TINT)
        else if (var%usage_type == USAGE_PARAMETER) then
            ! Parameters default to real for numeric contexts
            var%inferred_type = create_mono_type(TREAL)
        else
            ! Local variables default to real for flexibility
            var%inferred_type = create_mono_type(TREAL)
        end if
    end subroutine apply_intelligent_default

    ! Pass 4: Validate type consistency across all usages
    subroutine validate_type_consistency(ctx, arena, undeclared_vars)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(undeclared_variable_t), intent(in) :: undeclared_vars(:)
        
        integer :: i
        
        ! Validate each variable's inferred type against its usage
        do i = 1, size(undeclared_vars)
            call validate_variable_type_usage(ctx, arena, undeclared_vars(i))
        end do
    end subroutine validate_type_consistency

    ! Helper functions for constraint analysis

    ! Add a type constraint to the constraints array
    subroutine add_constraint(constraints, count, var_name, constraint_type, &
                             source_type, source_node, confidence)
        type(type_constraint_t), intent(inout) :: constraints(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: var_name
        type(mono_type_t), intent(in) :: constraint_type
        integer, intent(in) :: source_type, source_node
        real, intent(in) :: confidence
        
        if (count >= size(constraints)) return
        
        count = count + 1
        constraints(count)%variable_name = var_name
        constraints(count)%required_type = constraint_type
        constraints(count)%source_type = source_type
        constraints(count)%source_node = source_node
        constraints(count)%confidence = confidence
    end subroutine add_constraint

    ! Infer type from expression node
    function infer_type_from_expression(ctx, arena, expr_index) result(mono_type)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: mono_type
        
        ! Default to real type for safety
        mono_type = create_mono_type(TREAL)
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        ! Analyze expression type based on node type
        ! This would be expanded with more sophisticated analysis
        mono_type = create_mono_type(TREAL)
    end function infer_type_from_expression

    ! Check if variable appears in expression
    function variable_appears_in_expression(arena, expr_index, var_name) &
        result(appears)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        logical :: appears
        
        appears = .false.
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (identifier_node)
            if (allocated(node%name) .and. node%name == var_name) then
                appears = .true.
            end if
        end select
    end function variable_appears_in_expression

    ! Check if expression contains real literals
    function has_real_literal(arena, expr_index) result(has_real)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        logical :: has_real
        
        has_real = .false.
        ! Simplified implementation - would need full expression analysis
    end function has_real_literal

    ! Check if variable appears in function call arguments
    function variable_in_call_args(arena, call_node, var_name) result(in_args)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        character(len=*), intent(in) :: var_name
        logical :: in_args
        
        in_args = .false.
        ! Simplified implementation - would need argument list traversal
    end function variable_in_call_args

    ! Collect all constraints for a specific variable
    subroutine collect_variable_constraints(constraints, var_name, &
                                           candidate_types, confidences, count)
        type(type_constraint_t), intent(in) :: constraints(:)
        character(len=*), intent(in) :: var_name
        type(mono_type_t), allocatable, intent(out) :: candidate_types(:)
        real, allocatable, intent(out) :: confidences(:)
        integer, intent(out) :: count
        
        integer :: i, max_count
        
        max_count = size(constraints)
        allocate(candidate_types(max_count))
        allocate(confidences(max_count))
        count = 0
        
        do i = 1, size(constraints)
            if (allocated(constraints(i)%variable_name) .and. &
                constraints(i)%variable_name == var_name) then
                count = count + 1
                candidate_types(count) = constraints(i)%required_type
                confidences(count) = constraints(i)%confidence
            end if
        end do
    end subroutine collect_variable_constraints

    ! Validate variable type against its usage context
    subroutine validate_variable_type_usage(ctx, arena, var)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(undeclared_variable_t), intent(in) :: var
        
        ! Validation logic would check if inferred type is consistent
        ! with all usages of the variable in the AST
        continue
    end subroutine validate_variable_type_usage

    ! Assignment operator for usage_pattern_t
    subroutine usage_pattern_assign(lhs, rhs)
        class(usage_pattern_t), intent(inout) :: lhs
        type(usage_pattern_t), intent(in) :: rhs
        
        lhs%is_read = rhs%is_read
        lhs%is_written = rhs%is_written
        lhs%is_array_access = rhs%is_array_access
        lhs%is_passed_to_func = rhs%is_passed_to_func
        lhs%is_modified_element = rhs%is_modified_element
        lhs%first_read_location = rhs%first_read_location
        lhs%first_write_location = rhs%first_write_location
    end subroutine usage_pattern_assign

    ! Analyze parameter intent based on usage patterns
    subroutine analyze_parameter_intent(this, arena, variable_name, &
                                       scope_index, intent_result)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: variable_name
        integer, intent(in) :: scope_index
        integer, intent(out) :: intent_result
        
        type(usage_pattern_t) :: pattern
        
        ! Initialize intent result
        intent_result = INTENT_NONE
        
        ! Collect usage pattern for this variable
        call collect_variable_usage_pattern(this, arena, scope_index, &
                                           variable_name, pattern)
        
        ! Determine intent based on usage pattern
        call determine_intent_from_pattern(pattern, intent_result)
    end subroutine analyze_parameter_intent

    ! Collect detailed usage pattern for a specific variable
    subroutine collect_variable_usage_pattern(ctx, arena, scope_index, &
                                             variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(out) :: pattern
        
        ! Initialize pattern
        pattern = usage_pattern_t()
        
        ! Validate inputs
        if (scope_index <= 0 .or. scope_index > arena%size) return
        if (.not. allocated(arena%entries(scope_index)%node)) return
        if (len_trim(variable_name) == 0) return
        
        ! Process the scope node to collect usage patterns
        select type (node => arena%entries(scope_index)%node)
        type is (function_def_node)
            call analyze_function_variable_usage(ctx, arena, node, &
                                                variable_name, pattern)
        type is (subroutine_def_node)
            call analyze_subroutine_variable_usage(ctx, arena, node, &
                                                  variable_name, pattern)
        end select
    end subroutine collect_variable_usage_pattern

    ! Analyze variable usage within a function
    subroutine analyze_function_variable_usage(ctx, arena, func_node, &
                                              variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        integer :: i
        
        ! Check if variable is the result variable
        if (allocated(func_node%result_variable) .and. &
            trim(func_node%result_variable) == trim(variable_name)) then
            ! Result variables are always written-only
            pattern%is_written = .true.
            if (allocated(func_node%body_indices) .and. &
                size(func_node%body_indices) > 0) then
                pattern%first_write_location = func_node%body_indices(1)
            end if
            return
        end if
        
        ! Process function body for variable usage
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                if (func_node%body_indices(i) > 0) then
                    call traverse_node_for_variable_usage(ctx, arena, &
                                                         func_node%body_indices(i), &
                                                         variable_name, pattern)
                end if
            end do
        end if
        
        ! Process parameters to check if this variable is a parameter
        if (allocated(func_node%param_indices)) then
            do i = 1, size(func_node%param_indices)
                if (func_node%param_indices(i) > 0) then
                    call check_parameter_usage(ctx, arena, &
                                              func_node%param_indices(i), &
                                              variable_name, pattern)
                end if
            end do
        end if
    end subroutine analyze_function_variable_usage

    ! Analyze variable usage within a subroutine
    subroutine analyze_subroutine_variable_usage(ctx, arena, sub_node, &
                                                variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        integer :: i
        
        ! Process subroutine body for variable usage
        if (allocated(sub_node%body_indices)) then
            do i = 1, size(sub_node%body_indices)
                if (sub_node%body_indices(i) > 0) then
                    call traverse_node_for_variable_usage(ctx, arena, &
                                                         sub_node%body_indices(i), &
                                                         variable_name, pattern)
                end if
            end do
        end if
        
        ! Process parameters to check if this variable is a parameter
        if (allocated(sub_node%param_indices)) then
            do i = 1, size(sub_node%param_indices)
                if (sub_node%param_indices(i) > 0) then
                    call check_parameter_usage(ctx, arena, &
                                              sub_node%param_indices(i), &
                                              variable_name, pattern)
                end if
            end do
        end if
    end subroutine analyze_subroutine_variable_usage

    ! Traverse AST node recursively to find variable usage patterns
    recursive subroutine traverse_node_for_variable_usage(ctx, arena, &
                                                         node_index, &
                                                         variable_name, &
                                                         pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        integer :: i
        integer, parameter :: MAX_DEPTH = 100
        integer, save :: current_depth = 0
        
        ! Depth protection against infinite recursion
        current_depth = current_depth + 1
        if (current_depth > MAX_DEPTH) then
            current_depth = current_depth - 1
            return
        end if
        
        ! Bounds checking
        if (node_index <= 0 .or. node_index > arena%size) then
            current_depth = current_depth - 1
            return
        end if
        if (.not. allocated(arena%entries(node_index)%node)) then
            current_depth = current_depth - 1
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name) .and. &
                trim(node%name) == trim(variable_name)) then
                ! Found variable usage - determine read/write context
                call determine_variable_usage_context(ctx, arena, node_index, &
                                                     pattern)
            end if
            
        type is (assignment_node)
            ! Check LHS for write access
            if (node%target_index > 0) then
                call check_assignment_target(ctx, arena, node%target_index, &
                                            variable_name, pattern)
            end if
            
            ! Check RHS for read access
            if (node%value_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%value_index, &
                                                     variable_name, pattern)
            end if
            
        type is (call_or_subscript_node)
            ! Check function name
            if (allocated(node%name) .and. &
                trim(node%name) == trim(variable_name)) then
                ! Variable used as function name
                if (.not. pattern%is_read) then
                    pattern%is_read = .true.
                    pattern%first_read_location = node_index
                end if
            end if
            
            ! Check arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) > 0) then
                        call check_function_argument_usage(ctx, arena, &
                                                          node%arg_indices(i), &
                                                          variable_name, &
                                                          pattern)
                        call traverse_node_for_variable_usage(ctx, arena, &
                                                             node%arg_indices(i), &
                                                             variable_name, &
                                                             pattern)
                    end if
                end do
            end if
            
        type is (binary_op_node)
            ! Check both operands for read access
            if (node%left_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%left_index, &
                                                     variable_name, pattern)
            end if
            if (node%right_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%right_index, &
                                                     variable_name, pattern)
            end if
            
        type is (if_node)
            ! Check condition
            if (node%condition_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%condition_index, &
                                                     variable_name, pattern)
            end if
            
            ! Check then block
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    if (node%then_body_indices(i) > 0) then
                        call traverse_node_for_variable_usage(ctx, arena, &
                                                             node%then_body_indices(i), &
                                                             variable_name, &
                                                             pattern)
                    end if
                end do
            end if
            
            ! Check else block
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    if (node%else_body_indices(i) > 0) then
                        call traverse_node_for_variable_usage(ctx, arena, &
                                                             node%else_body_indices(i), &
                                                             variable_name, &
                                                             pattern)
                    end if
                end do
            end if
            
        type is (do_loop_node)
            ! Check loop variable - it's stored as var_name (string)
            ! For loop variable, we handle it specially in assignment detection
            
            ! Check start, end, step expressions
            if (node%start_expr_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%start_expr_index, &
                                                     variable_name, pattern)
            end if
            if (node%end_expr_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%end_expr_index, &
                                                     variable_name, pattern)
            end if
            if (node%step_expr_index > 0) then
                call traverse_node_for_variable_usage(ctx, arena, &
                                                     node%step_expr_index, &
                                                     variable_name, pattern)
            end if
            
            ! Check if this variable is the loop variable
            if (allocated(node%var_name) .and. &
                trim(node%var_name) == trim(variable_name)) then
                ! Loop variable is written (loop counter assignment)
                if (.not. pattern%is_written) then
                    pattern%is_written = .true.
                    pattern%first_write_location = node_index
                end if
            end if
            
            ! Check loop body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call traverse_node_for_variable_usage(ctx, arena, &
                                                             node%body_indices(i), &
                                                             variable_name, &
                                                             pattern)
                    end if
                end do
            end if
        end select
        
        ! Decrement depth on exit
        current_depth = current_depth - 1
    end subroutine traverse_node_for_variable_usage

    ! Determine context of variable usage (read vs write)
    subroutine determine_variable_usage_context(ctx, arena, node_index, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(usage_pattern_t), intent(inout) :: pattern
        
        ! For now, assume read access unless proven otherwise
        ! This will be refined by assignment target analysis
        if (.not. pattern%is_read) then
            pattern%is_read = .true.
            pattern%first_read_location = node_index
        end if
    end subroutine determine_variable_usage_context

    ! Check if variable is assignment target (write access)
    subroutine check_assignment_target(ctx, arena, target_index, &
                                      variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        ! Bounds checking
        if (target_index <= 0 .or. target_index > arena%size) return
        if (.not. allocated(arena%entries(target_index)%node)) return
        
        select type (node => arena%entries(target_index)%node)
        type is (identifier_node)
            if (allocated(node%name) .and. &
                trim(node%name) == trim(variable_name)) then
                ! Variable is assignment target - mark as written
                if (.not. pattern%is_written) then
                    pattern%is_written = .true.
                    pattern%first_write_location = target_index
                end if
            end if
            
        type is (call_or_subscript_node)
            ! Check if this is array element assignment
            if (allocated(node%name) .and. &
                trim(node%name) == trim(variable_name)) then
                ! Array element assignment
                pattern%is_array_access = .true.
                pattern%is_modified_element = .true.
                if (.not. pattern%is_written) then
                    pattern%is_written = .true.
                    pattern%first_write_location = target_index
                end if
            end if
        end select
    end subroutine check_assignment_target

    ! Check function argument usage patterns
    subroutine check_function_argument_usage(ctx, arena, arg_index, &
                                            variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_index
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        ! Bounds checking
        if (arg_index <= 0 .or. arg_index > arena%size) return
        if (.not. allocated(arena%entries(arg_index)%node)) return
        
        select type (node => arena%entries(arg_index)%node)
        type is (identifier_node)
            if (allocated(node%name) .and. &
                trim(node%name) == trim(variable_name)) then
                ! Variable passed as function argument
                pattern%is_passed_to_func = .true.
            end if
        end select
    end subroutine check_function_argument_usage

    ! Check parameter usage patterns
    subroutine check_parameter_usage(ctx, arena, param_index, &
                                    variable_name, pattern)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=*), intent(in) :: variable_name
        type(usage_pattern_t), intent(inout) :: pattern
        
        ! This procedure checks if a variable is a parameter declaration
        ! The actual usage analysis is done in the traversal functions
        
        ! Bounds checking
        if (param_index <= 0 .or. param_index > arena%size) return
        if (.not. allocated(arena%entries(param_index)%node)) return
        
        ! For parameter declarations, we don't need to do anything special here
        ! The usage patterns are collected during body traversal
    end subroutine check_parameter_usage

    ! Determine intent from usage pattern
    subroutine determine_intent_from_pattern(pattern, intent_result)
        type(usage_pattern_t), intent(in) :: pattern
        integer, intent(out) :: intent_result
        
        ! Apply intent inference rules
        if (.not. pattern%is_read .and. .not. pattern%is_written) then
            ! Unused parameter
            intent_result = INTENT_NONE
        else if (.not. pattern%is_read .and. pattern%is_written) then
            ! Only written, never read
            intent_result = INTENT_OUT
        else if (pattern%is_read .and. .not. pattern%is_written) then
            ! Only read, never written
            intent_result = INTENT_IN
        else if (pattern%is_read .and. pattern%is_written) then
            ! Both read and written - check temporal order
            if (pattern%first_read_location > 0 .and. &
                pattern%first_write_location > 0) then
                if (pattern%first_read_location < pattern%first_write_location) then
                    ! Read before write
                    intent_result = INTENT_INOUT
                else
                    ! Write before read - could be OUT or INOUT
                    ! Default to INOUT for safety
                    intent_result = INTENT_INOUT
                end if
            else
                ! Both read and written but temporal order unclear
                intent_result = INTENT_INOUT
            end if
        else
            ! Default case
            intent_result = INTENT_NONE
        end if
        
        ! Special handling for array modifications
        if (pattern%is_modified_element) then
            ! Array element modification implies INOUT
            intent_result = INTENT_INOUT
        end if
    end subroutine determine_intent_from_pattern

    ! Assignment operator for declaration_t
    subroutine declaration_assign(this, other)
        class(declaration_t), intent(out) :: this
        type(declaration_t), intent(in) :: other

        if (allocated(other%type_spec)) this%type_spec = other%type_spec
        if (allocated(other%intent_attr)) this%intent_attr = other%intent_attr
        if (allocated(other%variable_name)) this%variable_name = other%variable_name
        this%is_array = other%is_array
        if (allocated(other%array_spec)) this%array_spec = other%array_spec
    end subroutine declaration_assign

    ! Generate variable declarations from undeclared variables with inferred types
    subroutine generate_variable_declarations(this, arena, scope_index, &
                                            undeclared_vars, declarations)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(undeclared_variable_t), intent(in) :: undeclared_vars(:)
        type(declaration_t), allocatable, intent(out) :: declarations(:)
        
        integer :: i, decl_count, existing_count
        type(declaration_t) :: temp_decl
        character(len=:), allocatable :: existing_names(:)
        logical :: already_declared
        
        ! First pass: count variables that need declarations
        decl_count = 0
        
        ! Get already declared variables in this scope
        call get_existing_declarations(this, arena, scope_index, existing_names)
        existing_count = 0
        if (allocated(existing_names)) existing_count = size(existing_names)
        
        do i = 1, size(undeclared_vars)
            ! Skip if already declared
            already_declared = .false.
            if (existing_count > 0) then
                already_declared = any(existing_names == undeclared_vars(i)%name)
            end if
            
            if (.not. already_declared) then
                decl_count = decl_count + 1
            end if
        end do
        
        ! Allocate declarations array
        allocate(declarations(decl_count))
        
        ! Second pass: generate declarations
        decl_count = 0
        do i = 1, size(undeclared_vars)
            ! Skip if already declared
            already_declared = .false.
            if (existing_count > 0) then
                already_declared = any(existing_names == undeclared_vars(i)%name)
            end if
            
            if (.not. already_declared) then
                decl_count = decl_count + 1
                call create_declaration_from_variable(undeclared_vars(i), temp_decl)
                declarations(decl_count) = temp_decl
            end if
        end do
    end subroutine generate_variable_declarations

    ! Helper: Get existing declaration names in scope
    subroutine get_existing_declarations(ctx, arena, scope_index, existing_names)
        type(semantic_context_t), intent(in) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=:), allocatable, intent(out) :: existing_names(:)
        
        type(scope_t) :: scope
        integer :: i, name_count
        character(len=256), allocatable :: temp_names(:)
        
        ! Get the scope
        if (scope_index > 0 .and. scope_index <= ctx%scopes%depth) then
            scope = ctx%scopes%scopes(scope_index)
            
            ! Count declared variables from type environment
            name_count = 0
            do i = 1, scope%env%count
                if (allocated(scope%env%names) .and. i <= size(scope%env%names)) then
                    if (len_trim(scope%env%names(i)) > 0) then
                        name_count = name_count + 1
                    end if
                end if
            end do
            
            ! Collect names
            if (name_count > 0) then
                allocate(character(len=256) :: temp_names(name_count))
                name_count = 0
                do i = 1, scope%env%count
                    if (allocated(scope%env%names) .and. i <= size(scope%env%names)) then
                        if (len_trim(scope%env%names(i)) > 0) then
                            name_count = name_count + 1
                            temp_names(name_count) = trim(scope%env%names(i))
                        end if
                    end if
                end do
                
                ! Convert to allocatable string array
                allocate(character(len=256) :: existing_names(name_count))
                do i = 1, name_count
                    existing_names(i) = temp_names(i)
                end do
            end if
        end if
    end subroutine get_existing_declarations

    ! Helper: Create declaration from undeclared variable
    subroutine create_declaration_from_variable(var, declaration)
        type(undeclared_variable_t), intent(in) :: var
        type(declaration_t), intent(out) :: declaration
        
        ! Set variable name
        declaration%variable_name = var%name
        
        ! Generate type specification from inferred type
        call mono_type_to_fortran_spec(var%inferred_type, declaration%type_spec, &
                                      declaration%is_array, declaration%array_spec)
        
        ! Set intent attribute
        call intent_to_fortran_attr(var%inferred_intent, var%usage_type, &
                                   declaration%intent_attr)
    end subroutine create_declaration_from_variable

    ! Helper: Convert mono_type_t to Fortran type specification
    recursive subroutine mono_type_to_fortran_spec(mono_type, type_spec, is_array, array_spec)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable, intent(out) :: type_spec
        logical, intent(out) :: is_array
        character(len=:), allocatable, intent(out) :: array_spec
        
        is_array = .false.
        
        select case (mono_type%kind)
        case (TINT)
            type_spec = "integer"
        case (TREAL)
            type_spec = "real(dp)"  ! Use double precision by default
        case (TCHAR)
            type_spec = "character(len=*)"
        case (TLOGICAL)
            type_spec = "logical"
        case (TARRAY)
            is_array = .true.
            if (allocated(mono_type%args) .and. size(mono_type%args) > 0) then
                call mono_type_to_fortran_spec(mono_type%args(1), type_spec, &
                                              is_array, array_spec)
                is_array = .true.  ! Override for array
                ! Use size field for array dimensionality
                if (mono_type%size > 0) then
                    call generate_array_spec(mono_type%size, array_spec)
                else
                    array_spec = "(:)"  ! Default assumed-shape
                end if
            else
                type_spec = "real(dp)"  ! Default element type
                array_spec = "(:)"
            end if
        case default
            type_spec = "real(dp)"  ! Default fallback
        end select
    end subroutine mono_type_to_fortran_spec

    ! Helper: Generate array specification from dimensions
    subroutine generate_array_spec(ndims, array_spec)
        integer, intent(in) :: ndims
        character(len=:), allocatable, intent(out) :: array_spec
        integer :: i
        
        array_spec = "("
        do i = 1, ndims
            if (i > 1) array_spec = array_spec // ","
            array_spec = array_spec // ":"
        end do
        array_spec = array_spec // ")"
    end subroutine generate_array_spec

    ! Helper: Convert intent to Fortran attribute
    subroutine intent_to_fortran_attr(intent_type, usage_type, intent_attr)
        integer, intent(in) :: intent_type, usage_type
        character(len=:), allocatable, intent(out) :: intent_attr
        
        ! Function result variables have no intent
        if (usage_type == USAGE_RESULT_VAR) then
            intent_attr = ""
            return
        end if
        
        select case (intent_type)
        case (INTENT_IN)
            intent_attr = "intent(in)"
        case (INTENT_OUT)
            intent_attr = "intent(out)"
        case (INTENT_INOUT)
            intent_attr = "intent(inout)"
        case default
            intent_attr = ""  ! No intent for local variables
        end select
    end subroutine intent_to_fortran_attr

end module semantic_analyzer
