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
    use ast_core
    use ast_nodes_bounds, only: array_spec_t, array_bounds_t, array_slice_node, &
                                range_expression_node, get_array_slice_node
    use parameter_tracker
    implicit none
    private

    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    public :: validate_array_bounds, check_shape_conformance

    ! Semantic analysis context
    type :: semantic_context_t
        type(type_env_t) :: env  ! Legacy flat environment (to be removed)
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(parameter_tracker_t) :: param_tracker  ! Track parameter attributes
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
        class default
            ! Single statement/expression
            call infer_and_store_type(ctx, arena, root_index)
        end select
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

                    if (.not. is_assignable(typ, target_type)) then
                        ! Type error - for now, just continue with inference
                        ! In a full implementation, we would report an error
                        ! error stop type_error(target_type, typ, &
                        !              "assignment to " // var_name)
                    end if

                    ! Use the existing type for consistency
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
            ! Not found - create fresh type variable (TEMPORARY: should be error)
            ! TODO: Re-enable after fixing parser multi-variable declaration bug
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
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

        ! Infer left operand type
        left_typ = ctx%infer(arena, binop%left_index)

        ! Infer right operand type
        right_typ = ctx%infer(arena, binop%right_index)

        ! Determine result type based on operator
        select case (trim(binop%operator))
        case (":")
            ! Array range operator - for now, return integer type
            ! TODO: This should return a proper range/slice type
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
            ! For now, just check both are character types
            ! The result will have combined length
            if (left_typ%kind == TCHAR .or. left_typ%kind == TVAR) then
                ! Left is char or will be inferred as char
            else
                s1 = ctx%unify(left_typ, create_mono_type(TCHAR))
                call ctx%compose_with_subst(s1)
            end if

            if (right_typ%kind == TCHAR .or. right_typ%kind == TVAR) then
                ! Right is char or will be inferred as char
            else
                s2 = ctx%unify(right_typ, create_mono_type(TCHAR))
                call ctx%compose_with_subst(s2)
            end if

            ! Result is a character type with combined length
            ! For now, use default size
            result_typ = create_mono_type(TCHAR)

        case default
            error stop "Unknown binary operator: "//trim(binop%operator)
        end select

        typ = ctx%apply_subst_to_type(result_typ)
    end function infer_binary_op

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
                    ! TODO: Handle multi-dimensional arrays and slicing properly
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

                    s = ctx%unify(result_typ, expected_fun_type)
                    call ctx%compose_with_subst(s)

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
            ! For string concatenation and other operations, we may need to
            ! unify character types of different lengths
            ! For now, just accept any character types
            ! TODO: Properly handle character length in type system

        case (TFUN)
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                error stop "Invalid function types"
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

            ! Min/max - variadic functions that take 2 or more arguments
        case ("min", "max")
            ! For now, create a type that accepts multiple arguments
            ! This is a simplification - proper variadic support would be better
            typ = create_fun_type(real_type, real_type)

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
                    block
                        type(mono_type_t) :: decl_type
                        decl_type = ctx%infer(arena, mod_node%declaration_indices(i))
                    end block
                end if
            end do
        end if

        ! Analyze module procedures
        if (allocated(mod_node%procedure_indices)) then
            do i = 1, size(mod_node%procedure_indices)
                if (mod_node%procedure_indices(i) > 0) then
                    block
                        type(mono_type_t) :: proc_type
                        proc_type = ctx%infer(arena, mod_node%procedure_indices(i))
                    end block
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
            typ = create_mono_type(TREAL)  ! TODO: track precision
        case ('character')
            typ = create_mono_type(TCHAR, char_size=1)  ! TODO: handle length
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

        ! Enter function scope
        call ctx%scopes%enter_function(func_def%name)

        ! Clear parameter tracker for new function
        call ctx%param_tracker%clear()

        ! Process parameters and add to local scope
        if (allocated(func_def%param_indices)) then
            allocate (param_types(size(func_def%param_indices)))
            do i = 1, size(func_def%param_indices)
                ! For now, assign fresh type variables to parameters
                param_types(i) = create_mono_type(TVAR, var=ctx%fresh_type_var())

                ! Add parameter to local scope - get from arena
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        call ctx%scopes%define(param%name, &
                      create_poly_type(forall_vars=[type_var_t::], mono=param_types(i)))
                    type is (parameter_declaration_node)
                        ! Track parameter with intent
                        call ctx%param_tracker%add_parameter(param%name, param%intent)
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
                return_type = create_mono_type(TREAL)  ! TODO: track precision
            case ('character')
                return_type = create_mono_type(TCHAR, char_size=1)
            case default
                ! Unknown type - use type variable
                return_type = create_mono_type(TVAR, var=ctx%fresh_type_var())
            end select
        else
            ! Infer return type (use a fresh type variable)
            return_type = create_mono_type(TVAR, var=ctx%fresh_type_var())
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
                        call ctx%param_tracker%add_parameter(param%name, param%intent)
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
    end function semantic_context_deep_copy

    subroutine semantic_context_assign(lhs, rhs)
        class(semantic_context_t), intent(out) :: lhs
        type(semantic_context_t), intent(in) :: rhs

        lhs%env = rhs%env                ! Uses type_env_t assignment (deep copy)
        lhs%scopes = rhs%scopes          ! Uses scope_stack_t assignment (deep copy)
        lhs%next_var_id = rhs%next_var_id
        lhs%subst = rhs%subst            ! Uses substitution_t assignment (deep copy)
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
        
        ! TODO: Add actual bounds validation logic here
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

end module semantic_analyzer
