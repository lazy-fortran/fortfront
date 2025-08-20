module variable_declaration_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t, identifier_node, assignment_node, function_def_node, &
                        declaration_node, program_node, implicit_statement_node, literal_node, &
                        binary_op_node
    use ast_types, only: LITERAL_STRING
    use semantic_analyzer, only: semantic_context_t
    use semantic_pipeline, only: shared_context_t
    use ast_factory, only: push_declaration
    use error_handling, only: result_t, ERROR_ERROR, ERROR_SEMANTIC
    use type_system_hm, only: mono_type_t, TVAR, TINT, TREAL, TCHAR, TLOGICAL, &
                               type_env_t, poly_type_t
    use scope_manager, only: scope_stack_t
    use standardizer, only: get_fortran_type_string
    implicit none
    private

    public :: variable_declaration_analyzer_t, variable_declaration_result_t

    ! Result type for variable declaration analysis
    type :: variable_declaration_result_t
        type(result_t) :: analysis_result
        logical :: has_declarations_added = .false.
        integer :: declarations_count = 0
        character(len=:), allocatable :: error_details
    contains
        procedure :: assign => assign_declaration_result
        generic :: assignment(=) => assign
    end type

    ! Variable declaration analyzer - generates missing declarations
    type, extends(semantic_analyzer_t) :: variable_declaration_analyzer_t
        type(variable_declaration_result_t) :: result
        logical :: analysis_complete = .false.
        logical :: strict_mode = .false.  ! Error on uninferable types
    contains
        procedure :: analyze => analyze_variable_declarations
        procedure :: get_results => get_declaration_results
        procedure :: get_name => get_declaration_analyzer_name
        procedure :: assign => assign_declaration_analyzer
        procedure :: get_dependencies => get_declaration_dependencies
        procedure :: reset_state => reset_declaration_state
    end type

contains

    subroutine analyze_variable_declarations(this, shared_context, arena, node_index)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        logical :: success

        ! Reset state for new analysis
        call this%reset_state()

        ! Use the shared context passed from the pipeline
        select type (shared_context)
        type is (shared_context_t)
            ! Need to extract semantic context from shared context
            block
                class(*), allocatable :: sem_ctx
                sem_ctx = shared_context%get_result("type_analyzer")
                if (allocated(sem_ctx)) then
                    select type(sem_ctx)
                    type is (semantic_context_t)
                        ! Now we have the semantic context
                        ! Process the AST to generate missing variable declarations
                        call process_node_for_declarations(this, arena, sem_ctx, node_index, success)
                    class default
                        success = .false.
                    end select
                else
                    success = .false.
                end if
            end block
        type is (semantic_context_t)
            ! Process the AST to generate missing variable declarations
            ! using the shared context that already has type information
            call process_node_for_declarations(this, arena, shared_context, node_index, success)
        class default
            ! Wrong context type - fail gracefully
            this%result%analysis_result%success = .false.
            this%result%analysis_result%error_code = ERROR_SEMANTIC
            this%result%analysis_result%error_message = "Invalid context type for variable_declaration_analyzer"
            this%analysis_complete = .true.
            return
        end select

        if (success) then
            this%result%analysis_result%success = .true.
            this%result%has_declarations_added = (this%result%declarations_count > 0)
        else
            this%result%analysis_result%success = .false.
            this%result%analysis_result%error_code = ERROR_SEMANTIC
        end if

        this%analysis_complete = .true.
    end subroutine

    recursive subroutine process_node_for_declarations(this, arena, context, node_index, success)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(semantic_context_t), intent(in) :: context
        integer, intent(in) :: node_index
        logical, intent(out) :: success

        success = .true.

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            call process_program_node(this, arena, context, node, success)
        type is (function_def_node)
            ! Need mutable access to function node for AST modification
            select type (mutable_node => arena%entries(node_index)%node)
            type is (function_def_node)
                call process_function_node(this, arena, context, mutable_node, success)
            end select
        class default
            ! For other node types, just continue recursively
            continue
        end select
    end subroutine

    subroutine process_program_node(this, arena, context, prog_node, success)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(semantic_context_t), intent(in) :: context
        type(program_node), intent(in) :: prog_node
        logical, intent(out) :: success

        character(len=:), allocatable :: var_names(:)
        character(len=:), allocatable :: var_types(:)
        logical, allocatable :: can_infer(:)
        integer :: undeclared_count
        integer :: i
        logical :: child_success

        success = .true.

        ! For programs, collect undeclared variables like we do for functions
        call collect_undeclared_variables_in_program(arena, prog_node, var_names, undeclared_count)
        
        if (undeclared_count > 0) then
            ! TODO: Implement type inference for undeclared variables in program
            ! For now, just mark analysis as complete without generating declarations
            ! This preserves the current behavior while allowing the function code to work
            this%result%has_declarations_added = .false.
        end if

        ! Process all body statements recursively
        if (allocated(prog_node%body_indices)) then
            do i = 1, size(prog_node%body_indices)
                call process_node_for_declarations(this, arena, context, &
                                                 prog_node%body_indices(i), child_success)
                if (.not. child_success) success = .false.
            end do
        end if
    end subroutine

    subroutine process_function_node(this, arena, context, func_node, success)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(semantic_context_t), intent(in) :: context
        type(function_def_node), intent(inout) :: func_node
        logical, intent(out) :: success

        character(len=:), allocatable :: var_names(:)
        character(len=:), allocatable :: var_types(:)
        logical, allocatable :: can_infer(:)
        integer :: undeclared_count
        integer :: i

        success = .true.

        ! Function scope only - preserve program-level implicit behavior
        call collect_undeclared_variables(arena, func_node, var_names, undeclared_count)
        
        
        if (undeclared_count == 0) return

        ! Allocate arrays for type information
        allocate(character(len=32) :: var_types(undeclared_count))
        allocate(can_infer(undeclared_count))

        ! Try to infer types for each undeclared variable
        call infer_variable_types(arena, context, var_names, var_types, can_infer, &
                                undeclared_count)

        ! CRITICAL: Error handling for uninferable types (Issue #327)
        do i = 1, undeclared_count
            if (.not. can_infer(i)) then
                if (this%strict_mode) then
                    ! Emit error instead of defaulting to real(8)
                    call set_type_inference_error(this, var_names(i))
                    success = .false.
                    cycle
                else
                    ! In non-strict mode, provide sensible defaults for Issue #320
                    var_types(i) = "real"  ! Default to real for numeric computations
                    can_infer(i) = .true.  ! Mark as inferable with default
                end if
            end if
        end do

        ! If any type inference failed and we're in strict mode, abort
        if (.not. success) return

        
        ! Generate and insert variable declarations for successfully inferred types
        call insert_variable_declarations(this, arena, func_node, var_names, var_types, &
                                        can_infer, undeclared_count, success)
        
    end subroutine

    subroutine collect_undeclared_variables(arena, func_node, var_names, count)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable, intent(out) :: var_names(:)
        integer, intent(out) :: count

        character(len=32), allocatable :: temp_names(:)
        logical, allocatable :: declared(:)
        logical, allocatable :: is_argument(:)
        integer :: max_vars = 100  ! Initial capacity
        integer :: i, j

        ! Allocate temporary storage
        allocate(character(len=32) :: temp_names(max_vars))
        allocate(declared(max_vars))
        allocate(is_argument(max_vars))
        count = 0

        ! Traverse function body to find variable usage
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                call scan_node_for_variables(arena, func_node%body_indices(i), &
                                           temp_names, declared, count, max_vars)
            end do
        end if
        
        ! Mark function arguments as declared
        ! Function arguments are stored as param_indices pointing to identifier nodes
        if (allocated(func_node%param_indices)) then
            do i = 1, size(func_node%param_indices)
                if (func_node%param_indices(i) > 0 .and. &
                    func_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(func_node%param_indices(i))%node)
                        type is (identifier_node)
                            ! Mark this parameter as declared
                            do j = 1, count
                                if (trim(temp_names(j)) == trim(param_node%name)) then
                                    declared(j) = .true.
                                    is_argument(j) = .true.
                                end if
                            end do
                        end select
                    end if
                end if
            end do
        end if

        ! Copy only undeclared variables to output
        ! First, count actual undeclared variables
        j = 0
        do i = 1, count
            if (.not. declared(i)) then
                j = j + 1
            end if
        end do
        
        ! Now copy only undeclared variables to output
        if (j > 0) then
            allocate(character(len=32) :: var_names(j))
            j = 0
            do i = 1, count
                if (.not. declared(i)) then
                    j = j + 1
                    var_names(j) = trim(temp_names(i))
                end if
            end do
            count = j  ! Update count to reflect only undeclared variables
        else
            allocate(character(len=32) :: var_names(0))
            count = 0
        end if
    end subroutine

    ! Similar to collect_undeclared_variables but for program nodes
    subroutine collect_undeclared_variables_in_program(arena, prog_node, var_names, count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog_node
        character(len=:), allocatable, intent(out) :: var_names(:)
        integer, intent(out) :: count

        character(len=32), allocatable :: temp_names(:)
        logical, allocatable :: declared(:)
        integer :: max_vars = 100  ! Initial capacity
        integer :: i, j

        ! Allocate temporary storage
        allocate(character(len=32) :: temp_names(max_vars))
        allocate(declared(max_vars))
        count = 0

        ! Traverse program body to find variable usage
        if (allocated(prog_node%body_indices)) then
            do i = 1, size(prog_node%body_indices)
                call scan_node_for_variables(arena, prog_node%body_indices(i), &
                                           temp_names, declared, count, max_vars)
            end do
        end if
        
        ! No parameters to mark as declared for programs
        
        ! Copy undeclared variables to output
        if (count > 0) then
            j = 0
            allocate(character(len=32) :: var_names(count))
            do i = 1, count
                if (.not. declared(i)) then
                    j = j + 1
                    var_names(j) = trim(temp_names(i))
                end if
            end do
            count = j  ! Update count to reflect only undeclared variables
        else
            allocate(character(len=32) :: var_names(0))
            count = 0
        end if
    end subroutine

    recursive subroutine scan_node_for_variables(arena, node_index, var_names, declared, &
                                               count, max_vars)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=32), intent(inout) :: var_names(:)
        logical, intent(inout) :: declared(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_vars

        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Check target variable
            call check_variable_usage(arena, node%target_index, var_names, declared, &
                                    count, max_vars)
            ! Check value expression recursively
            call scan_node_for_variables(arena, node%value_index, var_names, declared, &
                                       count, max_vars)
        type is (identifier_node)
            call check_variable_usage(arena, node_index, var_names, declared, &
                                    count, max_vars)
        type is (declaration_node)
            ! Mark variables as declared
            call mark_variables_declared(node, var_names, declared, count)
        class default
            ! Continue recursively for other node types
            continue
        end select
    end subroutine

    subroutine check_variable_usage(arena, node_index, var_names, declared, count, max_vars)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=32), intent(inout) :: var_names(:)
        logical, intent(inout) :: declared(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_vars

        character(len=32) :: var_name
        integer :: i
        logical :: found

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            var_name = trim(node%name)
            
            ! Skip built-in functions and intrinsics
            if (is_builtin_name(var_name)) return

            ! Check if already in list
            found = .false.
            do i = 1, count
                if (trim(var_names(i)) == var_name) then
                    found = .true.
                    exit
                end if
            end do

            ! Add if not found and not already declared
            if (.not. found .and. count < max_vars) then
                count = count + 1
                var_names(count) = var_name
                declared(count) = .false.
            end if
        end select
    end subroutine

    logical function is_builtin_name(name)
        character(*), intent(in) :: name
        character(len=10), parameter :: builtins(*) = [ &
            "abs       ", "sin       ", "cos       ", "tan       ", "sqrt      ", &
            "exp       ", "log       ", "int       ", "real      ", "char      ", &
            "ichar     ", "len       ", "trim      ", "adjustl   ", "adjustr   ", &
            "size      ", "shape     ", "lbound    ", "ubound    ", "allocated ", &
            "associated" &
        ]
        integer :: i

        is_builtin_name = .false.
        do i = 1, size(builtins)
            if (trim(name) == trim(builtins(i))) then
                is_builtin_name = .true.
                return
            end if
        end do
    end function

    subroutine mark_variables_declared(decl_node, var_names, declared, count)
        type(declaration_node), intent(in) :: decl_node
        character(len=32), intent(inout) :: var_names(:)
        logical, intent(inout) :: declared(:)
        integer, intent(in) :: count

        integer :: i, j

        ! Mark variables in this declaration as declared
        if (decl_node%is_multi_declaration .and. allocated(decl_node%var_names)) then
            do i = 1, size(decl_node%var_names)
                do j = 1, count
                    if (trim(var_names(j)) == trim(decl_node%var_names(i))) then
                        declared(j) = .true.
                    end if
                end do
            end do
        else if (allocated(decl_node%var_name)) then
            ! Single variable declaration
            do j = 1, count
                if (trim(var_names(j)) == trim(decl_node%var_name)) then
                    declared(j) = .true.
                end if
            end do
        end if
    end subroutine

    subroutine infer_variable_types(arena, context, var_names, var_types, can_infer, count)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: context
        character(len=:), allocatable, intent(in) :: var_names(:)
        character(len=32), intent(out) :: var_types(:)
        logical, intent(out) :: can_infer(:)
        integer, intent(in) :: count

        integer :: i

        do i = 1, count
            call infer_single_variable_type(arena, context, var_names(i), &
                                          var_types(i), can_infer(i))
        end do
    end subroutine

    subroutine infer_single_variable_type(arena, context, var_name, var_type, can_infer)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: context
        character(*), intent(in) :: var_name
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: can_infer

        type(mono_type_t) :: inferred_type
        logical :: found_type
        integer :: i
        
        can_infer = .false.
        var_type = ""

        ! First, try to find the inferred type in the semantic context
        ! This integrates with the existing Hindley-Milner type system
        call find_variable_type_in_context(context, var_name, inferred_type, found_type)

        if (found_type) then
            ! Check if the type is concrete (not a type variable)
            if (inferred_type%kind /= TVAR) then
                var_type = get_fortran_type_string(inferred_type)
                can_infer = .true.
            end if
        else
            ! If not found in context, search for the variable in the AST
            ! and check if it has an inferred_type attached
            call find_variable_type_in_ast(arena, var_name, inferred_type, found_type)
            if (found_type) then
                if (inferred_type%kind /= TVAR) then
                    var_type = get_fortran_type_string(inferred_type)
                    can_infer = .true.
                end if
            else
                ! Fallback: try to infer from usage patterns
                call infer_type_from_usage_patterns(arena, var_name, var_type, found_type)
                if (found_type) then
                    can_infer = .true.
                end if
            end if
        end if
    end subroutine

    subroutine find_variable_type_in_context(context, var_name, var_type, found)
        type(semantic_context_t), intent(in) :: context
        character(*), intent(in) :: var_name
        type(mono_type_t), intent(out) :: var_type
        logical, intent(out) :: found

        ! Search through the semantic context's type environment
        ! This integrates with the existing Hindley-Milner type system
        
        found = .false.
        
        ! First, try to find the variable in the current scope's variable types
        call lookup_variable_in_env(context%env, var_name, var_type, found)
        if (found) return
        
        ! If not found in env, try the scopes stack for nested scope resolution
        call lookup_variable_in_scopes(context%scopes, var_name, var_type, found)
    end subroutine

    subroutine insert_variable_declarations(this, arena, func_node, var_names, var_types, &
                                          can_infer, count, success)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_node
        character(len=:), allocatable, intent(in) :: var_names(:)
        character(len=32), intent(in) :: var_types(:)
        logical, intent(in) :: can_infer(:)
        integer, intent(in) :: count
        logical, intent(out) :: success

        integer :: i, decl_index
        integer :: insert_position
        logical :: declaration_created

        success = .true.

        ! Find insertion point (after implicit none if present)
        call find_declaration_insertion_point(arena, func_node, insert_position)

        ! Generate declarations for variables with inferred types
        do i = 1, count
            if (can_infer(i)) then
                ! Create declaration node using AST factory
                call create_variable_declaration_node(arena, var_names(i), var_types(i), &
                                                    decl_index, declaration_created)
                
                if (declaration_created) then
                    ! Insert into function body at appropriate position
                    call insert_declaration_at_position(arena, func_node, insert_position, &
                                                       decl_index, success)
                    if (success) then
                        this%result%declarations_count = this%result%declarations_count + 1
                    else
                        success = .false.
                    end if
                else
                    success = .false.
                end if
            end if
        end do
    end subroutine

    subroutine find_declaration_insertion_point(arena, func_node, position)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(out) :: position
        
        integer :: i
        logical :: found_implicit_none
        
        ! Default to beginning of function body
        position = 1
        found_implicit_none = .false.
        
        ! Search through function body for implicit none statement
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                if (func_node%body_indices(i) > 0 .and. &
                    func_node%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_node%body_indices(i))%node)) then
                        select type (node => arena%entries(func_node%body_indices(i))%node)
                        type is (implicit_statement_node)
                            if (node%is_none) then
                                ! Found implicit none - position after it
                                position = i + 1
                                found_implicit_none = .true.
                                exit
                            end if
                        type is (literal_node)
                            ! Check for literal "implicit none" as fallback
                            if (node%value == "implicit none") then
                                position = i + 1
                                found_implicit_none = .true.
                                exit
                            end if
                        end select
                    end if
                end if
            end do
        end if
        
        ! If no implicit none found, insert at beginning
        if (.not. found_implicit_none) then
            position = 1
        end if
    end subroutine

    subroutine create_variable_declaration_node(arena, var_name, var_type, node_index, success)
        type(ast_arena_t), intent(inout) :: arena
        character(*), intent(in) :: var_name
        character(*), intent(in) :: var_type
        integer, intent(out) :: node_index
        logical, intent(out) :: success

        ! Use AST factory to create declaration node
        ! This ensures proper arena management
        node_index = push_declaration(arena, var_type, var_name)
        success = (node_index > 0)
    end subroutine

    subroutine insert_declaration_at_position(arena, func_node, position, decl_index, success)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_node
        integer, intent(in) :: position
        integer, intent(in) :: decl_index
        logical, intent(out) :: success
        
        integer, allocatable :: new_body_indices(:)
        integer :: orig_size, i, insert_pos
        
        success = .false.
        
        ! Validate inputs
        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return
        

        ! Verify that the node is actually a declaration node
        select type (decl_node => arena%entries(decl_index)%node)
        type is (declaration_node)
            ! Valid declaration node - continue
        class default
            ! Not a declaration node - error
            return
        end select
        ! Handle case where function has no body yet
        if (.not. allocated(func_node%body_indices)) then
            allocate(func_node%body_indices(1))
            func_node%body_indices(1) = decl_index
            success = .true.
            return
        end if
        
        orig_size = size(func_node%body_indices)
        
        ! Validate position (1-based indexing, can insert at end+1)
        if (position < 1 .or. position > orig_size + 1) return
        
        ! Create new array with space for one more element
        allocate(new_body_indices(orig_size + 1))
        
        ! Determine actual insertion position
        insert_pos = min(position, orig_size + 1)
        
        ! Copy elements before insertion point
        if (insert_pos > 1) then
            new_body_indices(1:insert_pos-1) = func_node%body_indices(1:insert_pos-1)
        end if
        
        ! Insert the new declaration
        new_body_indices(insert_pos) = decl_index
        
        ! Copy elements after insertion point
        if (insert_pos <= orig_size) then
            new_body_indices(insert_pos+1:orig_size+1) = func_node%body_indices(insert_pos:orig_size)
        end if
        
        ! Replace the old array with the new one
        deallocate(func_node%body_indices)
        func_node%body_indices = new_body_indices
        
        
        success = .true.
    end subroutine

    subroutine set_type_inference_error(this, var_name)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        character(*), intent(in) :: var_name

        character(len=:), allocatable :: error_msg

        error_msg = "Cannot infer type for variable '" // trim(var_name) // &
                   "'. Explicit type declaration required."

        this%result%analysis_result%success = .false.
        this%result%analysis_result%error_code = ERROR_SEMANTIC
        this%result%analysis_result%error_message = error_msg
        this%result%analysis_result%component = "variable_declaration_analyzer"
        this%result%analysis_result%context = "type_inference"
        this%result%analysis_result%suggestion = &
            "Add explicit type declaration for '" // trim(var_name) // "'"
        
        if (allocated(this%result%error_details)) then
            this%result%error_details = this%result%error_details // "; " // error_msg
        else
            this%result%error_details = error_msg
        end if
    end subroutine

    ! Helper function to lookup variable type in type environment
    subroutine lookup_variable_in_env(env, var_name, var_type, found)
        type(type_env_t), intent(in) :: env
        character(*), intent(in) :: var_name
        type(mono_type_t), intent(out) :: var_type
        logical, intent(out) :: found
        
        integer :: i
        type(poly_type_t) :: scheme
        
        found = .false.
        
        ! Search through names and schemes in the environment
        if (allocated(env%names) .and. allocated(env%schemes)) then
            do i = 1, env%count
                if (trim(env%names(i)) == trim(var_name)) then
                    scheme = env%schemes(i)
                    ! If the scheme has no quantified variables, get the mono type
                    if (.not. allocated(scheme%forall) .or. size(scheme%forall) == 0) then
                        var_type = scheme%mono
                        found = .true.
                        return
                    end if
                end if
            end do
        end if
    end subroutine

    ! Helper function to lookup variable type in nested scopes
    subroutine lookup_variable_in_scopes(scopes, var_name, var_type, found)
        type(scope_stack_t), intent(in) :: scopes
        character(*), intent(in) :: var_name
        type(mono_type_t), intent(out) :: var_type
        logical, intent(out) :: found
        
        type(poly_type_t), allocatable :: scheme
        
        found = .false.
        
        ! Use the scope stack's lookup functionality
        ! This searches through scopes from innermost to outermost
        call scopes%lookup(var_name, scheme)
        
        if (allocated(scheme)) then
            ! If the scheme has no quantified variables, get the mono type
            if (.not. allocated(scheme%forall) .or. size(scheme%forall) == 0) then
                var_type = scheme%mono
                found = .true.
            end if
        end if
    end subroutine
    
    ! Helper function to search AST for variable type information
    recursive subroutine find_variable_type_in_ast(arena, var_name, var_type, found)
        type(ast_arena_t), intent(in) :: arena
        character(*), intent(in) :: var_name
        type(mono_type_t), intent(out) :: var_type
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        
        ! Search through all nodes in the arena
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                call check_node_for_variable_type(arena, i, var_name, var_type, found)
                if (found) return
            end if
        end do
    end subroutine
    
    subroutine check_node_for_variable_type(arena, node_index, var_name, var_type, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(*), intent(in) :: var_name
        type(mono_type_t), intent(out) :: var_type
        logical, intent(out) :: found
        
        found = .false.
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (trim(node%name) == trim(var_name)) then
                if (allocated(node%inferred_type)) then
                    var_type = node%inferred_type
                    found = .true.
                end if
            end if
        type is (assignment_node)
            ! Check if the target of assignment matches
            if (node%target_index > 0 .and. node%target_index <= arena%size) then
                if (allocated(arena%entries(node%target_index)%node)) then
                    select type (target => arena%entries(node%target_index)%node)
                    type is (identifier_node)
                        if (trim(target%name) == trim(var_name)) then
                            ! Get type from assignment node or value
                            if (allocated(node%inferred_type)) then
                                var_type = node%inferred_type
                                found = .true.
                            end if
                        end if
                    end select
                end if
            end if
        end select
    end subroutine
    
    ! Simple pattern-based type inference for demonstration
    subroutine infer_type_from_usage_patterns(arena, var_name, var_type, found)
        type(ast_arena_t), intent(in) :: arena
        character(*), intent(in) :: var_name
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        integer :: i
        logical :: is_function_result
        
        found = .false.
        var_type = ""
        is_function_result = .false.
        
        ! Check if this is a function result variable
        ! In "y = 2.0 * x", if y is the function result and assigned a real expression,
        ! we can infer it should be real
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (function_def_node)
                    if (allocated(node%result_variable)) then
                        if (trim(node%result_variable) == trim(var_name)) then
                            is_function_result = .true.
                            exit
                        end if
                    end if
                end select
            end if
        end do
        
        ! Always try to infer from assignment patterns, not just for function results
        ! This allows us to infer types for all variables based on their usage
        call infer_from_assignment_patterns(arena, var_name, var_type, found)
    end subroutine
    
    subroutine infer_from_assignment_patterns(arena, var_name, var_type, found)
        type(ast_arena_t), intent(in) :: arena
        character(*), intent(in) :: var_name
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        var_type = ""
        
        ! Look for assignment patterns
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (assignment_node)
                    ! Check if target is our variable
                    if (node%target_index > 0 .and. node%target_index <= arena%size) then
                        if (allocated(arena%entries(node%target_index)%node)) then
                            select type (target => arena%entries(node%target_index)%node)
                            type is (identifier_node)
                                if (trim(target%name) == trim(var_name)) then
                                    ! Found assignment to our variable
                                    call infer_type_from_expression(arena, node%value_index, var_type, found)
                                    if (found) return
                                end if
                            end select
                        end if
                    end if
                end select
            end if
        end do
    end subroutine
    
    recursive subroutine infer_type_from_expression(arena, expr_index, var_type, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        found = .false.
        var_type = ""
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            ! Direct literal assignment
            call infer_type_from_literal(node%value, var_type, found)
        type is (binary_op_node)
            ! Binary operation - handle different operators
            if (node%operator == "//") then
                ! String concatenation - infer character type and compute length
                block
                    character(len=32) :: left_type, right_type
                    logical :: left_found, right_found
                    integer :: left_len, right_len, total_len
                    
                    ! Get types of both operands
                    if (node%left_index > 0) then
                        call infer_type_from_expression(arena, node%left_index, left_type, left_found)
                    end if
                    if (node%right_index > 0) then
                        call infer_type_from_expression(arena, node%right_index, right_type, right_found)
                    end if
                    
                    ! If both are character types, compute combined length
                    if (left_found .and. right_found) then
                        if (index(left_type, "character") > 0 .and. index(right_type, "character") > 0) then
                            ! Extract lengths from type strings if present
                            left_len = extract_character_length(arena, node%left_index)
                            right_len = extract_character_length(arena, node%right_index)
                            
                            if (left_len > 0 .and. right_len > 0) then
                                total_len = left_len + right_len
                                write(var_type, '(a,i0,a)') "character(len=", total_len, ")"
                            else
                                var_type = "character(len=:), allocatable"
                            end if
                            found = .true.
                        end if
                    end if
                end block
            else
                ! Other binary operations - use left operand type
                if (node%left_index > 0) then
                    call infer_type_from_expression(arena, node%left_index, var_type, found)
                end if
            end if
        end select
    end subroutine
    
    subroutine infer_type_from_literal(literal_value, var_type, found)
        character(*), intent(in) :: literal_value
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        character(len=:), allocatable :: trimmed
        integer :: dot_pos, str_len
        
        found = .false.
        var_type = ""
        trimmed = trim(literal_value)
        
        ! Check for string/character literals first (quoted strings)
        if (is_character_literal(trimmed)) then
            ! Calculate actual string length (excluding quotes)
            str_len = len_trim(trimmed) - 2
            if (str_len > 0) then
                write(var_type, '(a,i0,a)') "character(len=", str_len, ")"
            else
                var_type = "character(len=0)"
            end if
            found = .true.
        else if (is_logical_literal(trimmed)) then
            var_type = "logical"
            found = .true.
        else
            ! Check for real literals (contains decimal point)
            dot_pos = index(trimmed, '.')
            if (dot_pos > 0) then
                ! It's a real literal
                var_type = "real"
                found = .true.
            else if (is_integer_literal(trimmed)) then
                var_type = "integer"
                found = .true.
            end if
        end if
    end subroutine
    
    logical function is_integer_literal(str)
        character(*), intent(in) :: str
        integer :: i, stat
        integer :: dummy
        
        is_integer_literal = .false.
        if (len_trim(str) == 0) return
        
        ! Try to read as integer
        read(str, *, iostat=stat) dummy
        is_integer_literal = (stat == 0)
    end function
    
    logical function is_logical_literal(str)
        character(*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        
        lower_str = trim(str)
        call to_lower(lower_str)
        is_logical_literal = (lower_str == ".true." .or. lower_str == ".false.")
    end function
    
    logical function is_character_literal(str)
        character(*), intent(in) :: str
        
        is_character_literal = .false.
        if (len_trim(str) >= 2) then
            is_character_literal = (str(1:1) == "'" .and. str(len_trim(str):len_trim(str)) == "'") .or. &
                                  (str(1:1) == '"' .and. str(len_trim(str):len_trim(str)) == '"')
        end if
    end function
    
    ! Helper function to extract character length from an expression node
    recursive function extract_character_length(arena, expr_index) result(length)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        integer :: length
        
        length = 0
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            ! For string literals, length is string length minus 2 for quotes
            if (node%literal_kind == LITERAL_STRING) then
                length = len_trim(node%value) - 2  ! Subtract quotes
            end if
        type is (binary_op_node)
            ! For concatenation, sum the lengths
            if (node%operator == "//") then
                block
                    integer :: left_len, right_len
                    left_len = extract_character_length(arena, node%left_index)
                    right_len = extract_character_length(arena, node%right_index)
                    if (left_len > 0 .and. right_len > 0) then
                        length = left_len + right_len
                    end if
                end block
            end if
        end select
    end function
    
    subroutine to_lower(str)
        character(len=:), allocatable, intent(inout) :: str
        integer :: i, char_code
        
        do i = 1, len(str)
            char_code = iachar(str(i:i))
            if (char_code >= 65 .and. char_code <= 90) then
                str(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine

    function get_declaration_results(this) result(results)
        class(variable_declaration_analyzer_t), intent(in) :: this
        class(*), allocatable :: results

        allocate(variable_declaration_result_t :: results)
        select type(results)
        type is (variable_declaration_result_t)
            results = this%result
        end select
    end function

    function get_declaration_analyzer_name(this) result(name)
        class(variable_declaration_analyzer_t), intent(in) :: this
        character(:), allocatable :: name

        name = "variable_declaration_analyzer"
        
        ! Parameter suppressed for interface compliance
        continue
    end function

    subroutine assign_declaration_analyzer(lhs, rhs)
        class(variable_declaration_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs

        select type(rhs)
        type is (variable_declaration_analyzer_t)
            lhs%result = rhs%result
            lhs%analysis_complete = rhs%analysis_complete
            lhs%strict_mode = rhs%strict_mode
        class default
            error stop "Type mismatch in variable_declaration_analyzer assignment"
        end select
    end subroutine

    function get_declaration_dependencies(this) result(deps)
        class(variable_declaration_analyzer_t), intent(in) :: this
        character(len=32), allocatable :: deps(:)

        ! Depends on type analysis to complete first
        allocate(deps(1))
        deps(1) = "type_analyzer"
        
        ! Parameter suppressed for interface compliance
        continue
    end function

    subroutine reset_declaration_state(this)
        class(variable_declaration_analyzer_t), intent(inout) :: this

        this%result%analysis_result%success = .true.
        this%result%analysis_result%error_code = 0
        if (allocated(this%result%analysis_result%error_message)) then
            deallocate(this%result%analysis_result%error_message)
        end if
        if (allocated(this%result%error_details)) then
            deallocate(this%result%error_details)
        end if
        this%result%has_declarations_added = .false.
        this%result%declarations_count = 0
        this%analysis_complete = .false.
    end subroutine

    subroutine assign_declaration_result(lhs, rhs)
        class(variable_declaration_result_t), intent(inout) :: lhs
        class(variable_declaration_result_t), intent(in) :: rhs

        lhs%analysis_result = rhs%analysis_result
        lhs%has_declarations_added = rhs%has_declarations_added
        lhs%declarations_count = rhs%declarations_count
        if (allocated(rhs%error_details)) then
            lhs%error_details = rhs%error_details
        end if
    end subroutine

end module variable_declaration_analyzer