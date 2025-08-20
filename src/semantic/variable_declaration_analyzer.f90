module variable_declaration_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t, identifier_node, assignment_node, function_def_node, &
                        declaration_node, program_node
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_factory, only: push_declaration
    use error_handling, only: result_t, ERROR_ERROR, ERROR_SEMANTIC
    use type_system_hm, only: mono_type_t, TVAR, TINT, TREAL, TCHAR, TLOGICAL
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
        logical :: strict_mode = .true.  ! Error on uninferable types
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
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        type(semantic_context_t) :: local_context
        logical :: success

        ! Reset state for new analysis
        call this%reset_state()

        ! Initialize semantic context for type inference
        local_context = create_semantic_context()

        ! Create a mutable copy of arena for analysis and modification
        block
            type(ast_arena_t) :: mutable_arena
            mutable_arena = arena
            
            ! Perform semantic analysis to get type information
            call analyze_program(local_context, mutable_arena, node_index)

            ! Process the AST to generate missing variable declarations
            call process_node_for_declarations(this, mutable_arena, local_context, node_index, success)
        end block

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
            call process_function_node(this, arena, context, node, success)
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

        integer :: i
        logical :: child_success

        success = .true.

        ! Process all body statements
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
        type(function_def_node), intent(in) :: func_node
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
        integer :: max_vars = 100  ! Initial capacity
        integer :: i

        ! Allocate temporary storage
        allocate(character(len=32) :: temp_names(max_vars))
        allocate(declared(max_vars))
        count = 0

        ! Traverse function body to find variable usage
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                call scan_node_for_variables(arena, func_node%body_indices(i), &
                                           temp_names, declared, count, max_vars)
            end do
        end if

        ! Copy results to output
        if (count > 0) then
            allocate(character(len=32) :: var_names(count))
            do i = 1, count
                var_names(i) = trim(temp_names(i))
            end do
        else
            allocate(character(len=32) :: var_names(0))
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

        type(mono_type_t), pointer :: inferred_type
        
        can_infer = .false.
        var_type = ""

        ! Try to find the inferred type in the semantic context
        ! This integrates with the existing Hindley-Milner type system
        call find_variable_type_in_context(context, var_name, inferred_type)

        if (associated(inferred_type)) then
            ! Check if the type is concrete (not a type variable)
            if (inferred_type%kind /= TVAR) then
                var_type = get_fortran_type_string(inferred_type)
                can_infer = .true.
            end if
        end if
    end subroutine

    subroutine find_variable_type_in_context(context, var_name, type_ptr)
        type(semantic_context_t), intent(in) :: context
        character(*), intent(in) :: var_name
        type(mono_type_t), pointer, intent(out) :: type_ptr

        ! This is a placeholder - in the actual implementation,
        ! this would search through the semantic context's type environment
        ! and scope stack to find the inferred type for the variable
        
        ! For now, return null to indicate type not found
        type_ptr => null()
        
        ! TODO: Implement actual type lookup in semantic context
        ! This should integrate with context%env and context%scopes
        associate(dummy_var => var_name, dummy_ctx => context)
        end associate
    end subroutine

    subroutine insert_variable_declarations(this, arena, func_node, var_names, var_types, &
                                          can_infer, count, success)
        class(variable_declaration_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
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

        ! Default to beginning of function body
        position = 1

        ! TODO: Implement logic to find position after implicit none
        ! This should scan the function body for implicit none statement
        ! and set position to the index after it
        
        associate(dummy_arena => arena, dummy_func => func_node)
        end associate
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
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: position
        integer, intent(in) :: decl_index
        logical, intent(out) :: success

        ! TODO: Implement AST modification to insert declaration
        ! This requires careful manipulation of the function body indices
        
        success = .true.  ! Placeholder
        
        associate(dummy1 => arena, dummy2 => func_node, dummy3 => position, dummy4 => decl_index)
        end associate
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
        
        associate(dummy => this)
        end associate
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
        
        associate(dummy => this)
        end associate
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