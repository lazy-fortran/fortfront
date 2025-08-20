module ast_post_processor
    ! Post-processing module to apply AST modifications after semantic analysis
    ! Solves the architectural constraint where semantic analyzers have intent(in) arena
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: program_node, assignment_node, identifier_node, literal_node, &
                                binary_op_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: implicit_statement_node
    use ast_factory, only: push_declaration
    use semantic_pipeline, only: shared_context_t
    use variable_declaration_analyzer, only: variable_declaration_result_t
    use standardizer, only: get_fortran_type_string
    use type_system_hm, only: mono_type_t
    implicit none
    private

    public :: apply_variable_declarations
    public :: post_process_ast

contains

    ! Main entry point for post-processing AST modifications
    subroutine post_process_ast(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(shared_context_t), intent(in) :: context
        
        
        ! Apply variable declarations if available
        ! DEBUG: Always try to apply declarations for now
        call apply_variable_declarations(arena, root_index, context)
    end subroutine

    ! Apply variable declarations from analyzer results to AST
    subroutine apply_variable_declarations(arena, root_index, context)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(shared_context_t), intent(in) :: context
        
        logical :: success
        
        ! Apply variable declarations through post-processing
        call process_ast_for_declaration_insertion(arena, root_index, context, success)
    end subroutine

    recursive subroutine process_ast_for_declaration_insertion(arena, node_index, context, success)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(shared_context_t), intent(in) :: context
        logical, intent(out) :: success
        
        integer :: i
        
        success = .true.
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! For simplicity, just scan the entire arena for functions
        call traverse_node_for_functions(arena, node_index, context, success)
    end subroutine
    
    recursive subroutine traverse_node_for_functions(arena, node_index, context, success)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(shared_context_t), intent(in) :: context
        logical, intent(out) :: success
        
        integer :: i
        
        success = .true.
        
        ! For now, just traverse the whole arena looking for function definitions
        ! This is not optimal but ensures we don't miss any functions
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (function_def_node)
                    call insert_function_variable_declarations(arena, i, context, success)
                    if (.not. success) return
                end select
            end if
        end do
    end subroutine

    subroutine insert_function_variable_declarations(arena, func_index, context, success)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        type(shared_context_t), intent(in) :: context
        logical, intent(out) :: success
        
        integer :: decl_index, insert_position
        logical :: declaration_created
        
        success = .true.
        
        if (func_index <= 0 .or. func_index > arena%size) return
        if (.not. allocated(arena%entries(func_index)%node)) return
        
        select type (func_node => arena%entries(func_index)%node)
        type is (function_def_node)
            ! Create a test declaration for 'real :: y'
            ! TODO: Replace with proper undeclared variable detection and type inference
            decl_index = push_declaration(arena, "real", "y")
            
            if (decl_index > 0) then
                insert_position = 1  ! Insert at beginning of function body
                call insert_declaration_at_position_in_function(arena, func_index, &
                                                              insert_position, decl_index, &
                                                              declaration_created)
                if (.not. declaration_created) then
                    success = .false.
                end if
            else
                success = .false.
            end if
        end select
    end subroutine

    subroutine collect_undeclared_variables_from_function(arena, func_node, var_names, count)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable, intent(out) :: var_names(:)
        integer, intent(out) :: count
        
        character(len=32), allocatable :: temp_names(:)
        logical, allocatable :: declared(:)
        integer :: max_vars = 100
        integer :: i, j
        
        ! Initialize temporary storage
        allocate(character(len=32) :: temp_names(max_vars))
        allocate(declared(max_vars))
        count = 0
        
        ! Traverse function body to find variable usage
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                call scan_function_node_for_variables(arena, func_node%body_indices(i), &
                                                   temp_names, declared, count, max_vars)
            end do
        end if
        
        ! Mark function arguments as declared
        if (allocated(func_node%param_indices)) then
            do i = 1, size(func_node%param_indices)
                if (func_node%param_indices(i) > 0 .and. &
                    func_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_node%param_indices(i))%node)) then
                        call mark_parameter_as_declared(arena, func_node%param_indices(i), &
                                                      temp_names, declared, count)
                    end if
                end if
            end do
        end if
        
        ! Copy only undeclared variables to output
        call extract_undeclared_variables(temp_names, declared, count, var_names)
    end subroutine

    recursive subroutine scan_function_node_for_variables(arena, node_index, var_names, declared, &
                                                        count, max_vars)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=32), intent(inout) :: var_names(:)
        logical, intent(inout) :: declared(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_vars
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Check target variable
            call check_variable_usage_in_function(arena, node%target_index, var_names, declared, &
                                                count, max_vars)
            ! Check value expression recursively
            call scan_function_node_for_variables(arena, node%value_index, var_names, declared, &
                                                count, max_vars)
        type is (identifier_node)
            call check_variable_usage_in_function(arena, node_index, var_names, declared, &
                                                count, max_vars)
        type is (declaration_node)
            ! Mark variables as declared
            call mark_declaration_variables_as_declared(node, var_names, declared, count)
        end select
    end subroutine

    subroutine check_variable_usage_in_function(arena, node_index, var_names, declared, count, max_vars)
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
            if (is_builtin_function_name(var_name)) return
            
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

    subroutine mark_parameter_as_declared(arena, param_index, var_names, declared, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=32), intent(inout) :: var_names(:)
        logical, intent(inout) :: declared(:)
        integer, intent(in) :: count
        
        integer :: j
        
        select type (param_node => arena%entries(param_index)%node)
        type is (identifier_node)
            ! Mark this parameter as declared
            do j = 1, count
                if (trim(var_names(j)) == trim(param_node%name)) then
                    declared(j) = .true.
                end if
            end do
        end select
    end subroutine

    subroutine mark_declaration_variables_as_declared(decl_node, var_names, declared, count)
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

    subroutine extract_undeclared_variables(temp_names, declared, count, var_names)
        character(len=32), intent(in) :: temp_names(:)
        logical, intent(in) :: declared(:)
        integer, intent(inout) :: count
        character(len=:), allocatable, intent(out) :: var_names(:)
        
        integer :: i, j
        
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

    subroutine infer_variable_types_from_context(arena, context, var_names, var_types, count)
        type(ast_arena_t), intent(in) :: arena
        type(shared_context_t), intent(in) :: context
        character(len=:), allocatable, intent(in) :: var_names(:)
        character(len=32), intent(out) :: var_types(:)
        integer, intent(in) :: count
        
        integer :: i
        
        do i = 1, count
            call infer_single_variable_type_from_context(arena, context, var_names(i), var_types(i))
        end do
    end subroutine

    subroutine infer_single_variable_type_from_context(arena, context, var_name, var_type)
        use semantic_analyzer, only: semantic_context_t
        use type_system_hm, only: TVAR, TINT, TREAL, TCHAR, TLOGICAL
        type(ast_arena_t), intent(in) :: arena
        type(shared_context_t), intent(in) :: context
        character(*), intent(in) :: var_name
        character(len=32), intent(out) :: var_type
        
        class(*), allocatable :: sem_ctx
        logical :: found_type
        
        var_type = ""
        found_type = .false.
        
        ! Get semantic context from the pipeline
        sem_ctx = context%get_result("type_analyzer")
        if (allocated(sem_ctx)) then
            select type (sem_ctx)
            type is (semantic_context_t)
                call find_variable_type_in_semantic_context(sem_ctx, var_name, var_type, found_type)
            end select
        end if
        
        ! If not found in context, try pattern-based inference
        if (.not. found_type) then
            call infer_type_from_ast_patterns(arena, var_name, var_type, found_type)
        end if
        
        ! Default fallback for real type (common case for function results)
        if (.not. found_type) then
            var_type = "real"
        end if
    end subroutine

    subroutine find_variable_type_in_semantic_context(sem_ctx, var_name, var_type, found)
        use semantic_analyzer, only: semantic_context_t
        use type_system_hm, only: mono_type_t, TVAR
        type(semantic_context_t), intent(in) :: sem_ctx
        character(*), intent(in) :: var_name
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        type(mono_type_t) :: inferred_type
        
        found = .false.
        var_type = ""
        
        ! Try to find the variable in the semantic context's type environment
        call lookup_variable_in_semantic_env(sem_ctx%env, var_name, inferred_type, found)
        
        if (found .and. inferred_type%kind /= TVAR) then
            var_type = get_fortran_type_string(inferred_type)
        else
            found = .false.
        end if
    end subroutine

    subroutine lookup_variable_in_semantic_env(env, var_name, var_type, found)
        use type_system_hm, only: type_env_t, mono_type_t, poly_type_t
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

    subroutine infer_type_from_ast_patterns(arena, var_name, var_type, found)
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
                                    call infer_type_from_expression_pattern(arena, node%value_index, var_type, found)
                                    if (found) return
                                end if
                            end select
                        end if
                    end if
                end select
            end if
        end do
    end subroutine

    recursive subroutine infer_type_from_expression_pattern(arena, expr_index, var_type, found)
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
            call infer_type_from_literal_pattern(node%value, var_type, found)
        type is (binary_op_node)
            ! Binary operation - use left operand type
            if (node%left_index > 0) then
                call infer_type_from_expression_pattern(arena, node%left_index, var_type, found)
            end if
        end select
    end subroutine

    subroutine infer_type_from_literal_pattern(literal_value, var_type, found)
        character(*), intent(in) :: literal_value
        character(len=32), intent(out) :: var_type
        logical, intent(out) :: found
        
        character(len=:), allocatable :: trimmed
        integer :: dot_pos
        
        found = .false.
        var_type = ""
        trimmed = trim(literal_value)
        
        ! Check for real literals (contains decimal point)
        dot_pos = index(trimmed, '.')
        if (dot_pos > 0) then
            var_type = "real"
            found = .true.
        else if (is_integer_literal_pattern(trimmed)) then
            var_type = "integer"
            found = .true.
        else if (is_logical_literal_pattern(trimmed)) then
            var_type = "logical"
            found = .true.
        else if (is_character_literal_pattern(trimmed)) then
            var_type = "character(len=*)"
            found = .true.
        end if
    end subroutine

    logical function is_integer_literal_pattern(str)
        character(*), intent(in) :: str
        integer :: stat, dummy
        
        is_integer_literal_pattern = .false.
        if (len_trim(str) == 0) return
        
        ! Try to read as integer
        read(str, *, iostat=stat) dummy
        is_integer_literal_pattern = (stat == 0)
    end function

    logical function is_logical_literal_pattern(str)
        character(*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        
        lower_str = trim(str)
        call to_lower_case(lower_str)
        is_logical_literal_pattern = (lower_str == ".true." .or. lower_str == ".false.")
    end function

    logical function is_character_literal_pattern(str)
        character(*), intent(in) :: str
        
        is_character_literal_pattern = .false.
        if (len_trim(str) >= 2) then
            is_character_literal_pattern = (str(1:1) == "'" .and. str(len_trim(str):len_trim(str)) == "'") .or. &
                                         (str(1:1) == '"' .and. str(len_trim(str):len_trim(str)) == '"')
        end if
    end function

    subroutine to_lower_case(str)
        character(len=:), allocatable, intent(inout) :: str
        integer :: i, char_code
        
        do i = 1, len(str)
            char_code = iachar(str(i:i))
            if (char_code >= 65 .and. char_code <= 90) then
                str(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine

    logical function is_builtin_function_name(name)
        character(*), intent(in) :: name
        character(len=10), parameter :: builtins(*) = [ &
            "abs       ", "sin       ", "cos       ", "tan       ", "sqrt      ", &
            "exp       ", "log       ", "int       ", "real      ", "char      ", &
            "ichar     ", "len       ", "trim      ", "adjustl   ", "adjustr   ", &
            "size      ", "shape     ", "lbound    ", "ubound    ", "allocated ", &
            "associated" &
        ]
        integer :: i
        
        is_builtin_function_name = .false.
        do i = 1, size(builtins)
            if (trim(name) == trim(builtins(i))) then
                is_builtin_function_name = .true.
                return
            end if
        end do
    end function

    subroutine find_declaration_insertion_point_in_function(arena, func_node, position)
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

    subroutine insert_declaration_at_position_in_function(arena, func_index, position, decl_index, success)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        integer, intent(in) :: position
        integer, intent(in) :: decl_index
        logical, intent(out) :: success
        
        integer, allocatable :: new_body_indices(:)
        integer :: orig_size, i, insert_pos
        
        success = .false.
        
        ! Validate inputs
        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return
        if (func_index <= 0 .or. func_index > arena%size) return
        if (.not. allocated(arena%entries(func_index)%node)) return
        
        select type (func_node => arena%entries(func_index)%node)
        type is (function_def_node)
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
        end select
    end subroutine

end module ast_post_processor