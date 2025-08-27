module standardizer_allocatable
    ! Allocatable marking logic module
    ! Handles array reassignment detection and string length change tracking
    
    use ast_core
    use type_system_unified
    use standardizer_types
    implicit none
    private

    public :: mark_allocatable_for_array_reassignments
    public :: mark_allocatable_for_string_length_changes
    public :: count_variable_assignments
    public :: mark_declarations_allocatable
    public :: handle_multi_variable_declaration_allocatable
    public :: split_multi_variable_declaration
    public :: update_program_body_indices
    public :: is_array_assignment
    public :: is_procedure_parameter
    public :: collect_string_vars_needing_allocatable

contains

    ! Mark variables that need allocatable due to array reassignment patterns (Issue 188)
    subroutine mark_allocatable_for_array_reassignments(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        character(len=64), allocatable :: assigned_vars(:)
        integer, allocatable :: assignment_counts(:)
        integer :: var_count, i, j
        
        ! Skip if program has no body
        if (.not. allocated(prog%body_indices)) return
        
        allocate(assigned_vars(100))
        allocate(assignment_counts(100))
        var_count = 0
        
        ! First pass: count assignments to each variable
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    call count_variable_assignments(arena, prog%body_indices(i), &
                                                   assigned_vars, assignment_counts, var_count)
                end if
            end if
        end do
        
        ! Second pass: mark declarations for variables with multiple array assignments
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    call mark_declarations_allocatable(arena, prog%body_indices(i), &
                                                      assigned_vars, assignment_counts, &
                                                      var_count, prog_index)
                end if
            end if
        end do
        
        deallocate(assigned_vars)
        deallocate(assignment_counts)
    end subroutine mark_allocatable_for_array_reassignments
    
    ! Count assignments to variables (helper for array reassignment detection)
    recursive subroutine count_variable_assignments(arena, stmt_index, assigned_vars, &
                                                    assignment_counts, var_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: assigned_vars(:)
        integer, intent(inout) :: assignment_counts(:)
        integer, intent(inout) :: var_count
        integer :: i, var_idx
        
        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return
        
        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            ! Check if this is an array assignment
            if (stmt%target_index > 0 .and. stmt%target_index <= arena%size) then
                if (allocated(arena%entries(stmt%target_index)%node)) then
                    select type (target => arena%entries(stmt%target_index)%node)
                    type is (identifier_node)
                        ! Check if value is an array expression
                        if (is_array_assignment(arena, stmt%value_index)) then
                            ! Find or add variable to tracking
                            var_idx = 0
                            do i = 1, var_count
                                if (trim(assigned_vars(i)) == trim(target%name)) then
                                    var_idx = i
                                    exit
                                end if
                            end do
                            
                            if (var_idx == 0) then
                                ! New variable
                                if (var_count < size(assigned_vars)) then
                                    var_count = var_count + 1
                                    assigned_vars(var_count) = target%name
                                    assignment_counts(var_count) = 1
                                end if
                            else
                                ! Increment count for existing variable
                                assignment_counts(var_idx) = assignment_counts(var_idx) + 1
                            end if
                        end if
                    end select
                end if
            end if
        type is (do_loop_node)
            ! Recursively count in loop body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call count_variable_assignments(arena, stmt%body_indices(i), &
                                                   assigned_vars, assignment_counts, var_count)
                end do
            end if
        type is (if_node)
            ! Recursively count in if branches
            if (allocated(stmt%then_body_indices)) then
                do i = 1, size(stmt%then_body_indices)
                    call count_variable_assignments(arena, stmt%then_body_indices(i), &
                                                   assigned_vars, assignment_counts, var_count)
                end do
            end if
            if (allocated(stmt%else_body_indices)) then
                do i = 1, size(stmt%else_body_indices)
                    call count_variable_assignments(arena, stmt%else_body_indices(i), &
                                                   assigned_vars, assignment_counts, var_count)
                end do
            end if
        end select
    end subroutine count_variable_assignments
    
    ! Mark declarations as allocatable for variables with multiple array assignments
    recursive subroutine mark_declarations_allocatable(arena, stmt_index, assigned_vars, &
                                                      assignment_counts, var_count, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        integer :: i, var_idx
        logical :: needs_split
        
        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return
        
        select type (stmt => arena%entries(stmt_index)%node)
        type is (declaration_node)
            ! Check if this is a multi-variable declaration that needs allocatable
            if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                call handle_multi_variable_declaration_allocatable(arena, stmt_index, &
                    assigned_vars, assignment_counts, var_count, prog_index, needs_split)
                
                if (needs_split) then
                    call split_multi_variable_declaration(arena, stmt_index, &
                        assigned_vars, assignment_counts, var_count, prog_index)
                end if
            else
                ! Single variable declaration
                do i = 1, var_count
                    if (trim(assigned_vars(i)) == trim(stmt%var_name)) then
                        ! Only mark allocatable if multiple assignments or if it's a procedure parameter
                        if (assignment_counts(i) > 1 .or. is_procedure_parameter(arena, stmt_index)) then
                            stmt%is_allocatable = .true.
                            if (stmt%is_array .and. allocated(stmt%dimension_indices)) then
                                ! Change fixed dimensions to deferred shape
                                deallocate(stmt%dimension_indices)
                                allocate(stmt%dimension_indices(1))
                                stmt%dimension_indices(1) = 0  ! Deferred shape
                            end if
                            ! Update the arena
                            arena%entries(stmt_index)%node = stmt
                        end if
                        exit
                    end if
                end do
            end if
        type is (do_loop_node)
            ! Recursively process loop body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call mark_declarations_allocatable(arena, stmt%body_indices(i), &
                        assigned_vars, assignment_counts, var_count, prog_index)
                end do
            end if
        type is (if_node)
            ! Recursively process if branches
            if (allocated(stmt%then_body_indices)) then
                do i = 1, size(stmt%then_body_indices)
                    call mark_declarations_allocatable(arena, stmt%then_body_indices(i), &
                        assigned_vars, assignment_counts, var_count, prog_index)
                end do
            end if
            if (allocated(stmt%else_body_indices)) then
                do i = 1, size(stmt%else_body_indices)
                    call mark_declarations_allocatable(arena, stmt%else_body_indices(i), &
                        assigned_vars, assignment_counts, var_count, prog_index)
                end do
            end if
        end select
    end subroutine mark_declarations_allocatable
    
    ! Handle multi-variable declarations for allocatable marking
    subroutine handle_multi_variable_declaration_allocatable(arena, decl_index, &
        assigned_vars, assignment_counts, var_count, prog_index, needs_split)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        logical, intent(out) :: needs_split
        integer :: i, j
        logical :: found_allocatable, found_non_allocatable
        
        needs_split = .false.
        found_allocatable = .false.
        found_non_allocatable = .false.
        
        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return
        
        select type (decl => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                ! Check each variable in the multi-declaration
                do i = 1, size(decl%var_names)
                    do j = 1, var_count
                        if (trim(assigned_vars(j)) == trim(decl%var_names(i))) then
                            if (assignment_counts(j) > 1 .or. is_procedure_parameter(arena, decl_index)) then
                                found_allocatable = .true.
                            else
                                found_non_allocatable = .true.
                            end if
                            exit
                        end if
                    end do
                    ! If we didn't find it in assigned vars, it doesn't need allocatable
                    if (j > var_count) then
                        found_non_allocatable = .true.
                    end if
                end do
                
                ! If some variables need allocatable and others don't, we need to split
                needs_split = (found_allocatable .and. found_non_allocatable)
            end if
        end select
    end subroutine handle_multi_variable_declaration_allocatable
    
    ! Split multi-variable declaration when some variables need allocatable
    subroutine split_multi_variable_declaration(arena, decl_index, &
        assigned_vars, assignment_counts, var_count, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        integer :: i, j
        type(declaration_node) :: allocatable_decl, non_allocatable_decl
        character(len=64), allocatable :: alloc_vars(:), non_alloc_vars(:)
        integer :: alloc_count, non_alloc_count
        logical :: needs_allocatable
        
        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return
        
        select type (decl => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. (decl%is_multi_declaration .and. allocated(decl%var_names))) return
            
            ! Allocate working arrays
            allocate(alloc_vars(size(decl%var_names)))
            allocate(non_alloc_vars(size(decl%var_names)))
            alloc_count = 0
            non_alloc_count = 0
            
            ! Categorize variables
            do i = 1, size(decl%var_names)
                needs_allocatable = .false.
                do j = 1, var_count
                    if (trim(assigned_vars(j)) == trim(decl%var_names(i))) then
                        if (assignment_counts(j) > 1 .or. is_procedure_parameter(arena, decl_index)) then
                            needs_allocatable = .true.
                        end if
                        exit
                    end if
                end do
                
                if (needs_allocatable) then
                    alloc_count = alloc_count + 1
                    alloc_vars(alloc_count) = decl%var_names(i)
                else
                    non_alloc_count = non_alloc_count + 1
                    non_alloc_vars(non_alloc_count) = decl%var_names(i)
                end if
            end do
            
            ! Create new declaration nodes
            if (alloc_count > 0) then
                call create_split_declaration(decl, alloc_vars, alloc_count, .true., allocatable_decl)
                call arena%push(allocatable_decl, "declaration", prog_index)
            end if
            
            if (non_alloc_count > 0) then
                call create_split_declaration(decl, non_alloc_vars, non_alloc_count, .false., non_allocatable_decl)
                call arena%push(non_allocatable_decl, "declaration", prog_index)
            end if
            
            ! Update program body indices to replace the old declaration
            call update_program_body_indices(arena, prog_index, decl_index, &
                merge(arena%size-1, 0, alloc_count > 0), &
                merge(arena%size, 0, non_alloc_count > 0))
            
            deallocate(alloc_vars)
            deallocate(non_alloc_vars)
        end select
    end subroutine split_multi_variable_declaration
    
    ! Create a split declaration from original with specified variables
    subroutine create_split_declaration(orig_decl, var_names, var_count, is_allocatable, new_decl)
        type(declaration_node), intent(in) :: orig_decl
        character(len=64), intent(in) :: var_names(:)
        integer, intent(in) :: var_count
        logical, intent(in) :: is_allocatable
        type(declaration_node), intent(out) :: new_decl
        
        ! Copy base properties
        new_decl = orig_decl
        
        ! Set variable-specific properties
        if (var_count == 1) then
            new_decl%is_multi_declaration = .false.
            new_decl%var_name = var_names(1)
            if (allocated(new_decl%var_names)) deallocate(new_decl%var_names)
        else
            new_decl%is_multi_declaration = .true.
            new_decl%var_name = var_names(1)  ! First variable as primary
            if (allocated(new_decl%var_names)) deallocate(new_decl%var_names)
            allocate(character(len=64) :: new_decl%var_names(var_count))
            new_decl%var_names(1:var_count) = var_names(1:var_count)
        end if
        
        new_decl%is_allocatable = is_allocatable
        
        ! Adjust dimensions for allocatable arrays
        if (is_allocatable .and. new_decl%is_array .and. allocated(new_decl%dimension_indices)) then
            deallocate(new_decl%dimension_indices)
            allocate(new_decl%dimension_indices(1))
            new_decl%dimension_indices(1) = 0  ! Deferred shape
        end if
    end subroutine create_split_declaration
    
    ! Update program body indices to replace old declaration with new ones
    subroutine update_program_body_indices(arena, prog_index, old_decl_index, &
        new_alloc_index, new_non_alloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, old_decl_index
        integer, intent(in) :: new_alloc_index, new_non_alloc_index
        integer :: i, old_pos, new_size, j
        integer, allocatable :: new_body_indices(:)
        
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return
        
        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            
            ! Find position of old declaration
            old_pos = 0
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) == old_decl_index) then
                    old_pos = i
                    exit
                end if
            end do
            
            if (old_pos == 0) return  ! Old declaration not found
            
            ! Calculate new size
            new_size = size(prog%body_indices) - 1  ! Remove old
            if (new_alloc_index > 0) new_size = new_size + 1
            if (new_non_alloc_index > 0) new_size = new_size + 1
            
            allocate(new_body_indices(new_size))
            
            ! Copy indices before old position
            do i = 1, old_pos - 1
                new_body_indices(i) = prog%body_indices(i)
            end do
            
            j = old_pos
            ! Insert new declarations
            if (new_alloc_index > 0) then
                new_body_indices(j) = new_alloc_index
                j = j + 1
            end if
            if (new_non_alloc_index > 0) then
                new_body_indices(j) = new_non_alloc_index
                j = j + 1
            end if
            
            ! Copy remaining indices
            do i = old_pos + 1, size(prog%body_indices)
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
            
            prog%body_indices = new_body_indices
            arena%entries(prog_index)%node = prog
        end select
    end subroutine update_program_body_indices
    
    ! Check if assignment value is an array expression
    function is_array_assignment(arena, value_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: value_index
        logical :: is_array
        
        is_array = .false.
        
        if (value_index <= 0 .or. value_index > arena%size) return
        if (.not. allocated(arena%entries(value_index)%node)) return
        
        select type (value => arena%entries(value_index)%node)
        type is (array_literal_node)
            is_array = .true.
        type is (call_or_subscript_node)
            ! Check if this is an array slice
            is_array = has_array_slice_args(arena, value)
        end select
    end function is_array_assignment
    
    ! Check if declaration is for a procedure parameter
    function is_procedure_parameter(arena, decl_index) result(is_param)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        logical :: is_param
        integer :: parent_index
        
        is_param = .false.
        
        if (decl_index <= 0 .or. decl_index > arena%size) return
        
        parent_index = arena%entries(decl_index)%parent_index
        if (parent_index <= 0 .or. parent_index > arena%size) return
        if (.not. allocated(arena%entries(parent_index)%node)) return
        
        select type (parent => arena%entries(parent_index)%node)
        type is (function_def_node)
            is_param = .true.
        type is (subroutine_def_node)
            is_param = .true.
        end select
    end function is_procedure_parameter
    
    ! Mark variables that need allocatable due to string length changes (Issue 218)
    subroutine mark_allocatable_for_string_length_changes(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        character(len=64), allocatable :: string_vars_needing_allocatable(:)
        integer :: var_count, i, j
        
        ! First pass: collect variables that need allocatable strings
        allocate(string_vars_needing_allocatable(100))
        var_count = 0
        
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    call collect_string_vars_needing_allocatable(arena, prog%body_indices(i), &
                        string_vars_needing_allocatable, var_count)
                end if
            end do
        end if
        
        ! Second pass: mark the corresponding declarations
        if (var_count > 0 .and. allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if this declaration needs to be marked
                            do j = 1, var_count
                                if (trim(stmt%var_name) == trim(string_vars_needing_allocatable(j))) then
                                    if (stmt%inferred_type%kind > 0) then
                                        stmt%inferred_type%alloc_info%needs_allocatable_string = .true.
                                        ! Clear type_name so codegen uses inferred_type
                                        stmt%type_name = ""
                                    else
                                        ! Create an inferred_type for the declaration
                                        stmt%inferred_type%kind = TCHAR
                                        stmt%inferred_type%size = 0  ! Unknown size for allocatable
                                        stmt%inferred_type%alloc_info%needs_allocatable_string = .true.
                                        ! Clear type_name so codegen uses inferred_type
                                        stmt%type_name = ""
                                    end if
                                    exit
                                end if
                            end do
                        end select
                    end if
                end if
            end do
        end if
        
        deallocate(string_vars_needing_allocatable)
    end subroutine mark_allocatable_for_string_length_changes
    
    ! Recursively collect variables that need allocatable strings
    recursive subroutine collect_string_vars_needing_allocatable(arena, stmt_index, &
                                                               var_list, var_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_list(:)
        integer, intent(inout) :: var_count
        character(len=:), allocatable :: var_name
        integer :: i, j
        
        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return
        
        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            ! Check if assignment target has needs_allocatable_string flag
            if (stmt%target_index > 0 .and. stmt%target_index <= arena%size) then
                if (allocated(arena%entries(stmt%target_index)%node)) then
                    select type (target => arena%entries(stmt%target_index)%node)
                    type is (identifier_node)
                        if (target%inferred_type%kind > 0) then
                            if (target%inferred_type%alloc_info%needs_allocatable_string) then
                                var_name = target%name
                                ! Add to list if not already present
                                do j = 1, var_count
                                    if (trim(var_list(j)) == trim(var_name)) return  ! Already in list
                                end do
                                if (var_count < size(var_list)) then
                                    var_count = var_count + 1
                                    var_list(var_count) = var_name
                                end if
                            end if
                        end if
                    end select
                end if
            end if
        type is (do_loop_node)
            ! Recursively process loop body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call collect_string_vars_needing_allocatable(arena, stmt%body_indices(i), &
                                                                var_list, var_count)
                end do
            end if
        type is (if_node)
            ! Recursively process if branches
            if (allocated(stmt%then_body_indices)) then
                do i = 1, size(stmt%then_body_indices)
                    call collect_string_vars_needing_allocatable(arena, stmt%then_body_indices(i), &
                                                                var_list, var_count)
                end do
            end if
            if (allocated(stmt%else_body_indices)) then
                do i = 1, size(stmt%else_body_indices)
                    call collect_string_vars_needing_allocatable(arena, stmt%else_body_indices(i), &
                                                                var_list, var_count)
                end do
            end if
        end select
    end subroutine collect_string_vars_needing_allocatable

end module standardizer_allocatable