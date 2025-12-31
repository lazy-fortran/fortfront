module standardizer_allocatable
    ! Allocatable marking logic module
    ! Handles array reassignment detection and string length change tracking

    use string_utils_mod, only: to_lower

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_loops
    use ast_nodes_control
    use ast_nodes_data
    use ast_nodes_procedure
    use ast_nodes_bounds, only: range_expression_node
    use type_system_unified
    use standardizer_types
    implicit none
    private

    integer, parameter :: default_stack_capacity = 64

    type :: node_stack_entry_t
        integer :: idx = 0
    end type node_stack_entry_t

    type :: node_stack_t
        type(node_stack_entry_t), allocatable :: entries(:)
        integer :: top = 0
    end type node_stack_t

    public :: mark_allocatable_for_array_reassignments
    public :: mark_allocatable_for_string_length_changes
    public :: count_variable_assignments
    public :: mark_declarations_allocatable
    public :: handle_multi_variable_declaration_allocatable
    public :: split_multi_variable_declaration
    public :: update_program_body_indices
    public :: is_array_assignment
    public :: is_procedure_parameter
    public :: is_type_component
    public :: collect_string_vars_needing_allocatable

contains

    pure logical function node_is_active(arena, idx) result(is_active)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        if (idx <= 0) then
            is_active = .false.
        else if (idx > arena%size) then
            is_active = .false.
        else
            is_active = allocated(arena%entries(idx)%node)
        end if
    end function node_is_active

    subroutine stack_init(stack, initial_capacity)
        type(node_stack_t), intent(inout) :: stack
        integer, intent(in), optional :: initial_capacity
        integer :: capacity

        capacity = default_stack_capacity
        if (present(initial_capacity)) capacity = initial_capacity

        if (allocated(stack%entries)) deallocate (stack%entries)
        allocate (stack%entries(capacity))
        stack%top = 0
    end subroutine stack_init

    logical function stack_is_empty(stack) result(is_empty)
        type(node_stack_t), intent(in) :: stack

        is_empty = (stack%top <= 0)
    end function stack_is_empty

    subroutine stack_push(stack, idx)
        type(node_stack_t), intent(inout) :: stack
        integer, intent(in) :: idx
        type(node_stack_entry_t), allocatable :: tmp(:)
        integer :: new_capacity

        if (idx <= 0) return

        if (.not. allocated(stack%entries)) then
            call stack_init(stack)
        end if

        if (stack%top >= size(stack%entries)) then
            new_capacity = max(1, size(stack%entries) * 2)
            allocate (tmp(new_capacity))
            if (stack%top > 0) tmp(1:stack%top) = stack%entries(1:stack%top)
            call move_alloc(tmp, stack%entries)
        end if

        stack%top = stack%top + 1
        stack%entries(stack%top)%idx = idx
    end subroutine stack_push

    integer function stack_pop(stack) result(idx)
        type(node_stack_t), intent(inout) :: stack

        if (stack%top <= 0) then
            idx = 0
        else
            idx = stack%entries(stack%top)%idx
            stack%top = stack%top - 1
        end if
    end function stack_pop

    subroutine stack_push_many(stack, indices)
        type(node_stack_t), intent(inout) :: stack
        integer, intent(in) :: indices(:)
        integer :: i

        do i = size(indices), 1, -1
            call stack_push(stack, indices(i))
        end do
    end subroutine stack_push_many

    integer function locate_variable(name, vars, count) result(index)
        character(len=*), intent(in) :: name
        character(len=64), intent(in) :: vars(:)
        integer, intent(in) :: count
        integer :: i

        index = 0
        do i = 1, count
            if (trim(vars(i)) == trim(name)) then
                index = i
                exit
            end if
        end do
    end function locate_variable

    pure integer function find_index(values, target) result(position)
        integer, intent(in) :: values(:)
        integer, intent(in) :: target
        integer :: i

        position = 0
        do i = 1, size(values)
            if (values(i) == target) then
                position = i
                exit
            end if
        end do
    end function find_index

    logical function variable_requires_allocatable(name, assigned_vars, &
                                                   assignment_counts, var_count, &
                                                   is_parameter, is_array) &
        result(needs_alloc)
        character(len=*), intent(in) :: name
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        logical, intent(in) :: is_parameter
        logical, intent(in), optional :: is_array
        integer :: idx

        idx = locate_variable(name, assigned_vars, var_count)
        needs_alloc = .false.
        if (idx <= 0) return

        ! Only mark as allocatable for multiple assignments if it's an array
        ! Scalars should never be marked allocatable just due to multiple assignments
        if (present(is_array) .and. is_array) then
            needs_alloc = (assignment_counts(idx) >= 2 .or. is_parameter)
        else
            ! For scalars, only mark as allocatable if it's a parameter
            needs_alloc = is_parameter
        end if
    end function variable_requires_allocatable

    subroutine record_assignment(arena, stmt, assigned_vars, assignment_counts, &
                                 var_count)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=64), intent(inout) :: assigned_vars(:)
        integer, intent(inout) :: assignment_counts(:)
        integer, intent(inout) :: var_count
        integer :: idx
        character(len=64) :: var_name

        if (.not. node_is_active(arena, stmt%target_index)) return

        var_name = ""

        ! Extract variable name from different target types
        select type (target => arena%entries(stmt%target_index)%node)
        type is (identifier_node)
            ! Simple assignment: x = value
            var_name = target%name
        type is (call_or_subscript_node)
            ! Element assignments like arr(0)=... should not trigger allocatable
            ! handling. Whole-array reassignments use identifier targets.
            var_name = ""
        class default
            ! Other assignment target types - ignore for allocatable tracking
            var_name = ""
        end select

        ! Only track assignments that could require allocatable behavior
        if (len_trim(var_name) > 0) then
            idx = locate_variable(var_name, assigned_vars, var_count)
            if (idx == 0) then
                if (var_count < size(assigned_vars)) then
                    var_count = var_count + 1
                    assigned_vars(var_count) = var_name
                    assignment_counts(var_count) = 1
                end if
            else
                assignment_counts(idx) = assignment_counts(idx) + 1
            end if
        end if
    end subroutine record_assignment

    subroutine record_string_variable(target, var_list, var_count)
        type(identifier_node), intent(in) :: target
        character(len=64), intent(inout) :: var_list(:)
        integer, intent(inout) :: var_count
        integer :: idx

        if (target%inferred_type%kind <= 0) return
        if (.not. target%inferred_type%alloc_info%needs_allocatable_string) return

        idx = locate_variable(target%name, var_list, var_count)
        if (idx > 0) return
        if (var_count >= size(var_list)) return

        var_count = var_count + 1
        var_list(var_count) = target%name
    end subroutine record_string_variable

    subroutine append_split_declaration(arena, template_decl, var_name, &
                                        needs_allocatable, prog_index, new_indices, &
                                        new_count)
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_node), intent(in) :: template_decl
        character(len=*), intent(in) :: var_name
        logical, intent(in) :: needs_allocatable
        integer, intent(in) :: prog_index
        integer, intent(inout) :: new_indices(:)
        integer, intent(inout) :: new_count
        type(declaration_node) :: single_decl
        character(len=64) :: names(1)

        names(1) = trim(var_name)
        call create_split_declaration(template_decl, names, 1, needs_allocatable, &
                                      single_decl)
        call arena%push(single_decl, "declaration", prog_index)
        new_count = new_count + 1
        new_indices(new_count) = arena%size
    end subroutine append_split_declaration

    subroutine replace_declaration_with_list(arena, prog_index, old_index, &
                                             new_indices, new_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: old_index
        integer, intent(in) :: new_indices(:)
        integer, intent(in) :: new_count
        integer :: pos, total, cursor, i
        integer, allocatable :: updated(:)

        if (.not. node_is_active(arena, prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return

            pos = 0
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) == old_index) then
                    pos = i
                    exit
                end if
            end do
            if (pos == 0) return

            total = size(prog%body_indices) - 1 + new_count
            allocate (updated(total))

            if (pos > 1) updated(1:pos - 1) = prog%body_indices(1:pos - 1)

            cursor = pos
            if (new_count > 0) then
                updated(cursor:cursor + new_count - 1) = new_indices(1:new_count)
                cursor = cursor + new_count
            end if

            do i = pos + 1, size(prog%body_indices)
                updated(cursor) = prog%body_indices(i)
                cursor = cursor + 1
            end do

            prog%body_indices = updated
            arena%entries(prog_index)%node = prog
        end select
    end subroutine replace_declaration_with_list

    ! Mark variables needing allocatable for array reassignment patterns
    ! (Issue 188)
    subroutine mark_allocatable_for_array_reassignments(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        character(len=64), allocatable :: assigned_vars(:)
        integer, allocatable :: assignment_counts(:)
        integer :: var_count, i, j
        integer, allocatable :: local_indices(:)

        ! Skip if program has no body
        if (.not. allocated(prog%body_indices)) return

        allocate (assigned_vars(100))
        allocate (assignment_counts(100))
        var_count = 0

        ! First pass: count assignments to each variable
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    call count_variable_assignments(arena, prog%body_indices(i), &
                                                    assigned_vars, &
                                                    assignment_counts, var_count)
                end if
            end if
        end do

        ! Second pass: mark declarations for variables with multiple array assignments
        ! CRITICAL: Copy indices to local array before iteration to prevent stale
        ! access. When mark_declarations_allocatable modifies the arena (via
        ! split_multi_variable_declaration), it updates the arenas version of
        ! prog%body_indices, but we have a stack copy (intent(in)).
        ! Using stale copy causes segfaults when accessing invalid/outdated indices.
        allocate (local_indices(size(prog%body_indices)))
        local_indices = prog%body_indices

        do i = 1, size(local_indices)
            if (local_indices(i) > 0 .and. local_indices(i) <= arena%size) then
                if (allocated(arena%entries(local_indices(i))%node)) then
                    call mark_declarations_allocatable(arena, local_indices(i), &
                                                       assigned_vars, &
                                                       assignment_counts, &
                                                       var_count, prog_index)
                end if
            end if
        end do

        deallocate (local_indices)
        deallocate (assigned_vars)
        deallocate (assignment_counts)
    end subroutine mark_allocatable_for_array_reassignments

    ! Count assignments to variables (helper for array reassignment detection)
    subroutine count_variable_assignments(arena, stmt_index, assigned_vars, &
                                          assignment_counts, var_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: assigned_vars(:)
        integer, intent(inout) :: assignment_counts(:)
        integer, intent(inout) :: var_count
        type(node_stack_t) :: stack
        integer :: current_index

        call stack_init(stack)
        call stack_push(stack, stmt_index)

        do while (.not. stack_is_empty(stack))
            current_index = stack_pop(stack)
            if (.not. node_is_active(arena, current_index)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (assignment_node)
                call record_assignment(arena, stmt, assigned_vars, &
                                       assignment_counts, var_count)
            type is (do_loop_node)
                if (allocated(stmt%body_indices)) then
                    call stack_push_many(stack, stmt%body_indices)
                end if
            type is (if_node)
                if (allocated(stmt%else_body_indices)) then
                    call stack_push_many(stack, stmt%else_body_indices)
                end if
                if (allocated(stmt%then_body_indices)) then
                    call stack_push_many(stack, stmt%then_body_indices)
                end if
            class default
                cycle
            end select
        end do
    end subroutine count_variable_assignments

    ! Mark declarations as allocatable for variables with multiple array assignments
    subroutine mark_declarations_allocatable(arena, stmt_index, assigned_vars, &
                                             assignment_counts, var_count, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        type(node_stack_t) :: stack
        integer :: current_index

        call stack_init(stack)
        call stack_push(stack, stmt_index)

        do while (.not. stack_is_empty(stack))
            current_index = stack_pop(stack)
            if (.not. node_is_active(arena, current_index)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (declaration_node)
                call process_declaration_allocatable(arena, current_index, stmt, &
                                                     assigned_vars, &
                                                     assignment_counts, var_count, &
                                                     prog_index)
            type is (do_loop_node)
                if (allocated(stmt%body_indices)) then
                    call stack_push_many(stack, stmt%body_indices)
                end if
            type is (if_node)
                if (allocated(stmt%else_body_indices)) then
                    call stack_push_many(stack, stmt%else_body_indices)
                end if
                if (allocated(stmt%then_body_indices)) then
                    call stack_push_many(stack, stmt%then_body_indices)
                end if
            class default
                cycle
            end select
        end do
    end subroutine mark_declarations_allocatable

    subroutine process_declaration_allocatable(arena, decl_index, stmt, &
                                               assigned_vars, assignment_counts, &
                                               var_count, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        type(declaration_node), intent(inout) :: stmt
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        logical :: needs_split
        logical :: needs_alloc
        logical :: is_parameter

        ! Skip processing derived type components - they should not be modified
        ! by allocatable reassignment logic (fixes issue #1738)
        if (is_type_component(arena, decl_index)) return

        ! Skip PARAMETER constants - allocatable conflicts with parameter
        if (stmt%is_parameter) return

        if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
            call handle_multi_variable_declaration_allocatable( &
                arena, decl_index, assigned_vars, assignment_counts, var_count, &
                prog_index, needs_split)

            if (needs_split) then
                call split_multi_variable_declaration(arena, decl_index, &
                                                      assigned_vars, &
                                                      assignment_counts, var_count, &
                                                      prog_index)
            end if
        else
            is_parameter = is_procedure_parameter(arena, decl_index)
            needs_alloc = variable_requires_allocatable(stmt%var_name, &
                                                        assigned_vars, &
                                                        assignment_counts, var_count, &
                                                        is_parameter, stmt%is_array)
            if (needs_alloc) then
                if (apply_allocatable_attributes(stmt)) then
                    arena%entries(decl_index)%node = stmt
                end if
            end if
        end if
    end subroutine process_declaration_allocatable

    ! Handle multi-variable declarations for allocatable marking
    subroutine handle_multi_variable_declaration_allocatable(arena, decl_index, &
                                                             assigned_vars, &
                                                             assignment_counts, &
                                                             var_count, prog_index, &
                                                             needs_split)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        logical, intent(out) :: needs_split
        integer :: i
        logical :: found_allocatable, found_non_allocatable, is_parameter
        integer :: idx

        needs_split = .false.
        found_allocatable = .false.; found_non_allocatable = .false.

        if (.not. node_is_active(arena, decl_index)) return

        select type (decl => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. (decl%is_multi_declaration .and. &
                       allocated(decl%var_names))) return

            is_parameter = is_procedure_parameter(arena, decl_index)
            do i = 1, size(decl%var_names)
                idx = locate_variable(decl%var_names(i), assigned_vars, var_count)
                if (idx <= 0) then
                    found_non_allocatable = .true.
                else if (assignment_counts(idx) >= 2 .or. is_parameter) then
                    found_allocatable = .true.
                else
                    found_non_allocatable = .true.
                end if
            end do

            needs_split = (found_allocatable .and. found_non_allocatable)
            if (found_allocatable .and. .not. found_non_allocatable) then
                block
                    type(declaration_node) :: tmp
                    tmp = decl
                    if (apply_allocatable_attributes(tmp)) then
                        arena%entries(decl_index)%node = tmp
                    end if
                end block
            end if
        end select
    end subroutine handle_multi_variable_declaration_allocatable

    ! Split multi-variable declaration when some variables need allocatable
    subroutine split_multi_variable_declaration(arena, decl_index, &
                                                assigned_vars, assignment_counts, &
                                                var_count, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        character(len=64), intent(in) :: assigned_vars(:)
        integer, intent(in) :: assignment_counts(:)
        integer, intent(in) :: var_count
        integer, intent(in) :: prog_index
        integer :: i
        integer, allocatable :: new_indices(:)
        integer :: new_count
        logical :: needs_allocatable
        logical :: is_parameter
        type(declaration_node) :: template_decl

        if (.not. node_is_active(arena, decl_index)) return

        select type (decl_orig => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. (decl_orig%is_multi_declaration .and. &
                       allocated(decl_orig%var_names))) return

     ! CRITICAL: Copy declaration to stack before arena pushes to avoid dangling pointer
            ! When append_split_declaration pushes to arena, it may trigger reallocation
      ! which would invalidate the decl_orig selector. Using a stack copy prevents this.
            template_decl = decl_orig

            allocate (new_indices(size(template_decl%var_names)))
            new_count = 0
            is_parameter = is_procedure_parameter(arena, decl_index)

            ! Safe: template_decl is a stack copy, not an arena pointer
            do i = 1, size(template_decl%var_names)
                needs_allocatable = variable_requires_allocatable( &
                                    template_decl%var_names(i), assigned_vars, &
                                    assignment_counts, &
                                    var_count, is_parameter, template_decl%is_array)
                call append_split_declaration(arena, template_decl, &
                                              template_decl%var_names(i), &
                                              needs_allocatable, prog_index, &
                                              new_indices, new_count)
            end do

            call replace_declaration_with_list(arena, prog_index, decl_index, &
                                               new_indices, new_count)
            deallocate (new_indices)
        end select
    end subroutine split_multi_variable_declaration

    ! Create a split declaration from original with specified variables
    subroutine create_split_declaration(orig_decl, var_names, var_count, &
                                        is_allocatable, new_decl)
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
            if (allocated(new_decl%var_names)) deallocate (new_decl%var_names)
        else
            new_decl%is_multi_declaration = .true.
            new_decl%var_name = var_names(1)  ! First variable as primary
            if (allocated(new_decl%var_names)) deallocate (new_decl%var_names)
            allocate (character(len=64) :: new_decl%var_names(var_count))
            new_decl%var_names(1:var_count) = var_names(1:var_count)
        end if

        new_decl%is_allocatable = is_allocatable

 ! Adjust dimensions for allocatable arrays (but preserve explicit bounds - fixes #1812)
        if (is_allocatable .and. new_decl%is_array .and. &
            allocated(new_decl%dimension_indices)) then
            ! Only convert to deferred shape if array does NOT have explicit bounds
            if (.not. has_explicit_array_bounds(new_decl)) then
                deallocate (new_decl%dimension_indices)
                allocate (new_decl%dimension_indices(1))
                new_decl%dimension_indices(1) = 0  ! Deferred shape
            end if
        end if
    end subroutine create_split_declaration

    ! Update program body indices to replace old declaration with new ones
    subroutine update_program_body_indices(arena, prog_index, old_decl_index, &
                                           new_alloc_index, new_non_alloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, old_decl_index
        integer, intent(in) :: new_alloc_index, new_non_alloc_index
        integer :: old_pos, new_size, insert_pos
        integer, allocatable :: new_body_indices(:)

        if (.not. node_is_active(arena, prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return

            old_pos = find_index(prog%body_indices, old_decl_index)
            if (old_pos == 0) return

            new_size = size(prog%body_indices) - 1
            if (new_alloc_index > 0) new_size = new_size + 1
            if (new_non_alloc_index > 0) new_size = new_size + 1

            allocate (new_body_indices(new_size))

            if (old_pos > 1) then
                new_body_indices(1:old_pos - 1) = prog%body_indices(1:old_pos - 1)
            end if

            insert_pos = old_pos
            if (new_alloc_index > 0) then
                new_body_indices(insert_pos) = new_alloc_index
                insert_pos = insert_pos + 1
            end if
            if (new_non_alloc_index > 0) then
                new_body_indices(insert_pos) = new_non_alloc_index
                insert_pos = insert_pos + 1
            end if

            if (old_pos < size(prog%body_indices)) then
                new_body_indices(insert_pos:new_size) = &
                    prog%body_indices(old_pos + 1:size(prog%body_indices))
            end if

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

    ! Check if declaration is a derived type component
    function is_type_component(arena, decl_index) result(is_component)
        use ast_nodes_data, only: derived_type_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        logical :: is_component
        integer :: parent_index

        is_component = .false.

        if (decl_index <= 0 .or. decl_index > arena%size) return

        parent_index = arena%entries(decl_index)%parent_index
        if (parent_index <= 0 .or. parent_index > arena%size) return
        if (.not. allocated(arena%entries(parent_index)%node)) return

        select type (parent => arena%entries(parent_index)%node)
        type is (derived_type_node)
            is_component = .true.
        end select
    end function is_type_component

    ! Mark variables that need allocatable due to string length changes (Issue 218)
    subroutine mark_allocatable_for_string_length_changes(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        character(len=64), allocatable :: string_vars_needing_allocatable(:)
        integer :: var_count

        ! First pass: collect variables that need allocatable strings
        allocate (string_vars_needing_allocatable(100))
        var_count = 0

        if (allocated(prog%body_indices)) then
            call collect_program_string_targets(arena, prog%body_indices, &
                                                string_vars_needing_allocatable, &
                                                var_count)
        end if

        if (var_count > 0 .and. allocated(prog%body_indices)) then
            call apply_string_allocatable_marks(arena, prog%body_indices, &
                                                string_vars_needing_allocatable, &
                                                var_count)
        end if

        deallocate (string_vars_needing_allocatable)
    end subroutine mark_allocatable_for_string_length_changes

    subroutine collect_program_string_targets(arena, body_indices, targets, &
                                              target_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=64), intent(inout) :: targets(:)
        integer, intent(inout) :: target_count
        integer :: i

        do i = 1, size(body_indices)
            if (.not. node_is_active(arena, body_indices(i))) cycle
            call collect_string_vars_needing_allocatable(arena, body_indices(i), &
                                                         targets, target_count)
        end do
    end subroutine collect_program_string_targets

    subroutine apply_string_allocatable_marks(arena, body_indices, targets, &
                                              target_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=64), intent(in) :: targets(:)
        integer, intent(in) :: target_count
        integer :: i

        do i = 1, size(body_indices)
            if (.not. node_is_active(arena, body_indices(i))) cycle
            select type (stmt => arena%entries(body_indices(i))%node)
            type is (declaration_node)
                call mark_string_declaration(stmt, targets, target_count)
            end select
        end do
    end subroutine apply_string_allocatable_marks

    subroutine mark_string_declaration(stmt, target_names, var_count)
        type(declaration_node), intent(inout) :: stmt
        character(len=64), intent(in) :: target_names(:)
        integer, intent(in) :: var_count
        integer :: idx
        character(len=64) :: type_name_trim
        logical :: is_character_decl

        if (var_count <= 0) return

        idx = locate_variable(stmt%var_name, target_names, var_count)
        if (idx <= 0) return

        type_name_trim = ""
        if (allocated(stmt%type_name)) then
            type_name_trim = trim(stmt%type_name)
            if (has_explicit_character_length(stmt%type_name)) then
                stmt%inferred_type%alloc_info%needs_allocatable_string = .false.
                return
            end if
        end if

        is_character_decl = (type_name_trim == 'character' .or. &
                             stmt%inferred_type%kind == TCHAR)
        if (.not. is_character_decl) return

        if (stmt%inferred_type%kind > 0) then
            stmt%inferred_type%alloc_info%needs_allocatable_string = .true.
            stmt%type_name = ""
        else
            stmt%inferred_type%kind = TCHAR
            stmt%inferred_type%size = 0
            stmt%inferred_type%alloc_info%needs_allocatable_string = .true.
            stmt%type_name = ""
        end if
    end subroutine mark_string_declaration

    ! Recursively collect variables that need allocatable strings
    subroutine collect_string_vars_needing_allocatable(arena, stmt_index, &
                                                       var_list, var_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_list(:)
        integer, intent(inout) :: var_count
        type(node_stack_t) :: stack
        integer :: current_index

        call stack_init(stack)
        call stack_push(stack, stmt_index)

        do while (.not. stack_is_empty(stack))
            current_index = stack_pop(stack)
            if (.not. node_is_active(arena, current_index)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (assignment_node)
                if (.not. node_is_active(arena, stmt%target_index)) cycle
                select type (target => arena%entries(stmt%target_index)%node)
                type is (identifier_node)
                    call record_string_variable(target, var_list, var_count)
                end select
            type is (do_loop_node)
                if (allocated(stmt%body_indices)) then
                    call stack_push_many(stack, stmt%body_indices)
                end if
            type is (if_node)
                if (allocated(stmt%else_body_indices)) then
                    call stack_push_many(stack, stmt%else_body_indices)
                end if
                if (allocated(stmt%then_body_indices)) then
                    call stack_push_many(stack, stmt%then_body_indices)
                end if
            class default
                cycle
            end select
        end do
    end subroutine collect_string_vars_needing_allocatable

    logical function apply_allocatable_attributes(decl)
        type(declaration_node), intent(inout) :: decl
        logical :: is_character

        apply_allocatable_attributes = .false.
        is_character = .false.

        ! Never apply allocatable to PARAMETER declarations - they conflict
        if (decl%is_parameter) then
            return
        end if

        ! Never apply allocatable to POINTER declarations - they are mutually exclusive
        ! ISO/IEC 1539-1:2018 Section 8.5.3: ALLOCATABLE and POINTER are incompatible
        if (decl%is_pointer) then
            return
        end if

        if (allocated(decl%type_name)) then
            if (trim(decl%type_name) == "character") then
                is_character = .true.
            end if
        end if

        if (decl%inferred_type%kind == TCHAR) then
            is_character = .true.
        end if

        ! Apply allocatable attributes only when explicitly needed:
        ! - character strings with deferred length
        ! - arrays flagged by assignment tracking (but NOT if they have explicit bounds)
        if (decl%is_array) then
        ! Do NOT convert arrays with explicit bounds to allocatable (fixes #1812, #2149)
            ! Arrays like integer :: arr(0:9) or real :: arr(-5:5) must preserve bounds
            ! Arrays from slices like slice1 = arr(1:5) have fixed sizes already set
            if (has_explicit_array_bounds(decl)) then
                return
            end if
            ! Assignment tracking now excludes element writes, so this path only
            ! runs for arrays that must be deferred shape.
            decl%is_allocatable = .true.
            ! Convert to deferred shape for allocatable arrays
            if (allocated(decl%dimension_indices)) then
                deallocate (decl%dimension_indices)
            end if
            allocate (decl%dimension_indices(1))
            decl%dimension_indices(1) = 0  ! Deferred shape (:)
            apply_allocatable_attributes = .true.
        else if (is_character) then
            decl%is_allocatable = .true.
            apply_allocatable_attributes = .true.
        end if
    end function apply_allocatable_attributes

    pure logical function has_explicit_character_length(type_name) result(has_len)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        if (len_trim(type_name) == 0) then
            has_len = .false.
            return
        end if

        lowered = trim(to_lower(type_name))

        if (index(lowered, "len=") > 0) then
            has_len = .true.
        else if (index(lowered, "(*)") > 0) then
            has_len = .true.
        else
            has_len = .false.
        end if
    end function has_explicit_character_length

    pure logical function has_explicit_array_bounds(decl) result(has_bounds)
        ! Check if a declaration has explicit array bounds or size
        ! (e.g., arr(5), arr(0:9), arr(-5:5)) vs deferred shape (e.g., arr(:))
        ! Arrays with explicit bounds/size should NOT be converted to allocatable
        ! to preserve the original declaration (fixes #2022).
        type(declaration_node), intent(in) :: decl
        integer :: i

        has_bounds = .false.

        if (.not. decl%is_array) return
        if (.not. allocated(decl%dimension_indices)) return
        if (size(decl%dimension_indices) == 0) return

        ! Check each dimension:
        ! - dimension_indices(i) = 0 means deferred shape (:)
        ! - dimension_indices(i) > 0 means explicit size or range expression
        ! Only deferred shape should be converted to allocatable
        do i = 1, size(decl%dimension_indices)
            if (decl%dimension_indices(i) > 0) then
                has_bounds = .true.
                return
            end if
        end do
    end function has_explicit_array_bounds

end module standardizer_allocatable
