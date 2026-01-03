module constant_transformation
    ! Module for compile-time constant folding and evaluation
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
                              assignment_node, call_or_subscript_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_LOGICAL, LITERAL_STRING
    use ast_nodes_control, only: if_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_core
    use standardizer_types, only: get_integer_literal_value, INVALID_INTEGER
    use debug_trace, only: trace_enter, trace_leave, trace_is_enabled
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: fold_constants_in_arena

contains

    subroutine fold_constants_in_arena(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i
        integer :: trace_counter

        trace_counter = 0

        ! Process all nodes in the arena
        do i = 1, arena%size
            call fold_node_constants(arena, i)
            trace_counter = trace_counter + 1
            ! Debug output disabled to avoid polluting stdout (used for generated code)
            ! if (mod(trace_counter, 100) == 0) then
            !     write (error_unit, '(A,I0,A,I0)') "fold_constants: processed ", &
            !         trace_counter, " of ", arena%size
            ! end if
        end do
    end subroutine fold_constants_in_arena

    subroutine fold_node_constants(arena, node_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        type stack_entry
            integer :: idx = 0
            logical :: processed = .false.
        end type stack_entry

        type(stack_entry), allocatable :: stack(:)
        integer :: top, capacity
        type(stack_entry) :: current
        integer :: iteration_count

        call trace_enter('fold_node_constants')

        capacity = 64
        allocate (stack(capacity))
        top = 0
        iteration_count = 0

        call push(node_index, .false.)

        do while (top > 0)
            iteration_count = iteration_count + 1
            if (trace_is_enabled() .and. mod(iteration_count, 100) == 0) then
                write (error_unit, '(A,I0,A,I0)') &
                    'fold_node_constants: iteration ', iteration_count, &
                    ', stack depth ', top
            end if

            current = pop()
            if (.not. arena%has_node_at(current%idx)) cycle

            if (.not. current%processed) then
                ! Schedule node for processing after its children
                call push(current%idx, .true.)
                select type (node => arena%entries(current%idx)%node)
                type is (binary_op_node)
                    call push(node%right_index, .false.)
                    call push(node%left_index, .false.)
                type is (if_node)
                    call push(node%condition_index, .false.)
                    if (allocated(node%then_body_indices)) then
                        call push_array(node%then_body_indices)
                    end if
                    if (allocated(node%else_body_indices)) then
                        call push_array(node%else_body_indices)
                    end if
                type is (declaration_node)
                    call push(node%initializer_index, .false.)
                type is (array_literal_node)
                    if (allocated(node%element_indices)) then
                        call push_array(node%element_indices)
                    end if
                type is (assignment_node)
                    call push(node%target_index, .false.)
                    call push(node%value_index, .false.)
                type is (call_or_subscript_node)
                    if (allocated(node%arg_indices)) then
                        call push_array(node%arg_indices)
                    end if
                class default
                    cycle
                end select
            else
                select type (node => arena%entries(current%idx)%node)
                type is (literal_node)
                    call fold_literal_node(node)
                type is (binary_op_node)
                    call fold_binary_op_node(arena, node)
                type is (if_node)
                    call fold_if_node(arena, node)
                type is (declaration_node)
                    call fold_declaration_node(arena, node)
                type is (identifier_node)
                    call fold_identifier_node(arena, node, current%idx)
                class default
                    cycle
                end select
            end if
        end do

        call trace_leave('fold_node_constants')

    contains

        subroutine push(idx, processed)
            integer, intent(in) :: idx
            logical, intent(in) :: processed
            type(stack_entry), allocatable :: temp(:)
            if (idx <= 0) return
            if (top >= capacity) then
                allocate (temp(capacity * 2))
                if (capacity > 0) temp(1:capacity) = stack(1:capacity)
                call move_alloc(temp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = idx
            stack(top)%processed = processed
        end subroutine push

        subroutine push_array(indices)
            integer, intent(in) :: indices(:)
            integer :: j
            if (size(indices) <= 0) return
            do j = size(indices), 1, -1
                call push(indices(j), .false.)
            end do
        end subroutine push_array

        function pop() result(entry)
            type(stack_entry) :: entry
            if (top <= 0) then
                entry%idx = 0
                entry%processed = .false.
            else
                entry = stack(top)
                top = top - 1
            end if
        end function pop

    end subroutine fold_node_constants

    subroutine fold_literal_node(node)
        type(literal_node), intent(inout) :: node
        integer :: read_status

        ! Mark literals as constants and store their values
        node%is_constant = .true.

        ! Store the constant value based on type
        if (node%value == ".false." .or. node%value == ".FALSE.") then
            node%constant_logical = .false.
            node%constant_type = LITERAL_LOGICAL
        else if (node%value == ".true." .or. node%value == ".TRUE.") then
            node%constant_logical = .true.
            node%constant_type = LITERAL_LOGICAL
        else if (node%literal_kind == LITERAL_INTEGER) then
            read (node%value, *, iostat=read_status) node%constant_integer
            if (read_status /= 0) then
                node%is_constant = .false.
                return
            end if
            node%constant_type = LITERAL_INTEGER
        else if (node%literal_kind == LITERAL_REAL) then
            read (node%value, *, iostat=read_status) node%constant_real
            if (read_status /= 0) then
                node%is_constant = .false.
                return
            end if
            node%constant_type = LITERAL_REAL
        end if
    end subroutine fold_literal_node

    subroutine fold_binary_op_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(inout) :: node

        ! Validate indices
        if (node%left_index <= 0 .or. node%left_index > arena%size) return
        if (node%right_index <= 0 .or. node%right_index > arena%size) return

        ! Children are already folded by the stack-based traversal
        ! Check if both operands are constants
        associate (left_node => arena%entries(node%left_index)%node, &
                   right_node => arena%entries(node%right_index)%node)
            if (.not. left_node%is_constant .or. .not. right_node%is_constant) return

            ! Fold comparison operators
            select case (trim(node%operator))
            case (">")
                call fold_comparison_gt(node, left_node, right_node)
            case ("<")
                call fold_comparison_lt(node, left_node, right_node)
            case (">=")
                call fold_comparison_ge(node, left_node, right_node)
            case ("<=")
                call fold_comparison_le(node, left_node, right_node)
            case ("==", ".eq.")
                call fold_comparison_eq(node, left_node, right_node)
            case ("/=", ".ne.")
                call fold_comparison_ne(node, left_node, right_node)
            end select
        end associate
    end subroutine fold_binary_op_node

    subroutine fold_comparison_gt(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer > right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real > right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_gt

    subroutine fold_comparison_lt(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer < right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real < right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_lt

    subroutine fold_comparison_ge(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer >= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real >= right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_ge

    subroutine fold_comparison_le(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer <= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real <= right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_le

    subroutine fold_comparison_eq(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer == right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = &
                abs(left%constant_real - right%constant_real) < 1e-10
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_LOGICAL .and. &
                 right%constant_type == LITERAL_LOGICAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_logical .eqv. right%constant_logical
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_eq

    subroutine fold_comparison_ne(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right

        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer /= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = &
                abs(left%constant_real - right%constant_real) >= 1e-10
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_LOGICAL .and. &
                 right%constant_type == LITERAL_LOGICAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_logical .neqv. right%constant_logical
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_ne

    subroutine fold_if_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(inout) :: node

        ! Children are already folded by the stack-based traversal
        ! No additional processing needed for if nodes in constant folding
    end subroutine fold_if_node

    subroutine fold_declaration_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_node), intent(inout) :: node

        ! Children are already folded by the stack-based traversal
        ! If the initializer is constant, propagate that to the declaration
        if (node%has_initializer .and. node%initializer_index > 0 .and. &
            node%initializer_index <= arena%size) then
            select type (init => arena%entries(node%initializer_index)%node)
            class default
                if (init%is_constant) then
                    node%is_constant = .true.
                    node%constant_type = init%constant_type
                    node%constant_integer = init%constant_integer
                    node%constant_real = init%constant_real
                    node%constant_logical = init%constant_logical
                end if
            end select
        end if
    end subroutine fold_declaration_node

    subroutine fold_identifier_node(arena, node, node_index)
        type(ast_arena_t), intent(inout) :: arena
        type(identifier_node), intent(inout) :: node
        integer, intent(in) :: node_index
        integer :: i
        integer :: value
        integer, save :: total_calls = 0
        integer, save :: total_scans = 0

        call trace_enter('fold_identifier_node')

        total_calls = total_calls + 1

        if (trace_is_enabled() .and. mod(total_calls, 100) == 0) then
            write (error_unit, '(A,I0,A,I0)') "fold_identifier_node: called ", &
                total_calls, " times, total scans: ", total_scans
        end if

        ! Early exit if node name is not allocated
        if (.not. allocated(node%name)) then
            call trace_leave('fold_identifier_node')
            return
        end if

        ! Look for declarations with matching name (including parameters)
        ! Only check nodes that come before this one in the arena
        ! Limit backwards scan to 200 nodes for performance (O(n) instead of O(n²))
        do i = node_index - 1, max(1, node_index - 200), -1
            total_scans = total_scans + 1
            select type (entry => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(entry%var_name)) then
                    if (trim(entry%var_name) == trim(node%name)) then
                        ! Propagate the declaration's constant value
                        if (entry%is_constant) then
                            node%is_constant = .true.
                            node%constant_type = entry%constant_type
                            node%constant_integer = entry%constant_integer
                            node%constant_real = entry%constant_real
                            node%constant_logical = entry%constant_logical
                            call trace_leave('fold_identifier_node')
                            return  ! Found match, no need to continue
                        end if
                    end if
                end if
            type is (assignment_node)
                if (.not. arena%has_node_at(entry%target_index)) cycle
                select type (target => arena%entries(entry%target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    if (trim(target%name) /= trim(node%name)) cycle
                    value = get_integer_literal_value(arena, entry%value_index)
                    if (value /= INVALID_INTEGER) then
                        node%is_constant = .true.
                        node%constant_type = LITERAL_INTEGER
                        node%constant_integer = value
                        call trace_leave('fold_identifier_node')
                        return
                    end if
                end select
            end select
        end do

        call trace_leave('fold_identifier_node')
    end subroutine fold_identifier_node

end module constant_transformation
