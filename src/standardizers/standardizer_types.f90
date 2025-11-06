module standardizer_types
    ! Type inference and utilities module
    ! Handles type analysis, expression type detection, and type string generation

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_bounds, only: array_slice_node
    use ast_nodes_loops
    use ast_nodes_misc, only: complex_literal_node
    use type_system_unified
    use type_string_utils, only: mono_type_to_string
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result, &
                              ERROR_TYPE_SYSTEM
    implicit none
    private

    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    ! Type standardization configuration (local copy)
    ! DISABLED: Converting real -> real(8) breaks generic interfaces that
    ! depend on exact type matching. Users should explicitly use real(8) or
    ! kind parameters if they want double precision.
    logical, save :: standardizer_type_standardization_enabled = .false.

    ! Result type for string operations
    type, public :: string_result_t
        type(result_t) :: result
        character(len=:), allocatable :: value  ! Valid only if result%success = .true.
    contains
        procedure :: is_success => string_is_success
        procedure :: get_value => string_get_value
        procedure :: get_error => string_get_error
    end type string_result_t

    public :: INVALID_INTEGER
    public :: is_array_type
    public :: get_expression_type
    public :: has_array_slice_args
    public :: is_array_expression
    public :: has_implied_do_loop
    public :: get_implied_do_size
    public :: calculate_loop_size
    public :: get_integer_literal_value
    public :: get_array_var_type
    public :: infer_element_type_from_literal
    public :: get_fortran_type_string

contains

    ! Local implementation of get_standardizer_type_standardization
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    ! Check if a mono_type is an array type
    function is_array_type(mono_type) result(is_array)
        type(mono_type_t), intent(in) :: mono_type
        logical :: is_array

        is_array = (mono_type%kind == TARRAY)
    end function is_array_type

    ! Get the type of an expression from the AST
    function get_expression_type(arena, expr_index) result(expr_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t), pointer :: expr_type

        expr_type => null()

        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            expr_type => get_literal_type(node)
        type is (identifier_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (array_literal_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (call_or_subscript_node)
            expr_type => get_call_or_subscript_type(arena, node)
        type is (array_slice_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (binary_op_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (complex_literal_node)
            expr_type => get_complex_literal_type(node)
        end select
    end function get_expression_type

    ! Get type for literal node
    function get_literal_type(node) result(expr_type)
        type(literal_node), intent(in), target :: node
        type(mono_type_t), pointer :: expr_type

        expr_type => null()

        if (node%inferred_type%kind > 0) then
            expr_type => node%inferred_type
        else
            allocate (expr_type)
            select case (node%literal_kind)
            case (LITERAL_INTEGER)
                expr_type = create_mono_type(TINT)
            case (LITERAL_REAL)
                expr_type = create_mono_type(TREAL)
            case (LITERAL_STRING)
                expr_type = create_mono_type(TCHAR)
                if (allocated(node%value)) then
                    if (len(node%value) >= 2) then
                        expr_type%size = len(node%value) - 2
                    else
                        expr_type%size = 0
                    end if
                end if
            case (LITERAL_LOGICAL)
                expr_type = create_mono_type(TLOGICAL)
            case default
                expr_type = create_mono_type(TREAL)
            end select
        end if
    end function get_literal_type

    ! Get type for complex literal node
    function get_complex_literal_type(node) result(expr_type)
        type(complex_literal_node), intent(in), target :: node
        type(mono_type_t), pointer :: expr_type

        if (node%inferred_type%kind > 0) then
            expr_type => node%inferred_type
        else
            allocate (expr_type)
            expr_type = create_mono_type(TCOMPLEX)
        end if
    end function get_complex_literal_type

    ! Get type for call or subscript node
    function get_call_or_subscript_type(arena, node) result(expr_type)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in), target :: node
        type(mono_type_t), pointer :: expr_type

        expr_type => null()

        if (node%inferred_type%kind > 0) then
            expr_type => node%inferred_type
        else
            expr_type => try_get_intrinsic_type(node)
            if (associated(expr_type)) return

            if (has_array_slice_args(arena, node)) then
                expr_type => build_array_slice_type(node)
            end if
        end if
    end function get_call_or_subscript_type

    ! Try to get intrinsic function return type
    function try_get_intrinsic_type(node) result(expr_type)
        use intrinsic_registry, only: get_intrinsic_signature, &
                                      is_intrinsic_function
        type(call_or_subscript_node), intent(in) :: node
        type(mono_type_t), pointer :: expr_type
        character(len=:), allocatable :: intrinsic_sig
        logical :: is_intrinsic_func

        expr_type => null()

        is_intrinsic_func = is_intrinsic_function(node%name)
        if (is_intrinsic_func) then
            intrinsic_sig = get_intrinsic_signature(node%name)
            if (len_trim(intrinsic_sig) > 0) then
                allocate (expr_type)
                expr_type = parse_intrinsic_return_type(intrinsic_sig)
            end if
        end if
    end function try_get_intrinsic_type

    ! Parse return type from intrinsic signature
    function parse_intrinsic_return_type(intrinsic_sig) result(typ)
        character(len=*), intent(in) :: intrinsic_sig
        type(mono_type_t) :: typ

        if (index(intrinsic_sig, "real(") == 1) then
            typ = create_mono_type(TREAL)
        else if (index(intrinsic_sig, "integer(") == 1) then
            typ = create_mono_type(TINT)
        else if (index(intrinsic_sig, "logical(") == 1) then
            typ = create_mono_type(TLOGICAL)
        else if (index(intrinsic_sig, "character(") == 1) then
            typ = create_mono_type(TCHAR)
        else
            typ = create_mono_type(TREAL)
        end if
    end function parse_intrinsic_return_type

    ! Build array slice type based on name patterns
    function build_array_slice_type(node) result(expr_type)
        type(call_or_subscript_node), intent(in) :: node
        type(mono_type_t), pointer :: expr_type
        type(mono_type_t) :: element_type_args(1)

        allocate (expr_type)

        if (allocated(node%name)) then
            element_type_args(1) = infer_element_type_from_name(node%name)
        else
            element_type_args(1) = create_mono_type(TREAL)
        end if
        expr_type = create_mono_type(TARRAY, args=element_type_args)
    end function build_array_slice_type

    ! Infer element type from array name patterns
    function infer_element_type_from_name(name) result(element_type)
        character(len=*), intent(in) :: name
        type(mono_type_t) :: element_type

        if (index(name, "int") > 0 .or. index(name, "idx") > 0 .or. &
            index(name, "_i") > 0) then
            element_type = create_mono_type(TINT)
        else if (index(name, "real") > 0 .or. index(name, "float") > 0 .or. &
                 index(name, "_r") > 0) then
            element_type = create_mono_type(TREAL)
        else
            element_type = create_mono_type(TREAL)
        end if
    end function infer_element_type_from_name

    ! Check if a call_or_subscript node has array slice arguments
    function has_array_slice_args(arena, node) result(has_slice)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        logical :: has_slice
        integer :: i

        has_slice = .false.

        if (.not. allocated(node%arg_indices)) return

        do i = 1, size(node%arg_indices)
            if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                if (allocated(arena%entries(node%arg_indices(i))%node)) then
                    select type (arg => arena%entries(node%arg_indices(i))%node)
                    type is (binary_op_node)
                        if (trim(arg%operator) == ":") then
                            has_slice = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function has_array_slice_args

    ! Check if an expression is an array expression by structure
    function is_array_expression(arena, expr_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        logical :: is_array

        is_array = .false.

        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            is_array = .true.
        type is (call_or_subscript_node)
            ! Check if this is an array slice (has colon operator in args)
            if (has_array_slice_args(arena, node)) then
                is_array = .true.
            end if
        end select
    end function is_array_expression

    ! Check if array literal contains an implied do loop
    function has_implied_do_loop(arena, array_node) result(has_implied)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: array_node
        logical :: has_implied

        has_implied = .false.

        if (allocated(array_node%element_indices)) then
            if (size(array_node%element_indices) == 1) then
                ! Check if the single element is a do_loop_node
                if (array_node%element_indices(1) > 0 .and. &
                    array_node%element_indices(1) <= arena%size) then
                    if (allocated(arena%entries(array_node%element_indices(1))%node)) then
                        select type (elem => &
                                     arena%entries(array_node%element_indices(1))%node)
                        type is (do_loop_node)
                            has_implied = .true.
                        class default
                        end select
                    end if
                end if
            end if
        end if
    end function has_implied_do_loop

    ! Calculate size of implied do loop, handling nested loops recursively
    recursive function get_implied_do_size(arena, do_node_index) result(total_size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_node_index
        integer :: total_size
        integer :: outer_size, inner_size, num_body

        total_size = -1  ! Return -1 if we can't determine the size

        if (do_node_index <= 0 .or. do_node_index > arena%size) return
        if (.not. allocated(arena%entries(do_node_index)%node)) return

        select type (do_node => arena%entries(do_node_index)%node)
        type is (do_loop_node)
            ! Calculate this loop's iteration count
            if (do_node%start_expr_index > 0 .and. do_node%end_expr_index > 0) then
                outer_size = calculate_loop_size(arena, do_node%start_expr_index, &
                                                 do_node%end_expr_index, &
                                                 do_node%step_expr_index)
                if (outer_size <= 0) then
                    total_size = outer_size
                    return
                end if

                ! Check if body contains nested do_loop_node
                if (allocated(do_node%body_indices)) then
                    num_body = size(do_node%body_indices)
                    if (num_body == 0) then
                        total_size = outer_size
                        return
                    end if
                    if (do_node%body_indices(1) > 0 .and. &
                        do_node%body_indices(1) <= arena%size) then
                        if (allocated(arena%entries(do_node%body_indices(1))%node)) then
                            select type (body_node => &
                                         arena%entries(do_node%body_indices(1))%node)
                            type is (do_loop_node)
                                ! Nested loop: multiply sizes
                                inner_size = get_implied_do_size( &
                                             arena, do_node%body_indices(1))
                                if (inner_size > 0) then
                                    total_size = outer_size * inner_size
                                else
                                    total_size = outer_size
                                end if
                                return
                            class default
                                total_size = outer_size
                            end select
                        else
                            total_size = outer_size
                        end if
                    else
                        total_size = outer_size
                    end if
                else
                    total_size = outer_size
                end if
            end if
        end select
    end function get_implied_do_size

    ! Calculate loop size from start, end, and step expressions
    function calculate_loop_size(arena, start_idx, end_idx, step_idx) result(size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: start_idx, end_idx, step_idx
        integer :: size
        integer :: start_val, end_val, step_val

        size = -1

        ! Get start value
        start_val = get_integer_literal_value(arena, start_idx)
        if (start_val == INVALID_INTEGER) then
            start_val = resolve_identifier_integer_value(arena, start_idx)
            if (start_val == INVALID_INTEGER) return
        end if

        ! Get end value
        end_val = get_integer_literal_value(arena, end_idx)
        if (end_val == INVALID_INTEGER) then
            end_val = resolve_identifier_integer_value(arena, end_idx)
            if (end_val == INVALID_INTEGER) return
        end if

        ! Get step value (default to 1 if not specified)
        if (step_idx > 0) then
            step_val = get_integer_literal_value(arena, step_idx)
            if (step_val == INVALID_INTEGER) then
                step_val = resolve_identifier_integer_value(arena, step_idx)
                if (step_val == INVALID_INTEGER) step_val = 1
            end if
        else
            step_val = 1
        end if

        ! Calculate size
        if (step_val /= 0) then
            if (step_val > 0) then
                ! Forward iteration
                if (end_val >= start_val) then
                    size = (end_val - start_val) / step_val + 1
                else
                    size = 0  ! No iterations
                end if
            else
                ! Backward iteration
                if (start_val >= end_val) then
                    size = (start_val - end_val) / abs(step_val) + 1
                else
                    size = 0  ! No iterations
                end if
            end if
        end if
    end function calculate_loop_size

    ! Get integer value from a literal node
    function get_integer_literal_value(arena, expr_idx) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_idx
        integer :: value

        type stack_entry
            integer :: idx = 0
            logical :: processed = .false.
        end type stack_entry

        type(stack_entry), allocatable :: stack(:)
        integer, allocatable :: results(:)
        integer :: capacity, top
        type(stack_entry) :: current
        integer :: idx
        integer :: left_val, right_val
        integer :: iostat

        value = INVALID_INTEGER
        if (expr_idx <= 0 .or. expr_idx > arena%size) return

        if (.not. allocated(arena%entries(expr_idx)%node)) return

        allocate (results(arena%size))
        results = INVALID_INTEGER
        capacity = 64
        allocate (stack(capacity))
        top = 0

        call push(expr_idx, .false.)

        do while (top > 0)
            current = pop()
            idx = current%idx

            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            if (.not. current%processed) then
                call push(idx, .true.)
                call queue_subexpressions(arena, idx)
            else
                value = evaluate_node(arena, idx, results)
                results(idx) = value
            end if
        end do

        value = results(expr_idx)

    contains

        subroutine queue_subexpressions(arena_l, node_idx)
            type(ast_arena_t), intent(in) :: arena_l
            integer, intent(in) :: node_idx

            select type (node => arena_l%entries(node_idx)%node)
            type is (binary_op_node)
                if (allocated(node%operator)) then
                    call push(node%right_index, .false.)
                    call push(node%left_index, .false.)
                end if
            class default
                ! literals handled in evaluate_node
            end select
        end subroutine queue_subexpressions

        function evaluate_node(arena_l, node_idx, res) result(val)
            type(ast_arena_t), intent(in) :: arena_l
            integer, intent(in) :: node_idx
            integer, intent(in) :: res(:)
            integer :: val
            integer :: iostat_loc

            val = INVALID_INTEGER

            select type (node => arena_l%entries(node_idx)%node)
            type is (literal_node)
                val = evaluate_literal_node(node, iostat_loc)
            type is (binary_op_node)
                val = evaluate_binary_op_node(node, res, arena_l%size)
            type is (identifier_node)
                val = evaluate_identifier_node(node)
            class default
                val = INVALID_INTEGER
            end select
        end function evaluate_node

        function evaluate_literal_node(node, iostat_loc) result(val)
            type(literal_node), intent(in) :: node
            integer, intent(out) :: iostat_loc
            integer :: val

            if (node%literal_kind == LITERAL_INTEGER .and. &
                allocated(node%value)) then
                read (node%value, *, iostat=iostat_loc) val
                if (iostat_loc /= 0) val = INVALID_INTEGER
            else
                val = INVALID_INTEGER
            end if
        end function evaluate_literal_node

        function evaluate_binary_op_node(node, res, arena_sz) result(val)
            type(binary_op_node), intent(in) :: node
            integer, intent(in) :: res(:)
            integer, intent(in) :: arena_sz
            integer :: val
            integer :: left_val, right_val

            val = INVALID_INTEGER
            if (.not. allocated(node%operator)) return

            left_val = INVALID_INTEGER
            right_val = INVALID_INTEGER
            if (node%left_index > 0 .and. node%left_index <= arena_sz) &
                left_val = res(node%left_index)
            if (node%right_index > 0 .and. node%right_index <= arena_sz) &
                right_val = res(node%right_index)

            if (left_val /= INVALID_INTEGER .and. &
                right_val /= INVALID_INTEGER) then
                select case (node%operator)
                case ("-")
                    val = left_val - right_val
                case ("+")
                    val = left_val + right_val
                case ("*")
                    val = left_val * right_val
                case ("/")
                    if (right_val /= 0) val = left_val / right_val
                end select
            end if
        end function evaluate_binary_op_node

        function evaluate_identifier_node(node) result(val)
            type(identifier_node), intent(in) :: node
            integer :: val

            if (node%is_constant .and. node%constant_type == &
                LITERAL_INTEGER) then
                val = node%constant_integer
            else
                val = INVALID_INTEGER
            end if
        end function evaluate_identifier_node

        subroutine push(i, processed)
            integer, intent(in) :: i
            logical, intent(in) :: processed
            type(stack_entry), allocatable :: tmp(:)
            if (top >= capacity) then
                allocate (tmp(capacity * 2))
                if (capacity > 0) tmp(1:capacity) = stack(1:capacity)
                call move_alloc(tmp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = i
            stack(top)%processed = processed
        end subroutine push

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

    end function get_integer_literal_value

    integer function resolve_identifier_integer_value(arena, expr_idx) result(val)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_idx

        character(len=:), allocatable :: name
        integer :: i

        val = INVALID_INTEGER
        if (expr_idx <= 0 .or. expr_idx > arena%size) return
        if (.not. allocated(arena%entries(expr_idx)%node)) return

        select type (node => arena%entries(expr_idx)%node)
        type is (identifier_node)
            if (.not. allocated(node%name)) return
            name = trim(node%name)
            if (len_trim(name) == 0) return
            if (node%is_constant .and. node%constant_type == LITERAL_INTEGER) then
                val = node%constant_integer
                return
            end if

            do i = expr_idx - 1, 1, -1
                if (.not. allocated(arena%entries(i)%node)) cycle
                select type (assign_node => arena%entries(i)%node)
                type is (assignment_node)
                    if (assign_node%target_index <= 0 .or. assign_node%target_index > &
                        arena%size) cycle
                    if (.not. &
                        allocated(arena%entries(assign_node%target_index)%node)) cycle
                    select type (target => arena%entries(assign_node%target_index)%node)
                    type is (identifier_node)
                        if (.not. allocated(target%name)) cycle
                        if (trim(target%name) /= name) cycle
                        val = get_integer_literal_value(arena, assign_node%value_index)
                        if (val /= INVALID_INTEGER) return
                    end select
                end select
            end do
        end select
    end function resolve_identifier_integer_value

    ! Get array variable type declaration from an array expression
    function get_array_var_type(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type

        var_type = ""

        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            if (allocated(node%element_indices)) then
                var_type = process_array_literal_type(arena, node)
            end if
        type is (call_or_subscript_node)
            if (has_array_slice_args(arena, node)) then
                var_type = ""
            end if
        end select
    end function get_array_var_type

    function process_array_literal_type(arena, node) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=64) :: var_type
        character(len=:), allocatable :: elem_type_str

        elem_type_str = infer_array_element_type(arena, node)

        if (has_implied_do_loop(arena, node)) then
            var_type = build_implied_do_type(arena, node, elem_type_str)
        else if (allocated(node%syntax_style) .and. node%syntax_style == "modern") then
            var_type = try_build_modern_implied_do_type(arena, node, elem_type_str)
            if (len_trim(var_type) == 0) then
                var_type = build_regular_array_type(arena, node, elem_type_str)
            end if
        else
            var_type = build_regular_array_type(arena, node, elem_type_str)
        end if
    end function process_array_literal_type

    function infer_array_element_type(arena, node) result(elem_type_str)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=:), allocatable :: elem_type_str
        type(mono_type_t), pointer :: expr_type

        if (node%inferred_type%kind > 0) then
            elem_type_str = extract_inferred_element_type(node%inferred_type)
        else if (size(node%element_indices) > 0) then
            expr_type => get_expression_type(arena, node%element_indices(1))
            if (associated(expr_type)) then
                elem_type_str = extract_expression_element_type(expr_type)
            else
                elem_type_str = infer_element_type_from_literal(arena, &
                                                                node%element_indices(1))
            end if
        else
            elem_type_str = ""
        end if

        if ((.not. allocated(elem_type_str) .or. len_trim(elem_type_str) == 0) .and. &
            allocated(node%type_spec) .and. len_trim(node%type_spec) > 0) then
            elem_type_str = trim(node%type_spec)
        end if

        if (.not. allocated(elem_type_str) .or. len_trim(elem_type_str) == 0) then
            elem_type_str = "integer"
        end if
    end function infer_array_element_type

    function extract_inferred_element_type(inferred_type) result(elem_type_str)
        type(mono_type_t), intent(in) :: inferred_type
        character(len=:), allocatable :: elem_type_str
        type(string_result_t) :: type_result

        type_result = get_fortran_type_string(inferred_type)
        if (type_result%is_success()) then
            elem_type_str = type_result%get_value()
        else
            elem_type_str = ""
        end if
    end function extract_inferred_element_type

    function extract_expression_element_type(expr_type) result(elem_type_str)
        type(mono_type_t), pointer, intent(in) :: expr_type
        character(len=:), allocatable :: elem_type_str
        type(string_result_t) :: type_result

        type_result = get_fortran_type_string(expr_type)
        if (type_result%is_success()) then
            elem_type_str = type_result%get_value()
        else
            elem_type_str = ""
        end if
    end function extract_expression_element_type

    function build_implied_do_type(arena, node, elem_type_str) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elem_type_str
        character(len=64) :: var_type
        integer :: implied_size

        implied_size = get_implied_do_size(arena, node%element_indices(1))
        if (implied_size > 0) then
            write (var_type, '(a,a,i0,a)') trim(elem_type_str), &
                ", dimension(", implied_size, ")"
        else
            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
        end if
    end function build_implied_do_type

    function try_build_modern_implied_do_type(arena, node, elem_type_str) &
        result(var_type)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elem_type_str
        character(len=64) :: var_type
        integer :: start_val, end_val, step_val, sz
        logical :: ok

        var_type = ""

        if (.not. allocated(node%element_indices)) return
        if (size(node%element_indices) < 4) return

        ok = check_modern_implied_do_pattern(arena, node)
        if (.not. ok) return

        start_val = get_integer_literal_value(arena, node%element_indices(3))
        end_val = get_integer_literal_value(arena, node%element_indices(4))
        if (size(node%element_indices) >= 5) then
            step_val = get_integer_literal_value(arena, node%element_indices(5))
        else
            step_val = INVALID_INTEGER
        end if

        if (start_val == INVALID_INTEGER .or. end_val == INVALID_INTEGER) then
            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
            return
        end if

        if (step_val == INVALID_INTEGER) then
            sz = calculate_loop_size(arena, node%element_indices(3), &
                                     node%element_indices(4), 0)
        else
            sz = calculate_loop_size(arena, node%element_indices(3), &
                                     node%element_indices(4), &
                                     node%element_indices(5))
        end if

        if (sz > 0) then
            write (var_type, '(a,a,i0,a)') trim(elem_type_str), &
                ", dimension(", sz, ")"
        else
            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
        end if
    end function try_build_modern_implied_do_type

    function check_modern_implied_do_pattern(arena, node) result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        logical :: is_valid
        integer :: idx

        is_valid = .false.

        if (node%element_indices(2) <= 0 .or. &
            node%element_indices(2) > arena%size) return
        if (.not. allocated(arena%entries(node%element_indices(2))%node)) return

        select type (idnode => arena%entries(node%element_indices(2))%node)
        type is (identifier_node)
            is_valid = .true.
        end select
    end function check_modern_implied_do_pattern

    function build_regular_array_type(arena, node, elem_type_str) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elem_type_str
        character(len=64) :: var_type
        logical :: has_array_element, all_literal_arrays
        integer :: inner_size

        if (size(node%element_indices) == 0) then
            var_type = trim(elem_type_str) // ", dimension(0)"
            return
        end if

        call analyze_nested_arrays(arena, node, has_array_element, &
                                   all_literal_arrays, inner_size)

        if (has_array_element) then
            var_type = build_nested_array_type(node, elem_type_str, &
                                               all_literal_arrays, inner_size)
        else
            write (var_type, '(a,a,i0,a)') trim(elem_type_str), &
                ", dimension(", size(node%element_indices), ")"
        end if
    end function build_regular_array_type

    subroutine analyze_nested_arrays(arena, node, has_array_element, &
                                      all_literal_arrays, inner_size)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        logical, intent(out) :: has_array_element, all_literal_arrays
        integer, intent(out) :: inner_size
        integer :: elem_idx, i

        has_array_element = .false.
        all_literal_arrays = .true.
        inner_size = -1

        do i = 1, size(node%element_indices)
            elem_idx = node%element_indices(i)
            if (elem_idx <= 0 .or. elem_idx > arena%size) cycle
            if (.not. allocated(arena%entries(elem_idx)%node)) cycle

            select type (arr_node => arena%entries(elem_idx)%node)
            type is (array_literal_node)
                call process_nested_array_literal(arr_node, has_array_element, &
                                                   all_literal_arrays, inner_size)
            type is (identifier_node)
                if (arr_node%inferred_type%kind == TARRAY) then
                    has_array_element = .true.
                    all_literal_arrays = .false.
                end if
            end select
        end do
    end subroutine analyze_nested_arrays

    subroutine process_nested_array_literal(arr_node, has_array_element, &
                                             all_literal_arrays, inner_size)
        type(array_literal_node), intent(in) :: arr_node
        logical, intent(inout) :: has_array_element, all_literal_arrays
        integer, intent(inout) :: inner_size

        has_array_element = .true.
        if (allocated(arr_node%element_indices)) then
            if (size(arr_node%element_indices) > 0) then
                if (inner_size < 0) then
                    inner_size = size(arr_node%element_indices)
                else if (inner_size /= size(arr_node%element_indices)) then
                    all_literal_arrays = .false.
                end if
            else
                all_literal_arrays = .false.
            end if
        else
            all_literal_arrays = .false.
        end if
    end subroutine process_nested_array_literal

    function build_nested_array_type(node, elem_type_str, all_literal_arrays, &
                                      inner_size) result(var_type)
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elem_type_str
        logical, intent(in) :: all_literal_arrays
        integer, intent(in) :: inner_size
        character(len=64) :: var_type

        if (all_literal_arrays .and. inner_size > 0) then
            write (var_type, '(a,a,i0,a,i0,a)') trim(elem_type_str), &
                ", dimension(", size(node%element_indices), ",", inner_size, ")"
        else
            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
        end if
    end function build_nested_array_type

    ! Helper function to infer type from a literal node
    function infer_element_type_from_literal(arena, elem_index) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: elem_index
        character(len=:), allocatable :: type_str
        logical :: standardizer_type_standardization_enabled

        type_str = ""  ! Empty default indicates type not determined

     call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        if (elem_index <= 0 .or. elem_index > arena%size) return
        if (.not. allocated(arena%entries(elem_index)%node)) return

        select type (elem => arena%entries(elem_index)%node)
        type is (literal_node)
            select case (elem%literal_kind)
            case (LITERAL_INTEGER)
                type_str = "integer"
            case (LITERAL_REAL)
                if (standardizer_type_standardization_enabled) then
                    type_str = "real(8)"
                else
                    type_str = "real"
                end if
            case (LITERAL_STRING)
                type_str = "character"
            case (LITERAL_LOGICAL)
                type_str = "logical"
            case default
                type_str = ""  ! No fallback for unknown literal types
            end select
        end select
    end function infer_element_type_from_literal

    ! Convert mono_type_t to Fortran type string
    recursive function get_fortran_type_string(mono_type) result(string_result)
        type(mono_type_t), intent(in) :: mono_type
        type(string_result_t) :: string_result
        logical :: standardizer_type_standardization_enabled
        logical :: type_success
        character(len=:), allocatable :: type_string
        character(len=:), allocatable :: element_type

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)

        if (mono_type%kind == TARRAY) then
            if (type_args_allocated(mono_type) .and. &
                type_args_size(mono_type) > 0) then
                element_type = mono_type_to_string( &
                               type_args_element(mono_type, 1), include_shape=.false., &
                               prefer_len_zero_char=.true., &
                             standardize_real=standardizer_type_standardization_enabled, &
                               success=type_success)
                if (type_success) then
                    string_result%result = success_result()
                    string_result%value = element_type
                else
                    string_result%result = create_error_result( &
                                           "Failed to determine array element type", &
                                           ERROR_TYPE_SYSTEM, &
                                           component="standardizer", &
                                           context="get_fortran_type_string", &
                             suggestion="Ensure array element type is properly inferred" &
                                           )
                end if
            else
                string_result%result = create_error_result( &
                                       "Array type has no element type information", &
                                       ERROR_TYPE_SYSTEM, &
                                       component="standardizer", &
                                       context="get_fortran_type_string", &
         suggestion="Array type should have at least one type argument for element type" &
                                       )
            end if
            return
        end if

        type_string = mono_type_to_string(mono_type, include_shape=.false., &
                                          prefer_len_zero_char=.true., &
                             standardize_real=standardizer_type_standardization_enabled, &
                                          success=type_success)

        if (type_success) then
            string_result%result = success_result()
            string_result%value = type_string
        else
            string_result%result = create_error_result( &
                                   "Unknown or unsupported type kind", &
                                   ERROR_TYPE_SYSTEM, &
                                   component="standardizer", &
                                   context="get_fortran_type_string", &
          suggestion="Type inference may have failed or encountered an unsupported type" &
                                   )
        end if
    end function get_fortran_type_string

    ! String result methods
    function string_is_success(this) result(success)
        class(string_result_t), intent(in) :: this
        logical :: success
        success = this%result%success
    end function string_is_success

    function string_get_value(this) result(value)
        class(string_result_t), intent(in) :: this
        character(len=:), allocatable :: value
        if (this%result%success .and. allocated(this%value)) then
            value = this%value
        else
            value = ""
        end if
    end function string_get_value

    function string_get_error(this) result(error_msg)
        class(string_result_t), intent(in) :: this
        character(len=:), allocatable :: error_msg
        error_msg = this%result%error_message
    end function string_get_error

end module standardizer_types
