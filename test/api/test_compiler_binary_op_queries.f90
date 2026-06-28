program test_compiler_binary_op_queries
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, &
        compile_frontend_from_string, &
        is_binary_op, get_binary_op_info, &
        INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: operator, error_msg
    integer :: top_index, left_index, right_index, line, column
    integer :: nested_left, nested_right
    character(len=*), parameter :: source = &
        'program main'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  integer :: a, b, c, d'//new_line('a')// &
        '  a = 1'//new_line('a')// &
        '  b = 2'//new_line('a')// &
        '  c = 3'//new_line('a')// &
        '  d = a + b * c'//new_line('a')// &
        'end program main'

    print *, '=== compiler binary op query API test ==='

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, 'FAIL: compiler frontend rejected source: ', &
            trim(result%diagnostic_text)
        stop 1
    end if

    ! a + b*c parses with + at the top (lower precedence binds last).
    top_index = find_binary_op_with_operator(result, '+')
    if (top_index <= 0) then
        print *, 'FAIL: no + binary operation exposed'
        stop 1
    end if

    call get_binary_op_info(result%arena, top_index, operator, left_index, &
        right_index, line, column, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: get_binary_op_info: ', trim(error_msg)
        stop 1
    end if
    if (trim(operator) /= '+') then
        print *, 'FAIL: expected top operator +, got ', trim(operator)
        stop 1
    end if
    if (left_index <= 0 .or. right_index <= 0) then
        print *, 'FAIL: operand indices not populated'
        stop 1
    end if

    ! The right operand is the b*c multiplication.
    if (.not. is_binary_op(result%arena, right_index)) then
        print *, 'FAIL: right operand is not a binary op'
        stop 1
    end if
    call get_binary_op_info(result%arena, right_index, operator, nested_left, &
        nested_right, line, column, error_msg)
    if (trim(operator) /= '*') then
        print *, 'FAIL: expected nested operator *, got ', trim(operator)
        stop 1
    end if

    ! A non-binary-op node (a leaf operand) is rejected.
    call get_binary_op_info(result%arena, nested_left, operator, left_index, &
        right_index, line, column, error_msg)
    if (len_trim(error_msg) == 0) then
        print *, 'FAIL: leaf operand wrongly reported as a binary op'
        stop 1
    end if

    print *, 'PASS: compiler binary op query API exposes operator and operands'

contains

    integer function find_binary_op_with_operator(frontend_result, want) &
            result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        character(len=*), intent(in) :: want
        character(len=:), allocatable :: op, msg
        integer :: i, li, ri, ln, col

        node_index = 0
        do i = 1, frontend_result%arena%size
            if (is_binary_op(frontend_result%arena, i)) then
                call get_binary_op_info(frontend_result%arena, i, op, li, ri, &
                    ln, col, msg)
                if (len_trim(msg) == 0 .and. trim(op) == want) then
                    node_index = i
                    return
                end if
            end if
        end do
    end function find_binary_op_with_operator

end program test_compiler_binary_op_queries
