program test_compiler_literal_queries
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, &
        compile_frontend_from_string, &
        is_literal, get_literal_info, &
        is_binary_op, get_binary_op_info, &
        INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: value, literal_type, error_msg
    integer :: lit_index
    character(len=*), parameter :: source = &
        'program main'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  integer :: a'//new_line('a')// &
        '  a = 42'//new_line('a')// &
        'end program main'

    print *, '=== compiler literal query API test ==='

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, 'FAIL: compiler frontend rejected source: ', &
            trim(result%diagnostic_text)
        stop 1
    end if

    lit_index = find_literal(result)
    if (lit_index <= 0) then
        print *, 'FAIL: no literal exposed'
        stop 1
    end if

    call get_literal_info(result%arena, lit_index, value, literal_type, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: get_literal_info: ', trim(error_msg)
        stop 1
    end if
    if (trim(value) /= '42') then
        print *, 'FAIL: expected literal value 42, got ', trim(value)
        stop 1
    end if

    ! A non-literal node is rejected. The program node at index 1 is not a literal.
    if (is_literal(result%arena, 1) .and. lit_index == 1) then
        print *, 'FAIL: sanity check failed'
        stop 1
    end if
    call get_literal_info(result%arena, 1, value, literal_type, error_msg)
    if (is_literal(result%arena, 1) .eqv. (len_trim(error_msg) == 0)) then
        continue
    else
        print *, 'FAIL: is_literal and get_literal_info disagree at node 1'
        stop 1
    end if

    print *, 'PASS: compiler literal query API exposes literal value and type'

contains

    integer function find_literal(frontend_result) result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        integer :: i

        node_index = 0
        do i = 1, frontend_result%arena%size
            if (is_literal(frontend_result%arena, i)) then
                node_index = i
                return
            end if
        end do
    end function find_literal

end program test_compiler_literal_queries
