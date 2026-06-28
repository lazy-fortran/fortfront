program test_compiler_identifier_queries
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, &
        compile_frontend_from_string, &
        is_identifier, get_identifier_name, &
        is_literal, &
        INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: result_options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: name, error_msg
    integer :: id_index
    character(len=*), parameter :: source = &
        'program main'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  integer :: alpha'//new_line('a')// &
        '  alpha = 1'//new_line('a')// &
        '  alpha = alpha + 1'//new_line('a')// &
        'end program main'

    print *, '=== compiler identifier query API test ==='

    result_options%input_mode = INPUT_MODE_STANDARD
    result_options%run_semantics = .true.
    call compile_frontend_from_string(source, result, result_options)
    if (.not. result%success()) then
        print *, 'FAIL: compiler frontend rejected source: ', &
            trim(result%diagnostic_text)
        stop 1
    end if

    id_index = find_identifier_named(result, 'alpha')
    if (id_index <= 0) then
        print *, 'FAIL: no identifier named alpha exposed'
        stop 1
    end if

    call get_identifier_name(result%arena, id_index, name, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: get_identifier_name: ', trim(error_msg)
        stop 1
    end if
    if (trim(name) /= 'alpha') then
        print *, 'FAIL: expected identifier alpha, got ', trim(name)
        stop 1
    end if

    ! A literal is not an identifier; the query rejects it.
    if (is_identifier(result%arena, find_first_literal(result))) then
        print *, 'FAIL: a literal was reported as an identifier'
        stop 1
    end if

    print *, 'PASS: compiler identifier query API exposes identifier name'

contains

    integer function find_identifier_named(frontend_result, want) &
            result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        character(len=*), intent(in) :: want
        character(len=:), allocatable :: nm, msg
        integer :: i

        node_index = 0
        do i = 1, frontend_result%arena%size
            if (is_identifier(frontend_result%arena, i)) then
                call get_identifier_name(frontend_result%arena, i, nm, msg)
                if (len_trim(msg) == 0 .and. trim(nm) == want) then
                    node_index = i
                    return
                end if
            end if
        end do
    end function find_identifier_named

    integer function find_first_literal(frontend_result) result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        integer :: i

        node_index = 0
        do i = 1, frontend_result%arena%size
            if (is_literal(frontend_result%arena, i)) then
                node_index = i
                return
            end if
        end do
    end function find_first_literal

end program test_compiler_identifier_queries
