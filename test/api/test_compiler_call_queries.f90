program test_compiler_call_queries
    use fortfront, only: compiler_frontend_options_t, &
                         compiler_frontend_result_t, &
                         compile_frontend_from_string, &
                         get_node_source_location_from_arena, &
                         get_node_type_kind, &
                         get_subroutine_call_arg_indices, &
                         get_subroutine_call_name, &
                         is_subroutine_call_statement, &
                         INPUT_MODE_STANDARD, TINT
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: name
    character(len=:), allocatable :: error_msg
    integer, allocatable :: arg_indices(:)
    integer :: call_index
    integer :: line
    integer :: column
    character(len=*), parameter :: source = &
        'program main'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  call show(5)'//new_line('a')// &
        'contains'//new_line('a')// &
        '  subroutine show(x)'//new_line('a')// &
        '    integer, intent(in) :: x'//new_line('a')// &
        '    print *, x'//new_line('a')// &
        '  end subroutine show'//new_line('a')// &
        'end program main'

    print *, '=== compiler CALL query API test ==='

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, 'FAIL: compiler frontend rejected source: ', &
            trim(result%diagnostic_text)
        stop 1
    end if

    call_index = find_subroutine_call(result)
    if (call_index <= 0) then
        print *, 'FAIL: explicit CALL statement was not exposed'
        stop 1
    end if

    call get_subroutine_call_name(result%arena, call_index, name, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: get_subroutine_call_name: ', trim(error_msg)
        stop 1
    end if
    if (trim(name) /= 'show') then
        print *, 'FAIL: expected callee show, got ', trim(name)
        stop 1
    end if

    call get_subroutine_call_arg_indices(result%arena, call_index, arg_indices, &
                                         error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: get_subroutine_call_arg_indices: ', trim(error_msg)
        stop 1
    end if
    if (size(arg_indices) /= 1) then
        print *, 'FAIL: expected one call argument, got ', size(arg_indices)
        stop 1
    end if

    if (get_node_type_kind(result%arena, arg_indices(1)) /= TINT) then
        print *, 'FAIL: call argument type metadata is not integer'
        stop 1
    end if

    call get_node_source_location_from_arena(result%arena, call_index, line, &
                                             column)
    if (line /= 3 .or. column <= 0) then
        print *, 'FAIL: CALL source location is not available'
        stop 1
    end if

    print *, 'PASS: compiler CALL query API exposes callee, args, type, location'

contains

    integer function find_subroutine_call(frontend_result) result(node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend_result
        integer :: i

        node_index = 0
        do i = 1, frontend_result%arena%size
            if (is_subroutine_call_statement(frontend_result%arena, i)) then
                node_index = i
                return
            end if
        end do
    end function find_subroutine_call

end program test_compiler_call_queries
