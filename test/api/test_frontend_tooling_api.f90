program test_frontend_tooling_api
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        tooling_load_ast_from_file, ast_arena_t, token_t, &
        get_node_type_at, ast_to_json
    implicit none

    logical :: all_passed

    print *, '=== fortfront Tooling Lightweight API Tests ==='
    print *

    all_passed = .true.
    if (.not. test_parse_string()) all_passed = .false.
    if (.not. test_parse_file()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All tooling lightweight API tests passed!'
        stop 0
    else
        print *, 'Tooling lightweight API tests failed!'
        stop 1
    end if

contains

    logical function test_parse_string()
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: json_output
        integer :: root_index
        integer :: start_clock
        integer :: end_clock
        integer :: clock_rate
        real(dp) :: elapsed_seconds
        character(len=*), parameter :: source = &
            'program sample' // new_line('a') // &
            '  implicit none' // new_line('a') // &
            '  integer :: x' // new_line('a') // &
            '  x = 3' // new_line('a') // &
            '  print *, "a'//achar(92)//'b"' // new_line('a') // &
            'end program sample'

        test_parse_string = .true.
        print *, 'Testing tooling_load_ast_from_string...'

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call system_clock(start_clock, clock_rate)
        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options, tokens)
        call system_clock(end_clock)

        if (has_error(error_msg)) then
            print *, '  FAIL: unexpected error message ->', trim(error_msg)
            test_parse_string = .false.
            return
        end if

        if (root_index <= 0) then
            print *, '  FAIL: root index not set'
            test_parse_string = .false.
            return
        end if

        if (arena%size <= 0) then
            print *, '  FAIL: arena is empty'
            test_parse_string = .false.
            return
        end if

        if (.not. allocated(tokens)) then
            print *, '  FAIL: tokens not returned'
            test_parse_string = .false.
            return
        end if

        print *, '  INFO: tokens captured =', size(tokens)
        print *, '  INFO: arena nodes     =', arena%size
        call ast_to_json(arena, root_index, json_output)
        print *, '  INFO: JSON snapshot   =', trim(json_output)
        if (index(json_output, 'a'//achar(92)//achar(92)//'b') == 0) then
            print *, '  FAIL: JSON did not escape the backslash'
            test_parse_string = .false.
            return
        end if

        if (clock_rate > 0) then
            elapsed_seconds = real(end_clock - start_clock, dp) / &
                real(clock_rate, dp)
            print *, '  INFO: string parse time (s) =', elapsed_seconds
        end if

        if (get_node_type_at(arena, root_index) /= 'program') then
            print *, '  FAIL: root node is not a program'
            test_parse_string = .false.
            return
        end if

        print *, '  PASS: tooling_load_ast_from_string'
    end function test_parse_string

    logical function test_parse_file()
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        integer :: root_index
        integer :: start_clock
        integer :: end_clock
        integer :: clock_rate
        real(dp) :: elapsed_seconds
        character(len=*), parameter :: file_path = 'tooling_api_sample.f90'
        integer :: unit
        integer :: io_stat

        test_parse_file = .true.
        print *, 'Testing tooling_load_ast_from_file...'

        open (newunit=unit, file=file_path, status='replace', action='write', &
            iostat=io_stat)
        if (io_stat /= 0) then
            print *, '  FAIL: unable to create sample file'
            test_parse_file = .false.
            return
        end if
        write (unit, '(A)') 'program tooling_sample'
        write (unit, '(A)') '  implicit none'
        write (unit, '(A)') '  print *, 7'
        write (unit, '(A)') 'end program tooling_sample'
        close (unit)

        options = tooling_parse_options_t()
        call system_clock(start_clock, clock_rate)
        call tooling_load_ast_from_file(file_path, arena, root_index, error_msg, &
            options)
        call system_clock(end_clock)

        if (clock_rate > 0) then
            elapsed_seconds = real(end_clock - start_clock, dp) / &
                real(clock_rate, dp)
            print *, '  INFO: file parse time (s) =', elapsed_seconds
        end if

        if (has_error(error_msg)) then
            print *, '  FAIL: unexpected error message ->', trim(error_msg)
            call cleanup_temp_file(file_path)
            test_parse_file = .false.
            return
        end if

        if (root_index <= 0) then
            print *, '  FAIL: root index not set for file parse'
            call cleanup_temp_file(file_path)
            test_parse_file = .false.
            return
        end if

        if (get_node_type_at(arena, root_index) /= 'program') then
            print *, '  FAIL: root node from file is not a program'
            call cleanup_temp_file(file_path)
            test_parse_file = .false.
            return
        end if

        print *, '  PASS: tooling_load_ast_from_file'
        call cleanup_temp_file(file_path)
    end function test_parse_file

    subroutine cleanup_temp_file(path)
        character(len=*), intent(in) :: path
        integer :: unit
        integer :: stat
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return

        open (newunit=unit, file=path, status='old', action='write', iostat=stat)
        if (stat == 0) then
            close (unit, status='delete')
        end if
    end subroutine cleanup_temp_file

    logical function has_error(message)
        character(len=:), allocatable, intent(in) :: message

        if (.not. allocated(message)) then
            has_error = .false.
        else
            has_error = len_trim(message) > 0
        end if
    end function has_error

end program test_frontend_tooling_api
