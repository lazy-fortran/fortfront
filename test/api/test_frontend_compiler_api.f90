program test_frontend_compiler_api
    use fortfront, only: compiler_frontend_result_t, &
                         compile_frontend_from_string, &
                         compile_frontend_from_file, get_node_type_at
    implicit none

    logical :: all_passed

    print *, '=== fortfront Compiler Frontend API Tests ==='
    print *

    all_passed = .true.
    if (.not. test_compile_string()) all_passed = .false.
    if (.not. test_file_diagnostics()) all_passed = .false.
    if (.not. test_compile_file()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All compiler frontend API tests passed!'
        stop 0
    else
        print *, 'Compiler frontend API tests failed!'
        stop 1
    end if

contains

    logical function test_compile_string()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: source = &
                                       'program sample'//new_line('a')// &
                                       '  implicit none'//new_line('a')// &
                                       '  integer :: x'//new_line('a')// &
                                       '  x = 3'//new_line('a')// &
                                       'end program sample'

        test_compile_string = .true.
        print *, 'Testing compile_frontend_from_string...'

        call compile_frontend_from_string(source, result)

        if (.not. result%success()) then
            print *, '  FAIL: unexpected diagnostic -> ', &
                trim(result%diagnostic_text)
            test_compile_string = .false.
            return
        end if

        if (result%root_index <= 0) then
            print *, '  FAIL: root index not set'
            test_compile_string = .false.
            return
        end if

        if (result%arena%size <= 0) then
            print *, '  FAIL: arena is empty'
            test_compile_string = .false.
            return
        end if

        if (.not. allocated(result%tokens)) then
            print *, '  FAIL: tokens were not retained'
            test_compile_string = .false.
            return
        end if

        if (get_node_type_at(result%arena, result%root_index) /= 'program') then
            print *, '  FAIL: root node is not a program'
            test_compile_string = .false.
            return
        end if

        print *, '  PASS: compile_frontend_from_string'
    end function test_compile_string

    logical function test_file_diagnostics()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: file_path = &
                                       'compiler_api_missing_sample.f90'

        test_file_diagnostics = .true.
        print *, 'Testing file diagnostics retention...'

        call cleanup_temp_file(file_path)
        call compile_frontend_from_file(file_path, result)

        if (result%success()) then
            print *, '  FAIL: missing file failure was not reported'
            test_file_diagnostics = .false.
            return
        end if

        if (result%parse_ok) then
            print *, '  FAIL: parse_ok set on missing file'
            test_file_diagnostics = .false.
            return
        end if

        if (len_trim(result%diagnostic_text) == 0) then
            print *, '  FAIL: diagnostic text is empty'
            test_file_diagnostics = .false.
            return
        end if

        if (trim(result%source_path) /= file_path) then
            print *, '  FAIL: source path not retained for missing file'
            test_file_diagnostics = .false.
            return
        end if

        print *, '  PASS: file diagnostics retained'
    end function test_file_diagnostics

    logical function test_compile_file()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: file_path = 'compiler_api_sample.f90'
        integer :: unit
        integer :: io_stat

        test_compile_file = .true.
        print *, 'Testing compile_frontend_from_file...'

        open (newunit=unit, file=file_path, status='replace', action='write', &
              iostat=io_stat)
        if (io_stat /= 0) then
            print *, '  FAIL: unable to create sample file'
            test_compile_file = .false.
            return
        end if
        write (unit, '(A)') 'program file_sample'
        write (unit, '(A)') '  implicit none'
        write (unit, '(A)') '  integer :: x'
        write (unit, '(A)') '  x = 7'
        write (unit, '(A)') 'end program file_sample'
        close (unit)

        call compile_frontend_from_file(file_path, result)

        if (.not. result%success()) then
            print *, '  FAIL: unexpected diagnostic -> ', &
                trim(result%diagnostic_text)
            call cleanup_temp_file(file_path)
            test_compile_file = .false.
            return
        end if

        if (trim(result%source_path) /= file_path) then
            print *, '  FAIL: source path not retained'
            call cleanup_temp_file(file_path)
            test_compile_file = .false.
            return
        end if

        print *, '  PASS: compile_frontend_from_file'
        call cleanup_temp_file(file_path)
    end function test_compile_file

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

end program test_frontend_compiler_api
