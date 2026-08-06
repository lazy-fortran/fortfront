program test_path_validation
    use path_validation, only: validate_file_path, validate_input_path, &
        path_validation_result_t, PATH_VALID, PATH_INVALID_CHARACTERS, &
        PATH_INVALID_TRAVERSAL, PATH_INVALID_ABSOLUTE
    implicit none

    character(len=1), parameter :: WINDOWS_SEPARATOR = achar(92)

    call test_posix_glob_chars_allowed()
    call test_windows_like_disallows_glob_chars()
    call test_traversal_rejected()
    call test_windows_absolute_paths_rejected()
    call test_angle_brackets_rejected()
    call test_square_brackets_rejected()
    call test_posix_glob_chars_in_subdir()

    print *, 'Path validation tests passed'

contains

    subroutine test_posix_glob_chars_allowed()
        type(path_validation_result_t) :: res
        res = validate_input_path('data*?.txt')
        if (res%code /= PATH_VALID) then
            print *, 'Expected PATH_VALID for POSIX-like filename with * and ?'
            stop 1
        end if
    end subroutine test_posix_glob_chars_allowed

    subroutine test_windows_like_disallows_glob_chars()
        type(path_validation_result_t) :: res
        ! Heuristically Windows-like path should reject '?'
        res = validate_input_path('C:'//WINDOWS_SEPARATOR//'temp'// &
            WINDOWS_SEPARATOR//'file?.txt')
        if (res%code /= PATH_INVALID_CHARACTERS) then
            print *, 'Expected PATH_INVALID_CHARACTERS for Windows-like path with ?'
            stop 1
        end if
    end subroutine test_windows_like_disallows_glob_chars

    subroutine test_traversal_rejected()
        type(path_validation_result_t) :: res
        res = validate_input_path('../etc/passwd')
        if (res%code /= PATH_INVALID_TRAVERSAL) then
            print *, 'Expected PATH_INVALID_TRAVERSAL for ../ sequence'
            stop 1
        end if
        res = validate_input_path('..'//WINDOWS_SEPARATOR//'secret.f90')
        if (res%code /= PATH_INVALID_TRAVERSAL) then
            print *, 'Expected PATH_INVALID_TRAVERSAL for Windows traversal'
            stop 1
        end if
    end subroutine test_traversal_rejected

    subroutine test_windows_absolute_paths_rejected()
        type(path_validation_result_t) :: res

        res = validate_file_path('C:'//WINDOWS_SEPARATOR//'temp'// &
            WINDOWS_SEPARATOR//'file.txt')
        if (res%code /= PATH_INVALID_ABSOLUTE) then
            print *, 'Expected PATH_INVALID_ABSOLUTE for Windows drive path'
            stop 1
        end if

        res = validate_file_path(WINDOWS_SEPARATOR//WINDOWS_SEPARATOR// &
            'server'//WINDOWS_SEPARATOR//'share'//WINDOWS_SEPARATOR//'file.txt')
        if (res%code /= PATH_INVALID_ABSOLUTE) then
            print *, 'Expected PATH_INVALID_ABSOLUTE for Windows UNC path'
            stop 1
        end if
    end subroutine test_windows_absolute_paths_rejected

    subroutine test_angle_brackets_rejected()
        type(path_validation_result_t) :: res
        res = validate_input_path('data<.txt')
        if (res%code /= PATH_INVALID_CHARACTERS) then
            print *, 'Expected PATH_INVALID_CHARACTERS for angle bracket in filename'
            stop 1
        end if
    end subroutine test_angle_brackets_rejected

    subroutine test_square_brackets_rejected()
        type(path_validation_result_t) :: res
        res = validate_input_path('data[1].txt')
        if (res%code /= PATH_INVALID_CHARACTERS) then
            print *, 'Expected PATH_INVALID_CHARACTERS for square brackets in filename'
            stop 1
        end if
    end subroutine test_square_brackets_rejected

    subroutine test_posix_glob_chars_in_subdir()
        type(path_validation_result_t) :: res
        res = validate_input_path('subdir/data*?.txt')
        if (res%code /= PATH_VALID) then
            print *, 'Expected PATH_VALID for POSIX subdir path with * and ?'
            stop 1
        end if
    end subroutine test_posix_glob_chars_in_subdir

end program test_path_validation
