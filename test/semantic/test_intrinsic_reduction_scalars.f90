program test_intrinsic_reduction_scalars
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_1961_array_reduction_intrinsics.lf', &
                      input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) /= 0) then
        print *, "Lazy Fortran transform failed unexpectedly"
        print *, trim(error_msg)
        stop 1
    end if

    call ensure_scalar_decl(output_code, 'real', 'total')
    call ensure_scalar_decl(output_code, 'real', 'high')
    call ensure_scalar_decl(output_code, 'real', 'low')
    call ensure_scalar_decl(output_code, 'real', 'prod')
    call ensure_scalar_decl(output_code, 'logical', 'has_true')
    call ensure_scalar_decl(output_code, 'integer', 'true_count')

    call ensure_absent(output_code, "allocatable :: total")
    call ensure_absent(output_code, ":: total(:")
    call ensure_absent(output_code, "allocatable :: high")
    call ensure_absent(output_code, ":: high(:")
    call ensure_absent(output_code, "allocatable :: low")
    call ensure_absent(output_code, ":: low(:")
    call ensure_absent(output_code, "allocatable :: prod")
    call ensure_absent(output_code, ":: prod(:")
    call ensure_absent(output_code, "allocatable :: has_true")
    call ensure_absent(output_code, "allocatable :: true_count")

contains

    include '../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(a)') 'FAIL: failed to load example'
            stop 1
        end if
    end subroutine read_example

    subroutine ensure_contains(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern

        if (index(text, pattern) == 0) then
            print *, "Expected substring missing:", trim(pattern)
            stop 1
        end if
    end subroutine ensure_contains

    subroutine ensure_absent(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern

        if (index(text, pattern) /= 0) then
            print *, "Unexpected substring present:", trim(pattern)
            stop 1
        end if
    end subroutine ensure_absent

    subroutine ensure_scalar_decl(text, decl_type, name)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: decl_type
        character(len=*), intent(in) :: name

        if (.not. has_scalar_declaration(text, decl_type, name)) then
            print *, 'Expected scalar declaration missing:', &
                trim(decl_type), trim(name)
            stop 1
        end if
    end subroutine ensure_scalar_decl

    logical function has_scalar_declaration(text, decl_type, name)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: decl_type
        character(len=*), intent(in) :: name
        integer :: pos
        integer :: start_pos
        integer :: end_pos
        integer :: text_len
        character(len=:), allocatable :: line
        character(len=:), allocatable :: tail
        character(len=64) :: token
        integer :: sep
        character(1), parameter :: nl = new_line('a')

        has_scalar_declaration = .false.
        text_len = len(text)
        pos = index(text, trim(decl_type)//' ::')

        do while (pos > 0)
            start_pos = pos
            do while (start_pos > 1 .and. text(start_pos - 1:start_pos - 1) /= nl)
                start_pos = start_pos - 1
            end do

            end_pos = pos
            do while (end_pos <= text_len .and. text(end_pos:end_pos) /= nl)
                end_pos = end_pos + 1
            end do

            if (end_pos > text_len) then
                line = text(start_pos:)
            else
                line = text(start_pos:end_pos - 1)
            end if

            line = adjustl(line)
            if (index(line, trim(decl_type)//' ::') == 1 .or. &
                index(line, trim(decl_type)//'(') == 1) then
                sep = index(line, '::')
                if (sep > 0) then
                    tail = adjustl(line(sep + 2:))
                    do
                        if (len_trim(tail) == 0) exit
                        sep = index(tail, ',')
                        if (sep == 0) then
                            token = trim(tail)
                            tail = ''
                        else
                            token = trim(tail(1:sep - 1))
                            tail = adjustl(tail(sep + 1:))
                        end if

                        if (len_trim(token) == 0) cycle
                        if (trim(token) == trim(name)) then
                            has_scalar_declaration = .true.
                            return
                        end if
                    end do
                end if
            end if

            if (end_pos > text_len) exit
            pos = index(text(end_pos:), trim(decl_type)//' ::')
            if (pos > 0) pos = pos + end_pos - 1
        end do
    end function has_scalar_declaration

end program test_intrinsic_reduction_scalars
