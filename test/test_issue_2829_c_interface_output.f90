program test_issue_2829_c_interface_output
    ! Issue #2829 (c): fortfront_parse_source no longer discards the transformed
    ! output; C callers retrieve it via fortfront_get_output.
    use iso_c_binding, only: c_int, c_char, c_ptr, c_null_char, c_associated, &
                             c_f_pointer, c_loc
    use fortfront_c_interface, only: fortfront_initialize_c, fortfront_cleanup_c, &
                                     fortfront_parse_source_c, fortfront_get_output_c
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_output_returned()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", &
        test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_output_returned()
        character(len=*, kind=c_char), parameter :: src = "x = 5"//c_null_char
        character(len=:, kind=c_char), allocatable, target :: src_buf
        integer(c_int) :: status
        type(c_ptr) :: out_ptr
        character(len=:), allocatable :: output
        logical :: ok

        test_count = test_count + 1

        src_buf = src
        status = fortfront_initialize_c()

        status = fortfront_parse_source_c(c_loc(src_buf), len(src_buf, c_int))
        ok = status == 0_c_int

        if (ok) then
            out_ptr = fortfront_get_output_c()
            ok = c_associated(out_ptr)
        end if

        if (ok) then
            output = c_string_to_fortran(out_ptr)
            ok = index(output, "x") > 0 .and. len_trim(output) > 0
        end if

        call fortfront_cleanup_c()

        if (ok) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: transformed output returned to C caller"
            write (*, '(A)') "Output was:"
            write (*, '(A)') output
        else
            write (*, '(A)') "FAIL: transformed output not returned"
        end if
    end subroutine test_output_returned

    function c_string_to_fortran(ptr) result(str)
        type(c_ptr), intent(in) :: ptr
        character(len=:), allocatable :: str
        character(len=1, kind=c_char), pointer :: chars(:)
        integer :: n, i

        ! Find length up to a generous cap, stopping at the null terminator.
        call c_f_pointer(ptr, chars, [4096])
        n = 0
        do i = 1, 4096
            if (chars(i) == c_null_char) exit
            n = n + 1
        end do

        allocate (character(len=n) :: str)
        do i = 1, n
            str(i:i) = chars(i)
        end do
    end function c_string_to_fortran

end program test_issue_2829_c_interface_output
