program test_issue_2818_bind_c_roundtrip
    ! Parse and round-trip a valid interoperable BIND(C) module. The
    ! interoperability validation pass must accept it (no errors) and codegen
    ! must preserve the BIND(C) attribute on both the derived types and the
    ! procedure. See issue #2818 and ISO/IEC 1539-1:2004 Section 15.3.2.
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code, output_code, error_msg
    character(len=:), allocatable :: lowered

    call read_example('examples/f90/issue_2818_bind_c_interop.f90', input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) /= 0) then
        print *, 'FAIL: valid BIND(C) module reported errors: ', trim(error_msg)
        error stop 1
    end if

    lowered = to_lower(output_code)

    if (index(lowered, 'bind(c)') == 0) then
        print *, 'FAIL: BIND(C) attribute not preserved in output'
        print *, 'Output:', output_code
        error stop 1
    end if

    if (index(lowered, 'particle_t') == 0) then
        print *, 'FAIL: particle_t type not present in output'
        error stop 1
    end if

    print *, 'PASS: valid BIND(C) interoperable module round-trips cleanly'

contains

    include 'common/read_example.inc'

    pure function to_lower(text) result(lower_text)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower_text
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lower_text(i:i) = achar(code + 32)
            else
                lower_text(i:i) = text(i:i)
            end if
        end do
    end function to_lower

end program test_issue_2818_bind_c_roundtrip
