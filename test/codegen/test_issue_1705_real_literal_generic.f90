program test_issue_1705_real_literal_generic
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1705: Real literals preserve precision in generic interfaces ==="

    source = &
        "module math_mod" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    interface add_values" // new_line('a') // &
        "        module procedure add_integers, add_reals" // new_line('a') // &
        "    end interface add_values" // new_line('a') // &
        "contains" // new_line('a') // &
        "    function add_integers(a, b) result(sum)" // new_line('a') // &
        "        integer, intent(in) :: a, b" // new_line('a') // &
        "        integer :: sum" // new_line('a') // &
        "        sum = a + b" // new_line('a') // &
        "    end function add_integers" // new_line('a') // &
        "    function add_reals(a, b) result(sum)" // new_line('a') // &
        "        real, intent(in) :: a, b" // new_line('a') // &
        "        real :: sum" // new_line('a') // &
        "        sum = a + b" // new_line('a') // &
        "    end function add_reals" // new_line('a') // &
        "end module math_mod" // new_line('a') // &
        "" // new_line('a') // &
        "program test" // new_line('a') // &
        "    use math_mod" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    print *, add_values(5, 3)" // new_line('a') // &
        "    print *, add_values(5.0, 3.0)" // new_line('a') // &
        "end program test"

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        ! Real literals should NOT be converted to double precision
        ! because that breaks generic interface resolution
        if (index(output, "5.0d0, 3.0d0") > 0) success = .false.
        ! Function signatures should remain as real, not real(8)
        if (index(output, "real(8) :: a, b") > 0) success = .false.
        ! Real type should be preserved (note: could be real or real(4))
        if (index(output, "real :: a, b") == 0 .and. &
            index(output, "real(4) :: a, b") == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: real literals or types converted to double precision'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

end program test_issue_1705_real_literal_generic
