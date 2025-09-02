program test_array_indexing_brackets
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: ok

    print *, "=== Array indexing with square brackets maps to parentheses ==="

    call test_simple_index()
    call test_multi_index()
    call test_slice_basic()
    call test_slice_stride()
    call test_slice_empty_bounds()

contains

    subroutine test_simple_index()
        print *, "Testing y[1] -> y(1)..."
        source = "program t" // new_line('a') // &
                 "  implicit none" // new_line('a') // &
                 "  integer :: x" // new_line('a') // &
                 "  x = y[1]" // new_line('a') // &
                 "end program t"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR:", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "x = y(1)") > 0
        if (ok) then
            print *, "  PASS"
        else
            print *, "  FAIL: output=", trim(output)
            stop 1
        end if
    end subroutine test_simple_index

    subroutine test_multi_index()
        print *, "Testing a[i, j+1] -> a(i, j+1)..."
        source = "program t" // new_line('a') // &
                 "  implicit none" // new_line('a') // &
                 "  integer :: i, j" // new_line('a') // &
                 "  i = 1" // new_line('a') // &
                 "  j = 2" // new_line('a') // &
                 "  i = a[i, j+1]" // new_line('a') // &
                 "end program t"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR:", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "i = a(i, j+1)") > 0 .or. &
             index(output, "i = a(i,j+1)") > 0 .or. &
             index(output, "i = a(i, j + 1)") > 0
        if (ok) then
            print *, "  PASS"
        else
            print *, "  FAIL: output=", trim(output)
            stop 1
        end if
    end subroutine test_multi_index

    subroutine test_slice_basic()
        print *, "Testing a[2:5] -> a(2:5)..."
        source = "program t" // new_line('a') // &
                 "  implicit none" // new_line('a') // &
                 "  integer :: x" // new_line('a') // &
                 "  x = a[2:5]" // new_line('a') // &
                 "end program t"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR:", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "x = a(2:5)") > 0
        if (ok) then
            print *, "  PASS"
        else
            print *, "  FAIL: output=", trim(output)
            stop 1
        end if
    end subroutine test_slice_basic

    subroutine test_slice_stride()
        print *, "Testing a[1:10:2] -> a(1:10:2)..."
        source = "program t" // new_line('a') // &
                 "  implicit none" // new_line('a') // &
                 "  integer :: x" // new_line('a') // &
                 "  x = a[1:10:2]" // new_line('a') // &
                 "end program t"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR:", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "x = a(1:10:2)") > 0
        if (ok) then
            print *, "  PASS"
        else
            print *, "  FAIL: output=", trim(output)
            stop 1
        end if
    end subroutine test_slice_stride

    subroutine test_slice_empty_bounds()
        print *, "Testing a[:5], a[3:], a[:] -> parentheses equivalents..."
        source = "program t" // new_line('a') // &
                 "  implicit none" // new_line('a') // &
                 "  integer :: x" // new_line('a') // &
                 "  x = a[:5]" // new_line('a') // &
                 "  x = a[3:]" // new_line('a') // &
                 "  x = a[:]" // new_line('a') // &
                 "end program t"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR:", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "x = a(:5)") > 0 .and. &
             index(output, "x = a(3:)") > 0 .and. &
             index(output, "x = a(:)") > 0
        if (ok) then
            print *, "  PASS"
        else
            print *, "  FAIL: output=", trim(output)
            stop 1
        end if
    end subroutine test_slice_empty_bounds


end program test_array_indexing_brackets
