program test_do_concurrent_type_spec_issue_2849
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "Testing DO CONCURRENT type-spec index form (Issue #2849)"

    ! Test 1: Basic integer type-spec
    call read_example('examples/f90/do_concurrent_type_spec.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: parse failed for basic type-spec form'
            print *, 'Error: ', trim(error_msg)
            print *, 'Source:'
            print *, trim(source)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'integer :: i') == 0) then
        print *, 'ERROR: type-spec "integer :: i" missing from DO CONCURRENT output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    ! Test 2: Real type-spec with kind parameter
    source = "program p2"&
         // new_line('a') &
         // "  integer :: n = 10"&
         // new_line('a') &
         // "  real(8) :: arr(10)"&
         // new_line('a') &
         // "  do concurrent (real(8) :: x = 0.0d0:1.0d0:0.1d0)"&
         // new_line('a') &
         // "    arr(1) = x"&
         // new_line('a') &
         // "  end do"&
         // new_line('a') &
         // "end program p2"
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: parse failed for real(8) type-spec form'
            print *, 'Error: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT missing from real(8) output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'real(8)') == 0) then
        print *, 'ERROR: type-spec "real(8)" missing from DO CONCURRENT output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    ! Test 3: Multi-index with type-spec on inner loop
    source = "program p3"&
         // new_line('a') &
         // "  integer :: n = 5, m = 3"&
         // new_line('a') &
         // "  do concurrent (i = 1:n, integer :: j = 1:m)"&
         // new_line('a') &
         // "  end do"&
         // new_line('a') &
         // "end program p3"
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: parse failed for mixed type-spec form'
            print *, 'Error: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT missing from mixed type-spec output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    ! Test 4: Logical type-spec
    source = "program p4"&
         // new_line('a') &
         // "  do concurrent (logical :: f = .true.:.false.)"&
         // new_line('a') &
         // "  end do"&
         // new_line('a') &
         // "end program p4"
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: parse failed for logical type-spec form'
            print *, 'Error: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT missing from logical type-spec output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: DO CONCURRENT type-spec index form parsed correctly'
    stop 0

contains

    include '../common/read_example.inc'
end program test_do_concurrent_type_spec_issue_2849
