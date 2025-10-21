program test_issue_935
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #935: Parameter constants with array dimensions ==='

    if (.not. test_parameter_with_dimension()) all_passed = .false.
    if (.not. test_parameter_in_allocate()) all_passed = .false.
    if (.not. test_multidim_array_with_params()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #935 fixed!'
    else
        print *, 'Issue #935 test failed!'
        stop 1
    end if

contains

    logical function test_parameter_with_dimension()
        character(len=:), allocatable :: source, output, error_msg
        logical :: found_array_dim

        test_parameter_with_dimension = .true.
        print *, 'Testing parameter with dimension attribute...'

        source = 'program test' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer, parameter :: n = 10' // new_line('a') // &
                 '    integer, dimension(n) :: arr' // new_line('a') // &
                 '    arr = 0' // new_line('a') // &
                 'end program test'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_parameter_with_dimension = .false.
                return
            end if
        end if

        found_array_dim = .false.
        if ((index(output, 'integer :: arr(') > 0 .or. &
             index(output, 'integer, dimension(') > 0) .and. &
            index(output, 'arr') > 0) then
            found_array_dim = .true.
        end if

        if (found_array_dim) then
            print *, '  PASS: Array declaration with dimensions preserved'
        else
            print *, '  FAIL: Array dimensions lost in output'
            test_parameter_with_dimension = .false.
        end if

    end function test_parameter_with_dimension

    logical function test_parameter_in_allocate()
        character(len=:), allocatable :: source, output, error_msg
        logical :: found_allocate_with_param

        test_parameter_in_allocate = .true.
        print *, 'Testing parameter in allocate statement...'

        source = 'program test' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer, parameter :: size = 100' // new_line('a') // &
                 '    integer, allocatable :: dyn_arr(:)' // new_line('a') // &
                 '    allocate(dyn_arr(size))' // new_line('a') // &
                 '    dyn_arr = 1' // new_line('a') // &
                 '    deallocate(dyn_arr)' // new_line('a') // &
                 'end program test'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_parameter_in_allocate = .false.
                return
            end if
        end if

        found_allocate_with_param = .false.
        if (contains_without_spaces(output, 'allocate(dyn_arr(size))') .or. &
            contains_without_spaces(output, 'allocate(dyn_arr(100))')) then
            found_allocate_with_param = .true.
        end if

        if (found_allocate_with_param) then
            print *, '  PASS: Parameter in allocate statement preserved'
        else
            print *, '  FAIL: Parameter usage in allocate lost'
            test_parameter_in_allocate = .false.
        end if

    end function test_parameter_in_allocate

    logical function test_multidim_array_with_params()
        character(len=:), allocatable :: source, output, error_msg
        logical :: found_multidim_array

        test_multidim_array_with_params = .true.
        print *, 'Testing multi-dimensional arrays with parameter dimensions...'

        source = 'program test' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer, parameter :: m = 5, n = 10' // new_line('a') // &
                 '    real, dimension(m, n) :: matrix' // new_line('a') // &
                 '    integer, dimension(m, n, 3) :: tensor' // new_line('a') // &
                 '    matrix = 0.0' // new_line('a') // &
                 '    tensor = 0' // new_line('a') // &
                 'end program test'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_multidim_array_with_params = .false.
                return
            end if
        end if

        found_multidim_array = .false.
        if ((index(output, 'matrix(') > 0 .or. index(output, 'dimension(m') > 0) .and. &
            (index(output, 'tensor(') > 0 .or. index(output, 'dimension(m, n, 3)') > 0 .or. &
             index(output, 'dimension(m,n,3)') > 0)) then
            found_multidim_array = .true.
        end if

        if (found_multidim_array) then
            print *, '  PASS: Multi-dimensional array declarations preserved'
        else
            print *, '  FAIL: Multi-dimensional array dimensions lost'
            print *, '  Output:'
            print *, trim(output)
            test_multidim_array_with_params = .false.
        end if

    end function test_multidim_array_with_params

    logical function contains_without_spaces(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compressed
        integer :: i

        compressed = ''
        do i = 1, len_trim(text)
            if (text(i:i) /= ' ') compressed = compressed // text(i:i)
        end do
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

end program test_issue_935
