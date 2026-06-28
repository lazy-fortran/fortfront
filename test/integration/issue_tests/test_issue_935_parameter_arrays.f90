program test_issue_935
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_parameter_with_dimension()) all_passed = .false.
    if (.not. test_parameter_in_allocate()) all_passed = .false.
    if (.not. test_multidim_array_with_params()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #935 - parameter dimensions preserved'
    else
        error stop 'FAIL: Issue #935 regression detected'
    end if

contains

    include '../../common/read_example.inc'


    logical function test_parameter_with_dimension()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: found_array_dim

        test_parameter_with_dimension = .true.

        call read_example('examples/lf/issue_935_param_dimension.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Compilation error: ' // trim(error_msg)
                test_parameter_with_dimension = .false.
                return
            end if
        end if

        found_array_dim = (index(output, 'integer, dimension(n) :: arr') > 0) .or. &
            (index(output, 'integer :: arr(') > 0)

        if (.not. found_array_dim) then
            write (error_unit, '(A)') 'FAIL: Array dimensions lost in output'
            write (error_unit, '(A)') trim(output)
            test_parameter_with_dimension = .false.
        end if
    end function test_parameter_with_dimension

    logical function test_parameter_in_allocate()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_parameter_in_allocate = .true.

        call read_example('examples/lf/issue_935_allocate_param.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Compilation error: ' // trim(error_msg)
                test_parameter_in_allocate = .false.
                return
            end if
        end if

        if (.not. contains_without_spaces(output, 'allocate(dyn_arr(size))')) then
            write (error_unit, '(A)') 'FAIL: Parameter usage in allocate lost'
            write (error_unit, '(A)') trim(output)
            test_parameter_in_allocate = .false.
        end if
    end function test_parameter_in_allocate

    logical function test_multidim_array_with_params()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_matrix
        logical :: has_tensor

        test_multidim_array_with_params = .true.

        call read_example('examples/lf/issue_935_multidim_arrays.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Compilation error: ' // trim(error_msg)
                test_multidim_array_with_params = .false.
                return
            end if
        end if

        has_matrix = index(output, 'real, dimension(m, n) :: matrix') > 0 .or. &
            index(output, 'real :: matrix(m,n)') > 0 .or. &
            index(output, 'real :: matrix(m, n)') > 0 .or. &
            index(output, 'real(8) :: matrix(m,n)') > 0 .or. &
            index(output, 'real(8) :: matrix(m, n)') > 0 .or. &
            index(output, 'real(dp) :: matrix(m,n)') > 0 .or. &
            index(output, 'real(dp) :: matrix(m, n)') > 0
        has_tensor = index(output, 'integer, dimension(m, n, 3) :: tensor') > 0 .or. &
            index(output, 'integer :: tensor(m,n,3)') > 0 .or. &
            index(output, 'integer :: tensor(m, n, 3)') > 0

        if (.not. (has_matrix .and. has_tensor)) then
            write (error_unit, '(A)') 'FAIL: Multi-dimensional array dimensions lost'
            write (error_unit, '(A)') trim(output)
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
