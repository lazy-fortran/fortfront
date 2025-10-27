program test_issue_1968_lazy_function
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_loop_var_decl
    logical :: has_function_name_decl
    logical :: has_result_assignment
    logical :: has_return_type

    print *, "=== Issue #1968: lazy function result variable handling ==="

    call read_example('examples/lf/issue_1968_lazy_function_result.lf', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error"
        print *, trim(error_msg)
        error stop 1
    end if

    has_return_type = index(output_code, "real function array_sum") > 0
    if (.not. has_return_type) then
        print *, "FAIL: missing explicit return type for array_sum"
        print *, trim(output_code)
        error stop 1
    end if

    has_function_name_decl = index(output_code, "real :: array_sum") > 0
    if (has_function_name_decl) then
        print *, "FAIL: function name declared as local variable"
        print *, trim(output_code)
        error stop 1
    end if

    has_loop_var_decl = index(output_code, "integer :: i") > 0
    if (.not. has_loop_var_decl) then
        print *, "FAIL: missing loop variable declaration for i"
        print *, trim(output_code)
        error stop 1
    end if

    has_result_assignment = index(output_code, "array_sum = total") > 0
    if (.not. has_result_assignment) then
        print *, "FAIL: expected assignment to function result not found"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: lazy function result variable generated correctly"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, iostat, file_size
        character(len=1), allocatable :: buffer(:)
        integer :: i

        open (newunit=unit, file=filepath, status='old', action='read', &
              access='stream', iostat=iostat)
        if (iostat /= 0) then
            print *, "FAIL: Could not open file: ", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=iostat) buffer
        close (unit)

        if (iostat /= 0) then
            print *, "FAIL: Could not read file: ", trim(filepath)
            error stop 1
        end if

        allocate (character(len=file_size) :: content)
        do i = 1, file_size
            content(i:i) = buffer(i)
        end do
    end subroutine read_example

end program test_issue_1968_lazy_function
