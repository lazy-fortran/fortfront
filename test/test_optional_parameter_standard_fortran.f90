program test_optional_parameter_standard_fortran
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed

    test_passed = .true.

    ! Test standard Fortran with OPTIONAL parameter should pass through unchanged
    call test_optional_preserved()

    if (test_passed) then
        print *, "test_optional_parameter_standard_fortran PASSED"
    else
        print *, "test_optional_parameter_standard_fortran FAILED"
        error stop 1
    end if

contains

    subroutine test_optional_preserved()
        call read_example('examples/f90/issue_2015_optional_param_wrong_monomorph.f90', &
                         input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        ! Should NOT create a module (standard Fortran should pass through)
        if (index(output, 'module auto_greet') > 0) then
            print *, "ERROR: Created monomorphized module for standard Fortran"
            test_passed = .false.
            return
        end if

        ! Should NOT create duplicate procedures
        if (index(output, 'greet__ch') > 0) then
            print *, "ERROR: Created monomorphized procedure variants"
            test_passed = .false.
            return
        end if

        ! Should preserve OPTIONAL attribute
        if (index(output, 'optional') == 0) then
            print *, "ERROR: Lost OPTIONAL attribute"
            test_passed = .false.
            return
        end if

        ! Should preserve the subroutine in contains
        if (index(output, 'subroutine greet') == 0) then
            print *, "ERROR: Lost original subroutine"
            test_passed = .false.
            return
        end if

        print *, "  - OPTIONAL parameter preserved correctly"
    end subroutine test_optional_preserved

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, stat, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) then
            print *, "ERROR: Cannot open file:", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (character(len=file_size) :: content)
        allocate (buffer(file_size))

        read (unit, iostat=stat) buffer
        close (unit)

        if (stat /= 0) then
            print *, "ERROR: Cannot read file:", trim(filepath)
            error stop 1
        end if

        content = transfer(buffer, content)
        deallocate (buffer)
    end subroutine read_example

end program test_optional_parameter_standard_fortran
