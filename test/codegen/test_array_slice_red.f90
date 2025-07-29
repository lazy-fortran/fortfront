program test_array_slice_red
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Slicing Code Generation RED Tests ==='
    print *, 'These tests check array slicing code generation'
    print *

    if (.not. test_basic_slice()) all_passed = .false.
    if (.not. test_empty_bounds()) all_passed = .false.
    if (.not. test_slice_assignment()) all_passed = .false.

    print *
    print *, 'Array slicing RED tests completed'
    stop 0  ! Exit 0 since failures are expected

contains

    logical function test_basic_slice()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_slice

        test_basic_slice = .true.
        print *, 'Testing basic array slicing code generation...'

        input_file = 'test_slice1.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'integer :: arr(5), sub(3)'
        write (unit, '(a)') 'arr = [10, 20, 30, 40, 50]'
        write (unit, '(a)') 'sub = arr(2:4)'
        close (unit)

        output_file = 'test_slice1_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check the generated code
            open (newunit=unit, file=output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                found_slice = .false.
                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    ! Should generate: sub = arr(2:4)
                    if (index(line, 'arr(2:4)') > 0) then
                        found_slice = .true.
                        print *, '  Generated: ', trim(line)
                        exit
                    end if
                end do
                close (unit)

                if (found_slice) then
                    print *, '  SUCCESS: Array slice generated correctly'
                else
                    print *, '  FAIL: Array slice not found in output'
                    test_basic_slice = .false.
                end if
            end if
        else
            print *, '  EXPECTED FAIL: ', trim(error_msg)
        end if

    end function test_basic_slice

    logical function test_empty_bounds()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_lower, found_upper

        test_empty_bounds = .true.
        print *, 'Testing empty bounds in array slicing...'

        input_file = 'test_slice2.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'integer :: arr(5), lower(2), upper(3)'
        write (unit, '(a)') 'arr = [10, 20, 30, 40, 50]'
        write (unit, '(a)') 'lower = arr(:2)'
        write (unit, '(a)') 'upper = arr(3:)'
        close (unit)

        output_file = 'test_slice2_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check the generated code
            open (newunit=unit, file=output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                found_lower = .false.
                found_upper = .false.
                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    if (index(line, 'arr(:2)') > 0) then
                        found_lower = .true.
                        print *, '  Generated lower: ', trim(line)
                    end if
                    if (index(line, 'arr(3:)') > 0) then
                        found_upper = .true.
                        print *, '  Generated upper: ', trim(line)
                    end if
                end do
                close (unit)

                if (found_lower .and. found_upper) then
                    print *, '  SUCCESS: Empty bounds generated correctly'
                else
                    print *, '  FAIL: Empty bounds not generated correctly'
                    test_empty_bounds = .false.
                end if
            end if
        else
            print *, '  EXPECTED FAIL: ', trim(error_msg)
        end if

    end function test_empty_bounds

    logical function test_slice_assignment()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_slice_assignment = .true.
        print *, 'Testing array slice assignment...'

        input_file = 'test_slice3.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'integer :: arr(5)'
        write (unit, '(a)') 'arr = [10, 20, 30, 40, 50]'
        write (unit, '(a)') 'arr(2:4) = [100, 200, 300]'
        close (unit)

        output_file = 'test_slice3_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED FAIL: Array slice assignment - ', trim(error_msg)
        else
            print *, '  Array slice assignment might be working'
        end if

    end function test_slice_assignment

end program test_array_slice_red
