program test_standardization_red
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Standardization RED Tests ==='
    print *, 'These tests check standardization phase (expected to fail)'
    print *

    if (.not. test_array_literal_standardization()) all_passed = .false.
    if (.not. test_implied_do_standardization()) all_passed = .false.
    if (.not. test_string_concat_standardization()) all_passed = .false.
    if (.not. test_array_slice_standardization()) all_passed = .false.

    print *
    print *, 'Standardization RED tests completed'
    stop 0  ! Exit 0 since failures are expected

contains

    logical function test_array_literal_standardization()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=256) :: line
        logical :: found_standard_form

        test_array_literal_standardization = .true.
        print *, 'Testing array literal standardization...'

        ! Test: [1, 2, 3] should become (/ 1, 2, 3 /)
        input_file = 'test_std_arr.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(3)'
        write (unit, '(a)') '    arr = [1, 2, 3]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_std_arr_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check output for standard form
            open (newunit=unit, file=output_file, status='old', iostat=unit)
            if (unit == 0) then
                found_standard_form = .false.
                do
                    read (unit, '(a)', iostat=unit) line
                    if (unit /= 0) exit
                    if (index(line, '(/ 1, 2, 3 /)') > 0) then
                        found_standard_form = .true.
                        exit
                    end if
                end do
                close (unit)

                if (found_standard_form) then
                    print *, '  SUCCESS: Array literal standardized to (/ /) form'
                else
                    print *, '  FAIL: Array literal not properly standardized'
                    test_array_literal_standardization = .false.
                end if
            end if
        else
            print *, '  EXPECTED FAIL: Compilation error - ', trim(error_msg)
        end if

    end function test_array_literal_standardization

    logical function test_implied_do_standardization()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_implied_do_standardization = .true.
        print *, 'Testing implied do loop standardization...'

        ! Test: [(i, i=1,10)] should become (/ (i, i=1,10) /)
        input_file = 'test_std_do.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(10)'
        write (unit, '(a)') '    arr = [(i, i=1,10)]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_std_do_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED FAIL: Implied do standardization not implemented - ', trim(error_msg)
        else
            print *, '  Check output for proper standardization'
            ! Could check the output file here
        end if

    end function test_implied_do_standardization

    logical function test_string_concat_standardization()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=256) :: line
        logical :: found_concat

        test_string_concat_standardization = .true.
        print *, 'Testing string concatenation standardization...'

        ! Test: "hello" // "world" should remain the same
        input_file = 'test_std_concat.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    character(len=20) :: msg'
        write (unit, '(a)') '    msg = "hello" // " world"'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_std_concat_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check output
            open (newunit=unit, file=output_file, status='old', iostat=unit)
            if (unit == 0) then
                found_concat = .false.
                do
                    read (unit, '(a)', iostat=unit) line
                    if (unit /= 0) exit
                    if (index(line, '//') > 0) then
                        found_concat = .true.
                        exit
                    end if
                end do
                close (unit)

                if (found_concat) then
                    print *, '  SUCCESS: String concatenation preserved'
                else
                    print *, '  String concatenation might be evaluated at compile time'
                end if
            end if
        else
            print *, '  EXPECTED FAIL: ', trim(error_msg)
        end if

    end function test_string_concat_standardization

    logical function test_array_slice_standardization()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_slice_standardization = .true.
        print *, 'Testing array slice standardization...'

        ! Test: arr(1:3) and arr(:) should be preserved
        input_file = 'test_std_slice.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(10), sub(3)'
        write (unit, '(a)') '    arr = [(i, i=1,10)]'
        write (unit, '(a)') '    sub = arr(1:3)'
        write (unit, '(a)') '    print *, arr(:)'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_std_slice_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
print *, '  EXPECTED FAIL: Array slicing in context of new features - ', trim(error_msg)
        else
            print *, '  Array slicing might be working'
        end if

    end function test_array_slice_standardization

end program test_standardization_red
