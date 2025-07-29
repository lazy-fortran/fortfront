program test_array_slicing_codegen
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Slicing Code Generation Tests ==='
    print *

    if (.not. test_basic_slicing()) all_passed = .false.
    if (.not. test_empty_bounds()) all_passed = .false.
    if (.not. test_multiple_subscripts()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All array slicing code generation tests passed!'
        stop 0
    else
        print *, 'Some array slicing code generation tests failed!'
        stop 1
    end if

contains

    logical function test_basic_slicing()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_correct_slice

        test_basic_slicing = .true.
        print *, 'Testing basic array slicing code generation...'

        ! Create test input
        input_file = 'test_slice.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [1, 2, 3, 4, 5]'
        write (unit, '(a)') 'print *, arr(2:4)'
        close (unit)

        ! Compile with frontend
        output_file = 'test_slice_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_basic_slicing = .false.
            return
        end if

        ! Check generated code contains arr(2:4)
        found_correct_slice = .false.
        open (newunit=unit, file=output_file, status='old')
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'arr(2:4)') > 0) then
                found_correct_slice = .true.
                print *, '  Found correct slice:', trim(line)
                exit
            end if
        end do
        close (unit)

        if (found_correct_slice) then
            print *, '  PASS: Array slicing generates correct code'
        else
            print *, '  FAIL: arr(2:4) not found in generated code'
            test_basic_slicing = .false.
        end if

    end function test_basic_slicing

    logical function test_empty_bounds()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_lower_empty, found_upper_empty, found_both_empty

        test_empty_bounds = .true.
        print *, 'Testing array slicing with empty bounds...'

        ! Create test input
        input_file = 'test_empty.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [1, 2, 3, 4, 5]'
        write (unit, '(a)') 'print *, arr(:3)'    ! Empty lower bound
        write (unit, '(a)') 'print *, arr(2:)'    ! Empty upper bound
        write (unit, '(a)') 'print *, arr(:)'     ! Both empty
        close (unit)

        ! Compile with frontend
        output_file = 'test_empty_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_empty_bounds = .false.
            return
        end if

        ! Check generated code
        found_lower_empty = .false.
        found_upper_empty = .false.
        found_both_empty = .false.

        open (newunit=unit, file=output_file, status='old')
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'arr(:3)') > 0) found_lower_empty = .true.
            if (index(line, 'arr(2:)') > 0) found_upper_empty = .true.
            if (index(line, 'arr(:)') > 0) found_both_empty = .true.
        end do
        close (unit)

        if (found_lower_empty .and. found_upper_empty .and. found_both_empty) then
            print *, '  PASS: Empty bounds handled correctly'
        else
            print *, '  FAIL: Empty bounds not generated correctly'
            if (.not. found_lower_empty) print *, '    Missing arr(:3)'
            if (.not. found_upper_empty) print *, '    Missing arr(2:)'
            if (.not. found_both_empty) print *, '    Missing arr(:)'
            test_empty_bounds = .false.
        end if

    end function test_empty_bounds

    logical function test_multiple_subscripts()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_multi_slice

        test_multiple_subscripts = .true.
        print *, 'Testing multiple subscripts (2D array slicing)...'

        ! Create test input
        input_file = 'test_multi.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'integer :: matrix(5,5)'
        write (unit, '(a)') 'print *, matrix(2:4, 1:3)'
        close (unit)

        ! Compile with frontend
        output_file = 'test_multi_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_multiple_subscripts = .false.
            return
        end if

        ! Check generated code
        found_multi_slice = .false.
        open (newunit=unit, file=output_file, status='old')
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, 'matrix(2:4, 1:3)') > 0 .or. &
                index(line, 'matrix(2:4,1:3)') > 0) then
                found_multi_slice = .true.
                print *, '  Found multi-dimensional slice:', trim(line)
                exit
            end if
        end do
        close (unit)

        if (found_multi_slice) then
            print *, '  PASS: Multi-dimensional slicing works'
        else
            print *, '  FAIL: Multi-dimensional slice not found'
            test_multiple_subscripts = .false.
        end if

    end function test_multiple_subscripts

end program test_array_slicing_codegen
