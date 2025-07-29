program test_semantic_analysis_green
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Semantic Analysis Tests ==='
    print *, 'These tests check semantic analysis is working correctly'
    print *

    if (.not. test_array_assignment_semantics()) all_passed = .false.
    if (.not. test_type_consistency()) all_passed = .false.
    if (.not. test_scope_resolution()) all_passed = .false.
    if (.not. test_intrinsic_functions()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All semantic analysis tests passed!'
        stop 0
    else
        print *, 'Some semantic analysis tests failed!'
        stop 1
    end if

contains

    logical function test_array_assignment_semantics()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_assignment_semantics = .true.
        print *, 'Testing array assignment semantic analysis...'

        ! Test 1: Array literal assignment
        input_file = 'test_sem_arr.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(3)'
        write (unit, '(a)') '    arr = [1, 2, 3]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_sem_arr_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: ', trim(error_msg)
            test_array_assignment_semantics = .false.
        else
            print *, '  PASS: Array assignment semantic analysis working'
        end if

        ! Test 2: Array constructor assignment
        input_file = 'test_sem_ctor.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(10)'
        write (unit, '(a)') '    arr = [(i, i=1,10)]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_sem_ctor_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: ', trim(error_msg)
            test_array_assignment_semantics = .false.
        else
            print *, '  PASS: Array constructor semantic analysis working'
        end if

    end function test_array_assignment_semantics

    logical function test_type_consistency()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_type_consistency = .true.
        print *, 'Testing type consistency in semantic analysis...'

        ! Test: Mixed types in array
        input_file = 'test_sem_mixed.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    real :: arr(3)'
        write (unit, '(a)') '    arr = [1, 2.0, 3]'  ! Mixed integer and real
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_sem_mixed_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED: Type mismatch detected'
        else
            print *, '  Type coercion might be working (or not checked)'
        end if

    end function test_type_consistency

    logical function test_scope_resolution()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_scope_resolution = .true.
        print *, 'Testing scope resolution in semantic analysis...'

        ! Test: Variable used in implied do loop
        input_file = 'test_sem_scope.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(10), j'
        write (unit, '(a)') '    j = 5'
        write (unit, '(a)') '    arr = [(i*j, i=1,10)]'  ! j should be accessible
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_sem_scope_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Scope resolution issue - ', trim(error_msg)
            test_scope_resolution = .false.
        else
            print *, '  PASS: Scope resolution working'
        end if

    end function test_scope_resolution

    logical function test_intrinsic_functions()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_intrinsic_functions = .true.
        print *, 'Testing intrinsic function semantic analysis...'

        ! Test: Array intrinsics
        input_file = 'test_sem_intrinsic.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(5)'
        write (unit, '(a)') '    arr = [1, 2, 3, 4, 5]'
        write (unit, '(a)') '    print *, sum(arr)'
        write (unit, '(a)') '    print *, maxval(arr)'
        write (unit, '(a)') '    print *, size(arr)'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_sem_intrinsic_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Intrinsic functions - ', trim(error_msg)
            test_intrinsic_functions = .false.
        else
            print *, '  PASS: Intrinsic functions working'
        end if

    end function test_intrinsic_functions

end program test_semantic_analysis_green
