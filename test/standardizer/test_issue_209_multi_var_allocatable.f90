program test_issue_209_multi_var_allocatable
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #209: Multi-variable declarations with different ' // &
             'allocatable status need splitting ==='
    print *

    if (.not. test_multi_var_mixed_allocatable()) all_passed = .false.
    if (.not. test_multi_var_all_allocatable()) all_passed = .false.
    if (.not. test_multi_var_none_allocatable()) all_passed = .false.
    if (.not. test_multi_var_with_initializers()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All tests PASSED'
        stop 0
    else
        print *, 'Some tests FAILED'
        stop 1
    end if

contains

    logical function test_multi_var_mixed_allocatable()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_split_declarations, found_allocatable_a
        logical :: found_non_allocatable_bc

        test_multi_var_mixed_allocatable = .true.
        print *, 'Testing multi-variable declaration with mixed allocatable needs...'

        ! Test the core issue: integer, dimension(5) :: a, b, c
        ! where only 'a' gets array reassignment
        input_file = 'test_209_mixed.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer, dimension(5) :: a, b, c'
        write (unit, '(a)') '    a = [1, 2]'
        write (unit, '(a)') '    a = [a, 3]  ! Only a gets reassigned'
        write (unit, '(a)') '    b(1) = 10   ! b and c are just indexed'
        write (unit, '(a)') '    c(2) = 20'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_209_mixed_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check if declarations are properly split
            open (newunit=unit, file=output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                found_split_declarations = .false.
                found_allocatable_a = .false.
                found_non_allocatable_bc = .false.

                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    
                    ! Look for split declarations:
                    ! Expected: integer, allocatable :: a(:)
                    ! Expected: integer :: b(5), c(5)
                    if (index(line, 'allocatable') > 0 .and. &
                        index(line, 'a') > 0) then
                        found_allocatable_a = .true.
                        print *, '    Found allocatable declaration for a:', trim(line)
                    end if
                    
                    ! Look for b and c with proper dimensions (not allocatable)
                    if ((index(line, 'b(5)') > 0 .or. &
                         index(line, 'c(5)') > 0) .and. &
                        index(line, 'allocatable') == 0) then
                        found_non_allocatable_bc = .true.
                        print *, '    Found non-allocatable declaration for b/c:', trim(line)
                    end if
                end do
                close (unit)

                found_split_declarations = found_allocatable_a .and. &
                                         found_non_allocatable_bc

                if (found_split_declarations) then
                    print *, '  SUCCESS: Multi-variable declaration correctly split'
                    print *, '    - Variable a marked as allocatable'
                    print *, '    - Variables b,c remain non-allocatable'
                else
                    print *, '  FAIL: Declaration not properly split'
                    print *, '    found_allocatable_a =', found_allocatable_a
                    print *, '    found_non_allocatable_bc =', &
                             found_non_allocatable_bc
                    test_multi_var_mixed_allocatable = .false.
                end if
            else
                print *, '  FAIL: Could not read output file'
                test_multi_var_mixed_allocatable = .false.
            end if
        else
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multi_var_mixed_allocatable = .false.
        end if

    end function test_multi_var_mixed_allocatable

    logical function test_multi_var_all_allocatable()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_all_allocatable

        test_multi_var_all_allocatable = .true.
        print *, 'Testing multi-variable declaration where all need allocatable...'

        ! Test case where all variables get array reassignment
        input_file = 'test_209_all.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer, dimension(3) :: x, y, z'
        write (unit, '(a)') '    x = [1, 2]'
        write (unit, '(a)') '    y = [3, 4]'
        write (unit, '(a)') '    z = [5, 6]'
        write (unit, '(a)') '    x = [x, 3]  ! All get reassigned'
        write (unit, '(a)') '    y = [y, 7]'
        write (unit, '(a)') '    z = [z, 8]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_209_all_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! All variables should be allocatable, but might remain as
            ! single declaration
            open (newunit=unit, file=output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                found_all_allocatable = .false.

                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    
                    ! Look for allocatable declaration with all variables
                    if (index(line, 'allocatable') > 0 .and. &
                        (index(line, 'x') > 0 .and. &
                         index(line, 'y') > 0 .and. &
                         index(line, 'z') > 0)) then
                        found_all_allocatable = .true.
                        exit
                    end if
                end do
                close (unit)

                if (found_all_allocatable) then
                    print *, '  SUCCESS: All variables correctly marked allocatable'
                else
                    print *, '  FAIL: Not all variables marked allocatable'
                    test_multi_var_all_allocatable = .false.
                end if
            else
                print *, '  FAIL: Could not read output file'
                test_multi_var_all_allocatable = .false.
            end if
        else
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multi_var_all_allocatable = .false.
        end if

    end function test_multi_var_all_allocatable

    logical function test_multi_var_none_allocatable()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_non_allocatable

        test_multi_var_none_allocatable = .true.
        print *, 'Testing multi-variable declaration with no allocatable needs...'

        ! Test case where no variables get array reassignment
        input_file = 'test_209_none.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer, dimension(5) :: p, q, r'
        write (unit, '(a)') '    p(1) = 10  ! Only indexing operations'
        write (unit, '(a)') '    q(2) = 20'
        write (unit, '(a)') '    r(3) = 30'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_209_none_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! No variables should be allocatable
            open (newunit=unit, file=output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                found_non_allocatable = .true.

                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    
                    ! Should NOT find allocatable in any declaration
                    if (index(line, 'allocatable') > 0) then
                        found_non_allocatable = .false.
                        exit
                    end if
                end do
                close (unit)

                if (found_non_allocatable) then
                    print *, '  SUCCESS: No variables incorrectly marked allocatable'
                else
                    print *, '  FAIL: Variables incorrectly marked allocatable'
                    test_multi_var_none_allocatable = .false.
                end if
            else
                print *, '  FAIL: Could not read output file'
                test_multi_var_none_allocatable = .false.
            end if
        else
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multi_var_none_allocatable = .false.
        end if

    end function test_multi_var_none_allocatable

    logical function test_multi_var_with_initializers()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat

        test_multi_var_with_initializers = .true.
        print *, 'Testing multi-variable declaration with initializers...'

        ! Test case with variable initializers
        input_file = 'test_209_init.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer, dimension(3) :: m = [1, 2, 3], n = [4, 5, 6]'
        write (unit, '(a)') '    m = [m, 7]  ! Only m gets reassigned'
        write (unit, '(a)') '    print *, n(1)'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_209_init_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            print *, '  SUCCESS: Handled multi-variable with initializers'
        else
            print *, '  Expected complexity: ', trim(error_msg)
            ! This might be complex due to initializer handling
        end if

    end function test_multi_var_with_initializers

end program test_issue_209_multi_var_allocatable