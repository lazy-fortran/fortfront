program test_issue_209_multi_var_allocatable
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #209: Multi-variable declarations with different ' // &
             'allocatable status need splitting ==='
    print *

    if (.not. test_mixed_allocatable_needs_splits_declaration()) &
        all_passed = .false.
    if (.not. test_all_allocatable_needs_single_declaration()) &
        all_passed = .false.
    if (.not. test_no_allocatable_needs_unchanged()) all_passed = .false.
    if (.not. test_multiple_mixed_declarations()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All tests PASSED'
    else
        print *, 'Some tests FAILED'
        error stop 1  ! Use error stop for actual failures
    end if

contains

    ! Given: A multi-variable declaration where only some variables need allocatable
    ! When: The standardizer processes the code
    ! Then: The declaration should be split with correct allocatable attributes  
    logical function test_mixed_allocatable_needs_splits_declaration()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        integer :: decl_count, allocatable_count, non_allocatable_count
        logical :: found_split, found_a_alloc, found_bc_non_alloc

        test_mixed_allocatable_needs_splits_declaration = .true.
        print *, 'Testing mixed allocatable needs splits declaration...'

        ! Given: Source with multi-var declaration where only 'a' needs allocatable
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

        ! When: We compile and standardize
        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_mixed_allocatable_needs_splits_declaration = .false.
            return
        end if

        ! Then: Verify declaration splitting and correct allocatable attributes
        call count_and_verify_declarations(output_file, decl_count, &
                                          allocatable_count, non_allocatable_count, &
                                          found_split, found_a_alloc, found_bc_non_alloc)

        ! CRITICAL ASSERTION: Must have >1 declaration (splitting occurred)
        if (.not. found_split .or. decl_count < 2) then
            print *, '  FAIL: Declaration was NOT split into multiple nodes'
            print *, '    - Expected: >1 declaration, Found:', decl_count
            test_mixed_allocatable_needs_splits_declaration = .false.
        end if

        ! CRITICAL ASSERTION: Variable 'a' must be allocatable
        if (.not. found_a_alloc) then
            print *, '  FAIL: Variable "a" was NOT marked as allocatable'
            test_mixed_allocatable_needs_splits_declaration = .false.
        end if

        ! CRITICAL ASSERTION: Variables 'b','c' must NOT be allocatable
        if (.not. found_bc_non_alloc) then
            print *, '  FAIL: Variables "b"/"c" incorrectly marked as allocatable'
            test_mixed_allocatable_needs_splits_declaration = .false.
        end if

        if (test_mixed_allocatable_needs_splits_declaration) then
            print *, '  SUCCESS: Declaration splitting and allocatable assignment correct'
        end if

    end function test_mixed_allocatable_needs_splits_declaration

    ! Given: A multi-variable declaration where all variables need allocatable
    ! When: The standardizer processes the code
    ! Then: All variables should be marked as allocatable (may remain single declaration)
    logical function test_all_allocatable_needs_single_declaration()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: all_vars_allocatable

        test_all_allocatable_needs_single_declaration = .true.
        print *, 'Testing all variables need allocatable...'

        ! Given: Source where all variables get array reassignments
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

        ! When: We compile and standardize  
        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_all_allocatable_needs_single_declaration = .false.
            return
        end if

        ! Then: Verify all variables are marked as allocatable
        call verify_all_variables_allocatable(output_file, ['x', 'y', 'z'], all_vars_allocatable)

        if (.not. all_vars_allocatable) then
            print *, '  FAIL: Not all variables marked as allocatable'
            test_all_allocatable_needs_single_declaration = .false.
        else
            print *, '  SUCCESS: All variables correctly marked as allocatable'
        end if

    end function test_all_allocatable_needs_single_declaration

    ! Given: A multi-variable declaration where no variables need allocatable
    ! When: The standardizer processes the code  
    ! Then: Declaration should remain unchanged with no allocatable attributes
    logical function test_no_allocatable_needs_unchanged()
        character(len=:), allocatable :: input_file, output_file  
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: any_vars_allocatable

        test_no_allocatable_needs_unchanged = .true.
        print *, 'Testing no allocatable needs unchanged...'

        ! Given: Source with only array indexing (no reassignments)
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

        ! When: We compile and standardize
        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_no_allocatable_needs_unchanged = .false.
            return
        end if

        ! Then: Verify no variables are marked as allocatable
        call verify_no_variables_allocatable(output_file, any_vars_allocatable)

        if (any_vars_allocatable) then
            print *, '  FAIL: Variables incorrectly marked as allocatable'
            test_no_allocatable_needs_unchanged = .false.
        else
            print *, '  SUCCESS: No variables incorrectly marked as allocatable'
        end if

    end function test_no_allocatable_needs_unchanged

    ! Given: Multiple multi-variable declarations with different mixed needs
    ! When: The standardizer processes the code
    ! Then: Each declaration should be handled correctly according to its specific needs
    logical function test_multiple_mixed_declarations()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options  
        integer :: unit
        logical :: complex_correct

        test_multiple_mixed_declarations = .true.
        print *, 'Testing multiple mixed declarations...'

        ! Given: Source with multiple complex multi-variable declarations
        input_file = 'test_209_complex.lf'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    ! First declaration: mixed needs'
        write (unit, '(a)') '    integer, dimension(3) :: a, b, c'
        write (unit, '(a)') '    ! Second declaration: all need allocatable'
        write (unit, '(a)') '    real, dimension(2) :: x, y'
        write (unit, '(a)') '    ! Third declaration: none need allocatable'
        write (unit, '(a)') '    logical, dimension(4) :: p, q'
        write (unit, '(a)') '    '
        write (unit, '(a)') '    ! Array reassignments'
        write (unit, '(a)') '    a = [1, 2]  ! Only a from first group needs allocatable'
        write (unit, '(a)') '    a = [a, 3]'
        write (unit, '(a)') '    x = [1.0, 2.0]  ! Both x and y need allocatable'
        write (unit, '(a)') '    y = [3.0, 4.0]'
        write (unit, '(a)') '    x = [x, 5.0]'
        write (unit, '(a)') '    y = [y, 6.0]'
        write (unit, '(a)') '    ! Only indexing for third group'
        write (unit, '(a)') '    p(1) = .true.'
        write (unit, '(a)') '    q(2) = .false.'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_209_complex_out.f90'
        options%output_file = output_file

        ! When: We compile and standardize
        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multiple_mixed_declarations = .false.
            return
        end if

        ! Then: Verify complex declarations handled correctly  
        call verify_complex_declarations_correctly(output_file, complex_correct)

        if (.not. complex_correct) then
            print *, '  FAIL: Complex multi-declarations not handled correctly'
            test_multiple_mixed_declarations = .false.
        else
            print *, '  SUCCESS: All complex multi-declarations handled correctly'
        end if

    end function test_multiple_mixed_declarations

    ! Helper subroutines for output file verification

    ! Count and verify declarations in output file for mixed case
    subroutine count_and_verify_declarations(output_file, decl_count, &
                                           allocatable_count, non_allocatable_count, &
                                           found_split, found_a_alloc, found_bc_non_alloc)
        character(len=*), intent(in) :: output_file
        integer, intent(out) :: decl_count, allocatable_count, non_allocatable_count
        logical, intent(out) :: found_split, found_a_alloc, found_bc_non_alloc
        integer :: unit, iostat
        character(len=256) :: line

        decl_count = 0
        allocatable_count = 0
        non_allocatable_count = 0
        found_split = .false.
        found_a_alloc = .false.
        found_bc_non_alloc = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Count declaration lines (exclude implicit none)
            if (index(line, '::') > 0 .and. index(line, 'implicit none') == 0) then
                decl_count = decl_count + 1
                
                if (index(line, 'allocatable') > 0) then
                    allocatable_count = allocatable_count + 1
                    ! Check for variable 'a' in allocatable declaration
                    if (index(line, ', a') > 0 .or. index(line, ':: a') > 0) then
                        found_a_alloc = .true.
                    end if
                else
                    non_allocatable_count = non_allocatable_count + 1
                    ! Check for variables 'b' or 'c' in non-allocatable declaration
                    if (index(line, ', b') > 0 .or. index(line, ':: b') > 0 .or. &
                        index(line, ', c') > 0 .or. index(line, ':: c') > 0) then
                        found_bc_non_alloc = .true.
                    end if
                end if
            end if
        end do
        close (unit)

        ! If we have >1 declaration, splitting occurred
        found_split = (decl_count > 1)

    end subroutine count_and_verify_declarations

    ! Verify all specified variables are marked as allocatable in output file
    subroutine verify_all_variables_allocatable(output_file, var_names, all_allocatable)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: var_names(:)
        logical, intent(out) :: all_allocatable
        integer :: unit, iostat, i
        character(len=256) :: line
        logical :: found_vars(size(var_names))

        all_allocatable = .false.
        found_vars = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Look for allocatable declarations
            if (index(line, 'allocatable') > 0) then
                do i = 1, size(var_names)
                    if (index(line, trim(var_names(i))) > 0) then
                        found_vars(i) = .true.
                    end if
                end do
            end if
        end do
        close (unit)

        all_allocatable = all(found_vars)

    end subroutine verify_all_variables_allocatable

    ! Verify no variables are marked as allocatable in output file
    subroutine verify_no_variables_allocatable(output_file, any_allocatable)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: any_allocatable
        integer :: unit, iostat
        character(len=256) :: line

        any_allocatable = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Look for any allocatable declarations
            if (index(line, 'allocatable') > 0) then
                any_allocatable = .true.
                exit
            end if
        end do
        close (unit)

    end subroutine verify_no_variables_allocatable

    ! Verify complex declarations are handled correctly in output file
    subroutine verify_complex_declarations_correctly(output_file, all_correct)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: all_correct
        integer :: unit, iostat
        character(len=256) :: line
        logical :: a_alloc, x_alloc, y_alloc, bc_non_alloc, pq_non_alloc

        all_correct = .false.
        a_alloc = .false.
        x_alloc = .false.
        y_alloc = .false.
        bc_non_alloc = .false.
        pq_non_alloc = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Look for allocatable declarations containing specific variables
            if (index(line, 'allocatable') > 0) then
                if (index(line, ', a') > 0 .or. index(line, ':: a') > 0) a_alloc = .true.
                if (index(line, ', x') > 0 .or. index(line, ':: x') > 0) x_alloc = .true.
                if (index(line, ', y') > 0 .or. index(line, ':: y') > 0) y_alloc = .true.
            end if

            ! Look for non-allocatable declarations
            if (index(line, '::') > 0 .and. index(line, 'allocatable') == 0 .and. &
                index(line, 'implicit none') == 0) then
                if (index(line, ', b') > 0 .or. index(line, ':: b') > 0 .or. &
                    index(line, ', c') > 0 .or. index(line, ':: c') > 0) bc_non_alloc = .true.
                if (index(line, ', p') > 0 .or. index(line, ':: p') > 0 .or. &
                    index(line, ', q') > 0 .or. index(line, ':: q') > 0) pq_non_alloc = .true.
            end if
        end do
        close (unit)

        ! All conditions must be met for correctness
        all_correct = a_alloc .and. x_alloc .and. y_alloc .and. bc_non_alloc .and. pq_non_alloc

    end subroutine verify_complex_declarations_correctly


end program test_issue_209_multi_var_allocatable