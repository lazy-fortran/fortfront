program test_issue_2834_first_assignment_inference
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_bare_true_declares_logical()) all_passed = .false.
    if (.not. test_chained_comparison_declares_logical()) all_passed = .false.
    if (.not. test_character_array_declares_character()) all_passed = .false.
    if (.not. test_derived_constructor_declares_type()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #2834 first-assignment inference forms'
    else
        error stop 'FAIL: Issue #2834 first-assignment inference forms'
    end if

contains

    include 'common/read_example.inc'

    logical function standardize(example, output) result(ok)
        character(len=*), intent(in) :: example
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable :: source, error_msg

        ok = .true.
        call read_example(example, source)
        call transform_lazy_fortran_string(source, output, error_msg)
        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: no output for '//example
            ok = .false.
            return
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'ERROR: '//example//': '//trim(error_msg)
                ok = .false.
            end if
        end if
    end function standardize

    logical function expect(output, needle, context) result(ok)
        character(len=*), intent(in) :: output, needle, context

        ok = index(output, needle) > 0
        if (.not. ok) then
            write (error_unit, '(A)') 'ERROR: '//context// &
                ' missing "'//needle//'"'
            write (error_unit, '(A)') trim(output)
        end if
    end function expect

    logical function test_bare_true_declares_logical() result(ok)
        character(len=:), allocatable :: output

        ok = standardize('examples/lf/boolean_assign_bare_true.lf', output)
        if (.not. ok) return
        if (.not. expect(output, 'logical :: flag', 'bare true')) ok = .false.
        if (.not. expect(output, '.true.', 'bare true')) ok = .false.
    end function test_bare_true_declares_logical

    logical function test_chained_comparison_declares_logical() result(ok)
        character(len=:), allocatable :: output

        ok = standardize('examples/lf/chained_comparison.lf', output)
        if (.not. ok) return
        if (.not. expect(output, 'logical :: result', 'chained')) ok = .false.
        if (.not. expect(output, '.and.', 'chained')) ok = .false.
        if (.not. expect(output, '1 < x', 'chained')) ok = .false.
        if (.not. expect(output, 'x < 10', 'chained')) ok = .false.
    end function test_chained_comparison_declares_logical

    logical function test_character_array_declares_character() result(ok)
        character(len=:), allocatable :: output

        ok = standardize('examples/lf/docs_character_arrays.lf', output)
        if (.not. ok) return
        if (.not. expect(output, 'character(len=7) :: names(3)', &
                         'character array')) ok = .false.
    end function test_character_array_declares_character

    logical function test_derived_constructor_declares_type() result(ok)
        character(len=:), allocatable :: output

        ok = standardize('examples/lf/issue_2827_derived_constructor_inference.lf', &
                         output)
        if (.not. ok) return
        if (.not. expect(output, 'type(point_t) :: p', 'derived')) ok = .false.
        if (index(output, 'external') > 0) then
            write (error_unit, '(A)') &
                'ERROR: derived: spurious external declaration for type name'
            write (error_unit, '(A)') trim(output)
            ok = .false.
        end if
    end function test_derived_constructor_declares_type

end program test_issue_2834_first_assignment_inference
