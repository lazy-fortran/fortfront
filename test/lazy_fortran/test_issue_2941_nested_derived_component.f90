program test_issue_2941_nested_derived_component
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_statement_after_nested_type()) all_passed = .false.
    if (.not. test_allocatable_component()) all_passed = .false.
    if (.not. test_declaration_after_nested_type()) all_passed = .false.
    if (.not. test_copied_derived_binding()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #2941 nested derived component keeps later statements'
    else
        error stop 'FAIL: Issue #2941 nested derived component'
    end if

contains

    logical function standardize(source, output) result(ok)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable :: error_msg

        ok = .true.
        call transform_lazy_fortran_string(source, output, error_msg)
        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: no output'
            ok = .false.
            return
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'ERROR: '//trim(error_msg)
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

    logical function test_statement_after_nested_type() result(ok)
        character(len=:), allocatable :: output

        ok = standardize( &
             'type :: a_t'//new_line('a')// &
             '    integer :: x'//new_line('a')// &
             'end type'//new_line('a')// &
             'type :: b_t'//new_line('a')// &
             '    type(a_t) :: y'//new_line('a')// &
             'end type'//new_line('a')// &
             'print *, 42'//new_line('a'), output)
        if (.not. ok) return
        if (.not. expect(output, 'type(a_t) :: y', 'nested component')) ok = .false.
        if (.not. expect(output, 'print *, 42', 'nested component')) ok = .false.
    end function test_statement_after_nested_type

    logical function test_allocatable_component() result(ok)
        character(len=:), allocatable :: output

        ok = standardize( &
             'type :: a_t'//new_line('a')// &
             '    integer :: x'//new_line('a')// &
             'end type'//new_line('a')// &
             'type :: b_t'//new_line('a')// &
             '    integer :: n'//new_line('a')// &
             '    type(a_t), allocatable :: y'//new_line('a')// &
             'end type'//new_line('a')// &
             'print *, 7'//new_line('a'), output)
        if (.not. ok) return
        if (.not. expect(output, 'print *, 7', 'allocatable component')) ok = .false.
    end function test_allocatable_component

    logical function test_declaration_after_nested_type() result(ok)
        character(len=:), allocatable :: output

        ok = standardize( &
             'type :: a_t'//new_line('a')// &
             '    integer :: x'//new_line('a')// &
             'end type'//new_line('a')// &
             'type :: b_t'//new_line('a')// &
             '    type(a_t) :: y'//new_line('a')// &
             'end type'//new_line('a')// &
             'integer :: k'//new_line('a')// &
             'k = 3'//new_line('a')// &
             'print *, k'//new_line('a'), output)
        if (.not. ok) return
        if (.not. expect(output, 'integer :: k', 'decl after')) ok = .false.
        if (.not. expect(output, 'k = 3', 'decl after')) ok = .false.
        if (.not. expect(output, 'print *, k', 'decl after')) ok = .false.
    end function test_declaration_after_nested_type

    logical function test_copied_derived_binding() result(ok)
        character(len=:), allocatable :: output

        ok = standardize( &
             'type :: p_t'//new_line('a')// &
             '    integer :: x'//new_line('a')// &
             'end type'//new_line('a')// &
             'p = p_t(3)'//new_line('a')// &
             'q = p'//new_line('a')// &
             'print *, q%x'//new_line('a'), output)
        if (.not. ok) return
        if (.not. expect(output, 'type(p_t) :: p, q', 'copied binding')) ok = .false.
        if (index(output, 'integer :: q') > 0) then
            write (error_unit, '(A)') 'ERROR: q inferred as integer'
            ok = .false.
        end if
    end function test_copied_derived_binding

end program test_issue_2941_nested_derived_component
