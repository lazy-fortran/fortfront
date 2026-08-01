program test_issue_2978_module_generic_conflict
    ! Issue #2978: a Lazy module procedure with untyped dummy arguments that is
    ! referenced at two incompatible argument types is not monomorphized, so a
    ! single body would be called at both types. That must be diagnosed, never
    ! silently miscompiled.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_conflicting_module_call_is_rejected()) all_passed = .false.
    if (.not. test_single_type_module_call_is_accepted()) all_passed = .false.
    if (.not. test_explicitly_typed_module_procedure_is_accepted()) &
        all_passed = .false.
    if (.not. test_program_level_generic_still_monomorphizes()) &
        all_passed = .false.
    if (.not. test_corpus_generic_interface_module_is_accepted()) &
        all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #2978 module generic conflict diagnostics'
    else
        error stop 'FAIL: Issue #2978 module generic conflict diagnostics'
    end if

contains

    include 'common/read_example.inc'

    subroutine standardize(source, output, error_msg)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: output
        character(len=:), allocatable, intent(out) :: error_msg

        call transform_lazy_fortran_string(source, output, error_msg)
        if (.not. allocated(error_msg)) error_msg = ''
        if (.not. allocated(output)) output = ''
    end subroutine standardize

    logical function test_conflicting_module_call_is_rejected() result(ok)
        character(len=:), allocatable :: source, output, error_msg

        source = 'module lm'//new_line('A')// &
            'contains'//new_line('A')// &
            '  function twice(x)'//new_line('A')// &
            '    twice = 2 * x'//new_line('A')// &
            '  end function'//new_line('A')// &
            '  subroutine useboth()'//new_line('A')// &
            '    print *, twice(3)'//new_line('A')// &
            '    print *, twice(2.5)'//new_line('A')// &
            '  end subroutine'//new_line('A')// &
            'end module lm'//new_line('A')// &
            'program main'//new_line('A')// &
            '  use lm'//new_line('A')// &
            '  call useboth()'//new_line('A')// &
            'end program'

        call standardize(source, output, error_msg)
        ok = len_trim(error_msg) > 0
        if (.not. ok) then
            write (error_unit, '(A)') &
                'ERROR: conflicting module generic accepted silently'
            write (error_unit, '(A)') trim(output)
            return
        end if
        if (index(error_msg, 'twice') == 0) then
            write (error_unit, '(A)') &
                'ERROR: diagnostic does not name the procedure: '// &
                trim(error_msg)
            ok = .false.
        end if
    end function test_conflicting_module_call_is_rejected

    logical function test_single_type_module_call_is_accepted() result(ok)
        character(len=:), allocatable :: source, output, error_msg

        source = 'module lm'//new_line('A')// &
            'contains'//new_line('A')// &
            '  function twice(x)'//new_line('A')// &
            '    twice = 2 * x'//new_line('A')// &
            '  end function'//new_line('A')// &
            '  subroutine useone()'//new_line('A')// &
            '    print *, twice(2.5)'//new_line('A')// &
            '    print *, twice(3.5)'//new_line('A')// &
            '  end subroutine'//new_line('A')// &
            'end module lm'//new_line('A')// &
            'program main'//new_line('A')// &
            '  use lm'//new_line('A')// &
            '  call useone()'//new_line('A')// &
            'end program'

        call standardize(source, output, error_msg)
        ok = len_trim(error_msg) == 0
        if (.not. ok) then
            write (error_unit, '(A)') &
                'ERROR: single-type module generic rejected: '//trim(error_msg)
        end if
    end function test_single_type_module_call_is_accepted

    logical function test_explicitly_typed_module_procedure_is_accepted() &
            result(ok)
        character(len=:), allocatable :: source, output, error_msg

        source = 'module lm'//new_line('A')// &
            '  implicit none'//new_line('A')// &
            'contains'//new_line('A')// &
            '  real function twice(x)'//new_line('A')// &
            '    real, intent(in) :: x'//new_line('A')// &
            '    twice = 2 * x'//new_line('A')// &
            '  end function'//new_line('A')// &
            '  subroutine useboth()'//new_line('A')// &
            '    print *, twice(3.0)'//new_line('A')// &
            '    print *, twice(2.5)'//new_line('A')// &
            '  end subroutine'//new_line('A')// &
            'end module lm'//new_line('A')// &
            'program main'//new_line('A')// &
            '  use lm'//new_line('A')// &
            '  call useboth()'//new_line('A')// &
            'end program'

        call standardize(source, output, error_msg)
        ok = len_trim(error_msg) == 0
        if (.not. ok) then
            write (error_unit, '(A)') &
                'ERROR: explicitly typed module procedure rejected: '// &
                trim(error_msg)
        end if
    end function test_explicitly_typed_module_procedure_is_accepted

    logical function test_program_level_generic_still_monomorphizes() result(ok)
        character(len=:), allocatable :: source, output, error_msg

        source = 'function twice(x)'//new_line('A')// &
            '  twice = 2 * x'//new_line('A')// &
            'end function'//new_line('A')// &
            'print *, twice(3)'//new_line('A')// &
            'print *, twice(2.5)'

        call standardize(source, output, error_msg)
        ok = len_trim(error_msg) == 0
        if (.not. ok) then
            write (error_unit, '(A)') &
                'ERROR: program-level generic rejected: '//trim(error_msg)
            return
        end if
        if (index(output, 'twice__') == 0) then
            write (error_unit, '(A)') &
                'ERROR: program-level generic no longer monomorphized'
            write (error_unit, '(A)') trim(output)
            ok = .false.
        end if
    end function test_program_level_generic_still_monomorphizes
    ! Accepted-side control taken from the corpus: a module whose generic
    ! interface resolves two explicitly typed specifics at two argument types
    ! must keep compiling.
    logical function test_corpus_generic_interface_module_is_accepted() &
            result(ok)
        character(len=:), allocatable :: source, output, error_msg

        call read_example('examples/lf/issue_1411_generic_module.lf', source)
        call standardize(source, output, error_msg)
        ok = len_trim(error_msg) == 0
        if (.not. ok) then
            write (error_unit, '(A)') &
                'ERROR: corpus generic-interface module rejected: '// &
                trim(error_msg)
        end if
    end function test_corpus_generic_interface_module_is_accepted

end program test_issue_2978_module_generic_conflict
