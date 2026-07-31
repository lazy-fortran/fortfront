program test_issue_2924_corpus_accepted_forms
    ! Issue #2924: the rejection wave over-rejected valid Fortran. Each case
    ! below is an accepted-side fixture drawn from the gfortran.dg corpus
    ! (named in the comment) for a rule that had to be narrowed, paired with
    ! the negative control that must stay rejected.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== issue-2924: corpus-derived accepted forms ==='

    call test_dummy_procedure_call_accepted(all_tests_passed)
    call test_function_call_target_still_rejected(all_tests_passed)
    call test_coindexed_type_bound_call_accepted(all_tests_passed)
    call test_external_implicit_none_type_bound_accepted(all_tests_passed)
    call test_external_implicit_none_still_rejected(all_tests_passed)
    call test_interface_bodies_share_binding_label(all_tests_passed)
    call test_duplicate_definition_labels_still_rejected(all_tests_passed)
    call test_assumed_length_bind_c_dummy_accepted(all_tests_passed)
    call test_length_two_bind_c_dummy_still_rejected(all_tests_passed)
    call test_continued_character_constant_accepted(all_tests_passed)
    call test_unterminated_character_constant_still_rejected(all_tests_passed)
    call test_nested_old_style_constructor_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All issue-2924 accepted-form tests passed'
        stop 0
    else
        print *, 'Some issue-2924 accepted-form tests failed'
        stop 1
    end if

contains

    ! gfortran.dg/proc_decl_12.f90: a dummy procedure declared with PROCEDURE
    ! is callable; it is not a typed data object.
    subroutine test_dummy_procedure_call_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'PROCEDURE dummy called with CALL (accepted)...'
        source = 'module m_dummy_proc'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine one(a)'//new_line('a')// &
            'integer, intent(in) :: a(:)'//new_line('a')// &
            'print *, size(a)'//new_line('a')// &
            'end subroutine one'//new_line('a')// &
            'subroutine driver(f)'//new_line('a')// &
            'procedure(one) :: f'//new_line('a')// &
            'call f([1, 2, 3])'//new_line('a')// &
            'end subroutine driver'//new_line('a')// &
            'end module m_dummy_proc'
        call expect_frontend_success(source, passed)
    end subroutine test_dummy_procedure_call_accepted

    subroutine test_function_call_target_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'CALL naming a typed local variable (rejected)...'
        source = 'program p_typed_call'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: f'//new_line('a')// &
            'f = 1'//new_line('a')// &
            'call f()'//new_line('a')// &
            'end program p_typed_call'
        call expect_frontend_error(source, 'not consistent with the CALL', passed)
    end subroutine test_function_call_target_still_rejected

    ! Issue #2924: `call x[1]%c%nopoly%sub()` names a binding of the declared
    ! type; the base object legitimately has a type.
    subroutine test_coindexed_type_bound_call_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Coindexed type-bound call (accepted)...'
        source = 'module m_coarray_call'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'type :: t0'//new_line('a')// &
            'contains'//new_line('a')// &
            'procedure, nopass :: sub => t0_sub'//new_line('a')// &
            'end type t0'//new_line('a')// &
            'type :: t1'//new_line('a')// &
            'type(t0) :: nopoly'//new_line('a')// &
            'end type t1'//new_line('a')// &
            'type :: t2'//new_line('a')// &
            'type(t1) :: c'//new_line('a')// &
            'end type t2'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine t0_sub()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end subroutine t0_sub'//new_line('a')// &
            'end module m_coarray_call'//new_line('a')// &
            'program p_coarray_call'//new_line('a')// &
            'use m_coarray_call, only: t2'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'type(t2) :: x[*]'//new_line('a')// &
            'call x[1]%c%nopoly%sub()'//new_line('a')// &
            'end program p_coarray_call'
        call expect_frontend_success(source, passed)
    end subroutine test_coindexed_type_bound_call_accepted

    ! Issue #2924: IMPLICIT NONE (EXTERNAL) looked up `call b%show()` as the
    ! literal name "b%show" and demanded a declaration for it.
    subroutine test_external_implicit_none_type_bound_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Type-bound call under IMPLICIT NONE (EXTERNAL) (accepted)...'
        source = 'module m_tbp_external'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'type :: box_t'//new_line('a')// &
            'integer :: v = 1'//new_line('a')// &
            'contains'//new_line('a')// &
            'procedure :: show => box_show'//new_line('a')// &
            'end type box_t'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine box_show(self)'//new_line('a')// &
            'class(box_t), intent(in) :: self'//new_line('a')// &
            'print *, self%v'//new_line('a')// &
            'end subroutine box_show'//new_line('a')// &
            'end module m_tbp_external'//new_line('a')// &
            'program p_tbp_external'//new_line('a')// &
            'use m_tbp_external, only: box_t'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'type(box_t) :: b'//new_line('a')// &
            'call b%show()'//new_line('a')// &
            'end program p_tbp_external'
        call expect_frontend_success(source, passed)
    end subroutine test_external_implicit_none_type_bound_accepted

    subroutine test_external_implicit_none_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Undeclared plain call under IMPLICIT NONE (EXTERNAL)...'
        source = 'program p_undeclared_external'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'call mystery_sub()'//new_line('a')// &
            'end program p_undeclared_external'
        call expect_frontend_error(source, 'IMPLICIT NONE (EXTERNAL)', passed)
    end subroutine test_external_implicit_none_still_rejected

    ! gfortran.dg/binding_label_tests_24.f90: two interface bodies in distinct
    ! scoping units may carry the same binding label.
    subroutine test_interface_bodies_share_binding_label(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Interface bodies sharing a binding label (accepted)...'
        source = 'module m_iface_a'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine f() bind(c, name="func")'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m_iface_a'//new_line('a')// &
            'module m_iface_b'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine g() bind(c, name="func")'//new_line('a')// &
            'end subroutine g'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m_iface_b'
        call expect_frontend_success(source, passed)
    end subroutine test_interface_bodies_share_binding_label

    subroutine test_duplicate_definition_labels_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Two definitions sharing a binding label (rejected)...'
        source = 'module m_dup_defs'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine alpha() bind(c, name="shared_symbol")'//new_line('a')// &
            'end subroutine alpha'//new_line('a')// &
            'subroutine beta() bind(c, name="shared_symbol")'//new_line('a')// &
            'end subroutine beta'//new_line('a')// &
            'end module m_dup_defs'
        call expect_frontend_error(source, 'binding label "shared_symbol"', passed)
    end subroutine test_duplicate_definition_labels_still_rejected

    ! gfortran.dg/c-prototypes_2.F90: an assumed-length character dummy is
    ! permitted in a BIND(C) interface.
    subroutine test_assumed_length_bind_c_dummy_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Assumed-length character BIND(C) dummy (accepted)...'
        source = 'subroutine s(b) bind(c)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(*) :: b'//new_line('a')// &
            'print *, b'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_success(source, passed)
    end subroutine test_assumed_length_bind_c_dummy_accepted

    subroutine test_length_two_bind_c_dummy_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Character(len=2) BIND(C) dummy (rejected)...'
        source = 'subroutine s(b) bind(c)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=2) :: b'//new_line('a')// &
            'print *, b'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'length other than 1', passed)
    end subroutine test_length_two_bind_c_dummy_still_rejected

    ! gfortran.dg/continuation_1.f90: a character constant continued with a
    ! trailing ampersand stays valid when the continuation line omits the
    ! leading ampersand.
    subroutine test_continued_character_constant_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Continued character constant (accepted)...'
        source = 'program p_continued'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=40) :: c'//new_line('a')// &
            'c = "Hello, &'//new_line('a')// &
            '     world!"'//new_line('a')// &
            'print *, c'//new_line('a')// &
            'end program p_continued'
        call expect_frontend_success(source, passed)
    end subroutine test_continued_character_constant_accepted

    subroutine test_unterminated_character_constant_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Genuinely unterminated character constant (rejected)...'
        source = 'program p_unterminated'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=10) :: c'//new_line('a')// &
            'c = "oops'//new_line('a')// &
            'print *, c'//new_line('a')// &
            'end program p_unterminated'
        call expect_frontend_error(source, 'Unterminated character constant', passed)
    end subroutine test_unterminated_character_constant_still_rejected

    ! gfortran.dg/array_constructor_1.f90: an old-style `(/ ... /)` nested in a
    ! bracket constructor is not a malformed complex literal.
    subroutine test_nested_old_style_constructor_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Nested old-style array constructor (accepted)...'
        source = 'program p_nested_ac'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer :: a(4)'//new_line('a')// &
            'a = [ (/ 1, 2, 3, 4 /) ]'//new_line('a')// &
            'print *, a'//new_line('a')// &
            'end program p_nested_ac'
        call expect_frontend_success(source, passed)
    end subroutine test_nested_old_style_constructor_accepted

    subroutine expect_frontend_error(source, expected, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (result%success()) then
            print *, '  FAIL: invalid source was accepted'
            passed = .false.
            return
        end if
        if (index(result%diagnostic_text, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_error

    subroutine expect_frontend_success(source, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%success()) then
            print *, '  FAIL: valid source was rejected'
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_success

end program test_issue_2924_corpus_accepted_forms
