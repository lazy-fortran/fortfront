program test_bind_c_validation
    ! Tests that BIND(C) interoperability validation rejects non-interoperable
    ! derived-type components and procedure dummy arguments, and accepts valid
    ! interoperable types and procedures, per ISO/IEC 1539-1:2004 Section 15.3.2.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: derived_type_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_bind_c_validation, only: validate_bind_c_derived_type, &
                                          validate_bind_c_procedure
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== BIND(C) Interoperability Validation Tests ==='
    print *

    call test_type_allocatable_component_rejected(all_tests_passed)
    call test_type_pointer_component_rejected(all_tests_passed)
    call test_type_procedure_component_rejected(all_tests_passed)
    call test_valid_type_accepted(all_tests_passed)
    call test_assumed_shape_dummy_rejected(all_tests_passed)
    call test_assumed_size_dummy_rejected(all_tests_passed)
    call test_procedure_dummy_rejected(all_tests_passed)
    call test_valid_procedure_accepted(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All BIND(C) validation tests PASSED!'
        stop 0
    else
        print *, 'Some BIND(C) validation tests FAILED!'
        stop 1
    end if

contains

    subroutine validate_derived_types(src, errors)
        character(len=*), intent(in) :: src
        type(error_collection_t), intent(inout) :: errors
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: err
        integer :: root, i

        call lex_source(src, tokens, err)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, err)
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (derived_type_node)
                call validate_bind_c_derived_type(arena, node, errors)
            end select
        end do
    end subroutine validate_derived_types

    subroutine validate_procedures(src, errors)
        character(len=*), intent(in) :: src
        type(error_collection_t), intent(inout) :: errors
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: err
        integer :: root, i

        call lex_source(src, tokens, err)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root, err)
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                call validate_bind_c_procedure(arena, node%param_indices, &
                                               node%body_indices, &
                                               node%bind_c_clause, node%name, &
                                               errors)
            type is (subroutine_def_node)
                call validate_bind_c_procedure(arena, node%param_indices, &
                                               node%body_indices, &
                                               node%bind_c_clause, node%name, &
                                               errors)
            end select
        end do
    end subroutine validate_procedures

    subroutine test_type_allocatable_component_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) type with allocatable component (rejected)...'
        errors = create_error_collection()
        src = 'module m'//new_line('a')// &
              '  type, bind(c) :: t'//new_line('a')// &
              '    real, allocatable :: a(:)'//new_line('a')// &
              '  end type t'//new_line('a')// &
              'end module'
        call validate_derived_types(src, errors)
        call expect_error(errors%has_errors(), &
                          'allocatable component not rejected', passed)
    end subroutine test_type_allocatable_component_rejected

    subroutine test_type_pointer_component_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) type with pointer component (rejected)...'
        errors = create_error_collection()
        src = 'module m'//new_line('a')// &
              '  type, bind(c) :: t'//new_line('a')// &
              '    integer, pointer :: p'//new_line('a')// &
              '  end type t'//new_line('a')// &
              'end module'
        call validate_derived_types(src, errors)
        call expect_error(errors%has_errors(), &
                          'pointer component not rejected', passed)
    end subroutine test_type_pointer_component_rejected

    subroutine test_type_procedure_component_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) type with procedure component (rejected)...'
        errors = create_error_collection()
        src = 'module m'//new_line('a')// &
              '  type, bind(c) :: t'//new_line('a')// &
              '    procedure(real), pointer, nopass :: f'//new_line('a')// &
              '  end type t'//new_line('a')// &
              'end module'
        call validate_derived_types(src, errors)
        call expect_error(errors%has_errors(), &
                          'procedure component not rejected', passed)
    end subroutine test_type_procedure_component_rejected

    subroutine test_valid_type_accepted(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing valid interoperable BIND(C) type (accepted)...'
        errors = create_error_collection()
        src = 'module m'//new_line('a')// &
              '  use iso_c_binding'//new_line('a')// &
              '  type, bind(c) :: t'//new_line('a')// &
              '    integer(c_int) :: id'//new_line('a')// &
              '    real(c_double) :: position(3)'//new_line('a')// &
              '    type(c_ptr) :: payload'//new_line('a')// &
              '  end type t'//new_line('a')// &
              'end module'
        call validate_derived_types(src, errors)
        call expect_no_error(errors%has_errors(), &
                             'valid type incorrectly rejected', passed)
    end subroutine test_valid_type_accepted

    subroutine test_assumed_shape_dummy_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) procedure with assumed-shape dummy (rejected)...'
        errors = create_error_collection()
        src = 'subroutine s(a) bind(c)'//new_line('a')// &
              '  integer :: a(:)'//new_line('a')// &
              'end subroutine'
        call validate_procedures(src, errors)
        call expect_error(errors%has_errors(), &
                          'assumed-shape dummy not rejected', passed)
    end subroutine test_assumed_shape_dummy_rejected

    subroutine test_assumed_size_dummy_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) procedure with assumed-size dummy (rejected)...'
        errors = create_error_collection()
        src = 'subroutine s(a) bind(c)'//new_line('a')// &
              '  integer :: a(*)'//new_line('a')// &
              'end subroutine'
        call validate_procedures(src, errors)
        call expect_error(errors%has_errors(), &
                          'assumed-size dummy not rejected', passed)
    end subroutine test_assumed_size_dummy_rejected

    subroutine test_procedure_dummy_rejected(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing BIND(C) procedure with procedure dummy (rejected)...'
        errors = create_error_collection()
        src = 'subroutine s(f) bind(c)'//new_line('a')// &
              '  procedure(real) :: f'//new_line('a')// &
              'end subroutine'
        call validate_procedures(src, errors)
        call expect_error(errors%has_errors(), &
                          'procedure dummy not rejected', passed)
    end subroutine test_procedure_dummy_rejected

    subroutine test_valid_procedure_accepted(passed)
        logical, intent(inout) :: passed
        type(error_collection_t) :: errors
        character(len=:), allocatable :: src

        print *, 'Testing valid interoperable BIND(C) procedure (accepted)...'
        errors = create_error_collection()
        src = 'subroutine s(n, a) bind(c)'//new_line('a')// &
              '  integer :: n'//new_line('a')// &
              '  real :: a(10)'//new_line('a')// &
              'end subroutine'
        call validate_procedures(src, errors)
        call expect_no_error(errors%has_errors(), &
                             'valid procedure incorrectly rejected', passed)
    end subroutine test_valid_procedure_accepted

    subroutine expect_error(has_errors, fail_msg, passed)
        logical, intent(in) :: has_errors
        character(len=*), intent(in) :: fail_msg
        logical, intent(inout) :: passed

        if (has_errors) then
            print *, '  PASS'
        else
            print *, '  FAIL: '//fail_msg
            passed = .false.
        end if
    end subroutine expect_error

    subroutine expect_no_error(has_errors, fail_msg, passed)
        logical, intent(in) :: has_errors
        character(len=*), intent(in) :: fail_msg
        logical, intent(inout) :: passed

        if (has_errors) then
            print *, '  FAIL: '//fail_msg
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_no_error

end program test_bind_c_validation
