program test_compiler_resolved_type_query
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        resolved_type_query_t, query_resolved_type, &
        INPUT_MODE_STANDARD, OPERATING_MODE_STRICT, TINT, TREAL, TCOMPLEX, &
        is_literal, get_literal_info, is_identifier, get_identifier_name, &
        is_binary_op, get_binary_op_info, get_node_type_at, &
        is_declaration_node, get_declaration_var_name
    use ast_nodes_core, only: call_or_subscript_node
    implicit none

    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    call read_example( &
        'examples/f90/compiler_resolved_expression_kinds.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    options%operating_mode = OPERATING_MODE_STRICT
    call compile_frontend_from_string(source, result, options)

    call require(result%parse_ok, 'compiler API did not parse kind-query example')
    call require(result%semantic_ok, &
        'compiler API did not analyze kind-query example: '//result%error_msg)

    call require_literal_kind('1_1', TINT, 1)
    call require_literal_kind('2_2', TINT, 2)
    call require_literal_kind('4_4', TINT, 4)
    call require_literal_kind('8_8', TINT, 8)
    call require_literal_kind('1.0_4', TREAL, 4)
    call require_literal_kind('1.0_dp', TREAL, 8)
    call require_literal_kind('1.0_wp', TREAL, 16)
    call require_literal_kind('2.0_8', TREAL, 8)
    call require_literal_kind('2.0_16', TREAL, 16)
    call require_identifier_kind('i1', TINT, 1)
    call require_identifier_kind('i2', TINT, 2)
    call require_identifier_kind('i4', TINT, 4)
    call require_identifier_kind('i8', TINT, 8)
    call require_identifier_kind('r4', TREAL, 4)
    call require_identifier_kind('r8', TREAL, 8)
    call require_identifier_kind('r16', TREAL, 16)
    call require_identifier_kind('r8_explicit', TREAL, 8)
    call require_identifier_kind('r16_explicit', TREAL, 16)
    call require_identifier_kind('mixed', TREAL, 16)
    call require_declaration_kind('r8', TREAL, 8)
    call require_declaration_kind('r16', TREAL, 16)
    call require_binary_kind('+', TREAL, 16)
    call require_binary_kind('-', TREAL, 16)
    call require_function_reference_kind(TREAL, 16)
    call require_intrinsic_call_kind('real', 1, TREAL, 8)
    call require_intrinsic_call_kind('real', 2, TREAL, 4)
    call require_intrinsic_call_kind('cmplx', 3, TCOMPLEX, 8)
    call require_intrinsic_call_kind('aimag', 1, TREAL, 8)
    call require_intrinsic_call_kind('abs', 1, TREAL, 8)
    call require_shadowed_kind_selector()
    call require_unavailable_result()
    call require_unavailable_without_semantics(source)

    print *, 'PASS: compiler resolved type query preserves exact kinds'

contains

    include '../common/read_example.inc'

    subroutine require_literal_kind(expected_value, expected_type, expected_kind)
        character(len=*), intent(in) :: expected_value
        integer, intent(in) :: expected_type, expected_kind
        character(len=:), allocatable :: value, literal_type, error_msg
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i

        matched = .false.
        do i = 1, result%arena%size
            if (.not. is_literal(result%arena, i)) cycle
            call get_literal_info(result%arena, i, value, literal_type, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(value) /= expected_value) cycle
            query = query_resolved_type(result%arena, i)
            call require_query(query, expected_type, expected_kind, &
                'literal '//expected_value)
            matched = .true.
            exit
        end do
        call require(matched, 'literal not found: '//expected_value)
    end subroutine require_literal_kind

    subroutine require_identifier_kind(expected_name, expected_type, expected_kind)
        character(len=*), intent(in) :: expected_name
        integer, intent(in) :: expected_type, expected_kind
        character(len=:), allocatable :: name, error_msg
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i

        matched = .false.
        do i = 1, result%arena%size
            if (.not. is_identifier(result%arena, i)) cycle
            call get_identifier_name(result%arena, i, name, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(name) /= expected_name) cycle
            query = query_resolved_type(result%arena, i)
            if (.not. query%found) cycle
            if (query%type_kind /= expected_type) cycle
            if (query%kind_value /= expected_kind) cycle
            matched = .true.
            exit
        end do
        call require(matched, 'resolved identifier not found: '//expected_name)
    end subroutine require_identifier_kind

    subroutine require_declaration_kind(expected_name, expected_type, expected_kind)
        character(len=*), intent(in) :: expected_name
        integer, intent(in) :: expected_type, expected_kind
        character(len=:), allocatable :: name, error_msg
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i

        matched = .false.
        do i = 1, result%arena%size
            if (.not. is_declaration_node(result%arena, i)) cycle
            call get_declaration_var_name(result%arena, i, name, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(name) /= expected_name) cycle
            query = query_resolved_type(result%arena, i)
            call require_query(query, expected_type, expected_kind, &
                'declaration '//expected_name)
            matched = .true.
            exit
        end do
        call require(matched, 'resolved declaration not found: '//expected_name)
    end subroutine require_declaration_kind

    subroutine require_binary_kind(expected_operator, expected_type, expected_kind)
        character(len=*), intent(in) :: expected_operator
        integer, intent(in) :: expected_type, expected_kind
        character(len=:), allocatable :: operator, error_msg
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i, left_index, right_index, line, column

        matched = .false.
        do i = 1, result%arena%size
            if (.not. is_binary_op(result%arena, i)) cycle
            call get_binary_op_info(result%arena, i, operator, left_index, &
                right_index, line, column, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(operator) /= expected_operator) cycle
            query = query_resolved_type(result%arena, i)
            if (.not. query%found) cycle
            if (query%type_kind /= expected_type) cycle
            if (query%kind_value /= expected_kind) cycle
            matched = .true.
            exit
        end do
        call require(matched, &
            'resolved binary operator not found: '//expected_operator)
    end subroutine require_binary_kind

    subroutine require_function_reference_kind(expected_type, expected_kind)
        integer, intent(in) :: expected_type, expected_kind
        character(len=:), allocatable :: node_type
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i

        matched = .false.
        do i = 1, result%arena%size
            node_type = get_node_type_at(result%arena, i)
            if (node_type /= 'call_or_subscript') cycle
            query = query_resolved_type(result%arena, i)
            if (.not. query%found) cycle
            if (query%type_kind /= expected_type) cycle
            if (query%kind_value /= expected_kind) cycle
            matched = .true.
            exit
        end do
        call require(matched, 'resolved function reference not found')
    end subroutine require_function_reference_kind

    subroutine require_intrinsic_call_kind(expected_name, expected_arg_count, &
            expected_type, expected_kind)
        character(len=*), intent(in) :: expected_name
        integer, intent(in) :: expected_arg_count, expected_type, expected_kind
        type(resolved_type_query_t) :: query
        logical :: matched
        integer :: i

        matched = .false.
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (call_or_subscript_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) /= expected_name) cycle
                if (.not. allocated(node%arg_indices)) cycle
                if (size(node%arg_indices) /= expected_arg_count) cycle
                call require(node%is_intrinsic, &
                    expected_name//' was not marked as intrinsic')
                query = query_resolved_type(result%arena, i)
                call require_query(query, expected_type, expected_kind, &
                    expected_name//' intrinsic call')
                matched = .true.
                exit
            end select
        end do
        call require(matched, 'resolved intrinsic call not found: '//expected_name)
    end subroutine require_intrinsic_call_kind

    subroutine require_shadowed_kind_selector()
        character(len=*), parameter :: source = &
            'module unrelated'//new_line('a')// &
            '  integer, parameter :: dp = 8'//new_line('a')// &
            'end module unrelated'//new_line('a')// &
            'program main'//new_line('a')// &
            '  implicit none'//new_line('a')// &
            '  integer, parameter :: dp = 4'//new_line('a')// &
            '  real(dp) :: value'//new_line('a')// &
            '  value = 1.5_dp'//new_line('a')// &
            'end program main'
        type(compiler_frontend_result_t) :: shadowed_result
        type(resolved_type_query_t) :: query
        character(len=:), allocatable :: name, error_msg
        logical :: matched
        integer :: i

        call compile_frontend_from_string(source, shadowed_result, options)
        call require(shadowed_result%parse_ok, &
            'shadowed-kind query source did not parse')
        call require(shadowed_result%semantic_ok, &
            'shadowed-kind query source did not analyze: '// &
            shadowed_result%error_msg)

        matched = .false.
        do i = 1, shadowed_result%arena%size
            if (.not. is_declaration_node(shadowed_result%arena, i)) cycle
            call get_declaration_var_name(shadowed_result%arena, i, name, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(name) /= 'value') cycle
            query = query_resolved_type(shadowed_result%arena, i)
            call require_query(query, TREAL, 4, 'shadowed dp declaration')
            matched = .true.
            exit
        end do
        call require(matched, 'shadowed dp declaration was not found')
    end subroutine require_shadowed_kind_selector

    subroutine require_unavailable_result()
        type(resolved_type_query_t) :: query

        query = query_resolved_type(result%arena, result%arena%size + 1)
        call require(.not. query%found, 'invalid node unexpectedly resolved')
        call require(len_trim(query%diagnostic) > 0, &
            'invalid-node query did not return a diagnostic')
    end subroutine require_unavailable_result

    subroutine require_unavailable_without_semantics(source_code)
        character(len=*), intent(in) :: source_code
        type(compiler_frontend_options_t) :: parse_options
        type(compiler_frontend_result_t) :: parse_result
        type(resolved_type_query_t) :: query
        integer :: i

        parse_options%input_mode = INPUT_MODE_STANDARD
        parse_options%operating_mode = OPERATING_MODE_STRICT
        parse_options%run_semantics = .false.
        call compile_frontend_from_string(source_code, parse_result, parse_options)
        call require(parse_result%parse_ok, &
            'parse-only compiler API did not parse kind-query example')
        do i = 1, parse_result%arena%size
            if (.not. is_literal(parse_result%arena, i)) cycle
            query = query_resolved_type(parse_result%arena, i)
            call require(.not. query%found, &
                'parse-only node unexpectedly reported an exact type')
            call require(len_trim(query%diagnostic) > 0, &
                'parse-only query did not return a diagnostic')
            return
        end do
        call require(.false., 'parse-only arena did not contain a literal')
    end subroutine require_unavailable_without_semantics

    subroutine require_query(query, expected_type, expected_kind, label)
        type(resolved_type_query_t), intent(in) :: query
        integer, intent(in) :: expected_type, expected_kind
        character(len=*), intent(in) :: label
        integer :: expected_storage_bits

        call require(query%found, label//' did not resolve: '//query%diagnostic)
        call require(query%type_kind == expected_type, &
            label//' resolved to the wrong intrinsic category')
        call require(query%kind_value == expected_kind, &
            label//' resolved to the wrong kind')
        call require(query%rank == 0, label//' did not resolve as a scalar')
        expected_storage_bits = 8 * expected_kind
        if (expected_type == TCOMPLEX) then
            expected_storage_bits = 16 * expected_kind
        end if
        call require(query%storage_size_bits == expected_storage_bits, &
            label//' reported the wrong storage size')
    end subroutine require_query

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine require

end program test_compiler_resolved_type_query
