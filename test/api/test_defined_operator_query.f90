program test_defined_operator_query
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, is_binary_op, &
        get_binary_op_info, defined_operator_query_t, query_defined_operator
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(defined_operator_query_t) :: query
    character(len=:), allocatable :: source, op, error_msg
    integer :: i, left_index, right_index, line, column, status
    integer :: plus_count, global_count, pointer_count, neg_count
    logical :: saw_untyped

    call read_example('examples/f90/defined_operator_query.f90', source)
    call execute_command_line('gfortran -std=f2018 -pedantic -Wall -Wextra '// &
        '-fsyntax-only examples/f90/defined_operator_query.f90', &
        wait=.true., exitstat=status)
    call require(status == 0, 'GNU Fortran rejected the valid operator fixture')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'operator fixture was rejected: '// &
        trim(result%diagnostic_text))

    plus_count = 0
    global_count = 0
    pointer_count = 0
    neg_count = 0
    do i = 1, result%arena%size
        if (.not. is_binary_op(result%arena, i)) cycle
        call get_binary_op_info(result%arena, i, op, left_index, right_index, &
            line, column, error_msg)
        if (len_trim(error_msg) > 0) cycle
        query = query_defined_operator(result%arena, i)
        if (.not. query%found) cycle
        select case (trim(op))
        case ('.plus.')
            if (right_index <= 0) cycle
            if (query%has_global_mutable_state) then
                global_count = global_count + 1
                call require(query%is_refused .and. query%is_unresolved .and. &
                    query%selected_procedure_node_index == 0 .and. &
                    index(query%refusal_reason, 'global') > 0, &
                    'global operator operand was not explicitly refused')
            else if (query%has_pointer_operand) then
                pointer_count = pointer_count + 1
                call require(query%is_refused .and. query%is_unresolved .and. &
                    query%selected_procedure_node_index == 0 .and. &
                    index(query%refusal_reason, 'pointer') > 0, &
                    'pointer operator operand was not explicitly refused')
            else
                plus_count = plus_count + 1
                call require(query%is_resolved .and. .not. query%is_refused .and. &
                    query%selected_procedure_node_index > 0 .and. &
                    size(query%candidates) == 2 .and. &
                    query%candidates(1)%is_match .and. &
                    .not. query%candidates(2)%is_match .and. &
                    query%candidates(2)%has_conversion, &
                    'exact operator candidate selection is incomplete')
                call require(query%candidates(1)%operands(1)%actual_type_known .and. &
                    query%candidates(1)%operands(1)%formal_type_known .and. &
                    query%candidates(1)%operands(1)%actual_rank == 0 .and. &
                    query%candidates(1)%operands(1)%formal_rank == 0 .and. &
                    query%candidates(1)%operands(1)%is_exact .and. &
                    query%candidates(1)%operands(2)%is_exact, &
                    'exact operand type/kind/rank facts are incomplete')
            end if
        case ('.neg.')
            neg_count = neg_count + 1
            call require(query%is_unary .and. .not. query%is_binary .and. &
                query%is_resolved .and. size(query%candidates) == 1 .and. &
                query%candidates(1)%is_match, &
                'unary defined operator was not selected exactly')
        end select
    end do

    call require(plus_count == 1 .and. global_count == 1 .and. &
        pointer_count == 1 .and. neg_count == 1, &
        'operator exact/refusal coverage is incomplete')

    ! Independent unknown-type boundary: GNU accepts the same source, but an
    ! API consumer that deliberately skips semantic inference must not guess a
    ! selected operator from syntax alone.
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%parse_ok, 'operator fixture did not parse without semantics')
    saw_untyped = .false.
    do i = 1, result%arena%size
        if (.not. is_binary_op(result%arena, i)) cycle
        call get_binary_op_info(result%arena, i, op, left_index, right_index, &
            line, column, error_msg)
        if (len_trim(error_msg) > 0 .or. trim(op) /= '.plus.') cycle
        query = query_defined_operator(result%arena, i)
        if (query%found .and. query%has_unknown_types) then
            saw_untyped = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%selected_procedure_node_index == 0 .and. &
                index(query%refusal_reason, 'unknown') > 0, &
                'unknown operator operand types were not refused')
            exit
        end if
    end do
    call require(saw_untyped, 'unknown operator type boundary was not observed')

    call check_refusal_fixture( &
        'examples/f90/defined_operator_ambiguous_refusal.f90', '.amb.', &
        .true., .false.)
    call check_refusal_fixture( &
        'examples/f90/defined_operator_conversion_refusal.f90', '.conv.', &
        .false., .true.)

    print *, 'PASS: defined-operator exact selection and refusal contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (error_unit, '(A)') 'FAIL: '//trim(message)
            error stop 1
        end if
    end subroutine require

    subroutine check_refusal_fixture(path, wanted_operator, expect_ambiguous, &
            expect_conversion)
        character(len=*), intent(in) :: path, wanted_operator
        logical, intent(in) :: expect_ambiguous, expect_conversion
        character(len=:), allocatable :: invalid_source, command, local_op
        integer :: syntax_status, j, local_left, local_right, local_line
        integer :: local_column
        logical :: observed

        call read_example(path, invalid_source)
        command = 'gfortran -std=f2018 -pedantic -Wall -Wextra -fsyntax-only '// &
            trim(path)
        call execute_command_line(command, wait=.true., exitstat=syntax_status)
        call require(syntax_status /= 0, &
            'GNU Fortran unexpectedly accepted a refusal fixture')

        options%run_semantics = .true.
        call compile_frontend_from_string(invalid_source, result, options)
        call require(result%parse_ok, 'refusal fixture did not parse')
        observed = .false.
        do j = 1, result%arena%size
            if (.not. is_binary_op(result%arena, j)) cycle
            call get_binary_op_info(result%arena, j, local_op, local_left, &
                local_right, local_line, local_column, error_msg)
            if (len_trim(error_msg) > 0 .or. trim(local_op) /= wanted_operator) &
                cycle
            query = query_defined_operator(result%arena, j)
            if (.not. query%found) cycle
            observed = .true.
            call require(query%is_refused .and. query%is_unresolved .and. &
                query%selected_procedure_node_index == 0, &
                'operator refusal did not remain unresolved')
            if (expect_ambiguous) then
                call require(query%is_ambiguous .and. &
                    index(query%refusal_reason, 'ambiguous') > 0, &
                    'ambiguous operator was not explicitly refused')
            end if
            if (expect_conversion) then
                call require(query%has_conversion .and. &
                    index(query%refusal_reason, 'conversion') > 0, &
                    'conversion operator was not explicitly refused')
            end if
            exit
        end do
        call require(observed, 'refusal fixture operator was not queried')
    end subroutine check_refusal_fixture

end program test_defined_operator_query
