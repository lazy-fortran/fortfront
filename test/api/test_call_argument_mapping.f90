program test_call_argument_mapping
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, call_arguments_query_t, query_call_arguments
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(call_arguments_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, apply_calls, evaluate_calls

    call read_example('examples/f90/call_argument_mapping.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, 'FAIL: call mapping example was rejected: ', &
            trim(result%diagnostic_text)
        error stop 1
    end if

    apply_calls = 0
    evaluate_calls = 0
    do i = 1, result%arena%size
        query = query_call_arguments(result%arena, i)
        if (.not. query%found) cycle
        select case (trim(query%procedure_name))
        case ('apply')
            apply_calls = apply_calls + 1
            if (apply_calls == 1) then
                call assert_apply_keyword_call(query)
            else if (apply_calls == 2) then
                call assert_apply_positional_call(query)
            end if
        case ('evaluate')
            evaluate_calls = evaluate_calls + 1
            call assert_evaluate_call(query)
        end select
    end do

    if (apply_calls /= 2) then
        print *, 'FAIL: expected two resolved apply calls, got ', apply_calls
        error stop 1
    end if
    if (evaluate_calls /= 1) then
        print *, 'FAIL: expected one resolved evaluate call, got ', evaluate_calls
        error stop 1
    end if

    print *, 'PASS: call argument query maps positional, keyword, and omitted optionals'

contains

    include '../common/read_example.inc'

    subroutine assert_apply_keyword_call(call_query)
        type(call_arguments_query_t), intent(in) :: call_query

        call assert_argument_shape(call_query, 'apply')
        if (.not. call_query%arguments(1)%is_supplied) error stop 1
        if (.not. call_query%arguments(1)%is_keyword) error stop 1
        if (trim(call_query%arguments(1)%formal_name) /= 'value') error stop 1
        if (.not. call_query%arguments(2)%is_supplied) error stop 1
        if (.not. call_query%arguments(2)%is_keyword) error stop 1
        if (trim(call_query%arguments(2)%formal_name) /= 'scale') error stop 1
        if (call_query%arguments(3)%is_supplied) error stop 1
        if (.not. call_query%arguments(3)%is_optional) error stop 1
        call assert_supplied_value_indices(call_query, 2)
    end subroutine assert_apply_keyword_call

    subroutine assert_apply_positional_call(call_query)
        type(call_arguments_query_t), intent(in) :: call_query

        call assert_argument_shape(call_query, 'apply')
        if (.not. call_query%arguments(1)%is_supplied) error stop 1
        if (call_query%arguments(1)%is_keyword) error stop 1
        if (trim(call_query%arguments(1)%formal_name) /= 'value') error stop 1
        if (call_query%arguments(2)%is_supplied) error stop 1
        if (.not. call_query%arguments(2)%is_optional) error stop 1
        if (call_query%arguments(3)%is_supplied) error stop 1
        if (.not. call_query%arguments(3)%is_optional) error stop 1
        call assert_supplied_value_indices(call_query, 1)
    end subroutine assert_apply_positional_call

    subroutine assert_evaluate_call(call_query)
        type(call_arguments_query_t), intent(in) :: call_query

        call assert_argument_shape(call_query, 'evaluate')
        if (.not. call_query%arguments(1)%is_supplied) error stop 1
        if (.not. call_query%arguments(1)%is_keyword) error stop 1
        if (trim(call_query%arguments(1)%formal_name) /= 'value') error stop 1
        if (.not. call_query%arguments(2)%is_supplied) error stop 1
        if (.not. call_query%arguments(2)%is_keyword) error stop 1
        if (trim(call_query%arguments(2)%formal_name) /= 'scale') error stop 1
        if (call_query%arguments(3)%is_supplied) error stop 1
        if (.not. call_query%arguments(3)%is_optional) error stop 1
        call assert_supplied_value_indices(call_query, 2)
    end subroutine assert_evaluate_call

    subroutine assert_argument_shape(call_query, procedure_name)
        type(call_arguments_query_t), intent(in) :: call_query
        character(len=*), intent(in) :: procedure_name

        if (.not. call_query%found) error stop 1
        if (trim(call_query%procedure_name) /= procedure_name) error stop 1
        if (size(call_query%arguments) /= 3) error stop 1
        if (call_query%procedure_node_index <= 0) error stop 1
        if (call_query%call_node_index <= 0) error stop 1
    end subroutine assert_argument_shape

    subroutine assert_supplied_value_indices(call_query, count)
        type(call_arguments_query_t), intent(in) :: call_query
        integer, intent(in) :: count
        integer :: j

        do j = 1, count
            if (call_query%arguments(j)%actual_node_index <= 0) error stop 1
            if (call_query%arguments(j)%actual_value_node_index <= 0) error stop 1
            if (call_query%arguments(j)%formal_node_index <= 0) error stop 1
        end do
    end subroutine assert_supplied_value_indices

end program test_call_argument_mapping
