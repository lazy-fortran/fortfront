program test_generic_resolution_query
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        generic_call_query_t, query_generic_call, INPUT_MODE_STANDARD, &
        TINT, TREAL
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(generic_call_query_t) :: query
    character(len=:), allocatable :: source
    integer :: i, scale_calls

    call read_example('examples/f90/generic_resolution_query.f90', source)
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        print *, 'FAIL: generic example was rejected: ', &
            trim(result%diagnostic_text)
        error stop 1
    end if

    scale_calls = 0
    do i = 1, result%arena%size
        query = query_generic_call(result%arena, i)
        if (.not. query%found) cycle
        if (trim(query%generic_name) /= 'scale') error stop 1
        scale_calls = scale_calls + 1
        call assert_candidates(query)
        if (query%is_ambiguous) error stop 1
        if (query%selected_procedure_node_index <= 0) error stop 1
        if (scale_calls == 1) then
            if (trim(query%candidates(1)%procedure_name) /= 'scale_int') &
                error stop 1
            if (.not. query%candidates(1)%is_match) error stop 1
            if (query%candidates(2)%is_match) error stop 1
        else if (scale_calls == 2) then
            if (trim(query%candidates(2)%procedure_name) /= 'scale_real') &
                error stop 1
            if (.not. query%candidates(2)%is_match) error stop 1
            if (query%candidates(1)%is_match) error stop 1
        end if
    end do

    if (scale_calls /= 2) error stop 1
    print *, 'PASS: generic query exposes exact type-kind-rank candidate selection'

contains

    include '../common/read_example.inc'

    subroutine assert_candidates(call_query)
        type(generic_call_query_t), intent(in) :: call_query

        if (size(call_query%candidates) /= 2) error stop 1
        if (size(call_query%candidates(1)%arguments) /= 1) error stop 1
        if (size(call_query%candidates(2)%arguments) /= 1) error stop 1
        if (.not. call_query%candidates(1)%arguments(1)%found) error stop 1
        if (.not. call_query%candidates(2)%arguments(1)%found) error stop 1
        if (call_query%candidates(1)%arguments(1)%type_kind /= TINT) &
            error stop 1
        if (call_query%candidates(1)%arguments(1)%kind_value /= 4) &
            error stop 1
        if (call_query%candidates(1)%arguments(1)%rank /= 0) error stop 1
        if (call_query%candidates(2)%arguments(1)%type_kind /= TREAL) &
            error stop 1
        if (call_query%candidates(2)%arguments(1)%kind_value /= 8) &
            error stop 1
        if (call_query%candidates(2)%arguments(1)%rank /= 0) error stop 1
    end subroutine assert_candidates

end program test_generic_resolution_query
