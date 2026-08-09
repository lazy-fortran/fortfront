program test_defined_operator_query_boundary
    use iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, get_node_type_at, is_binary_op, &
        get_binary_op_info, defined_operator_query_t, &
        query_defined_operator_into
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(defined_operator_query_t) :: query
    character(len=:), allocatable :: source, op, error_msg
    integer :: i, left_index, right_index, line, column
    logical :: observed

    call read_example('examples/f90/defined_operator_query_boundary.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'valid operator boundary fixture rejected: '// &
        trim(result%diagnostic_text))

    observed = .false.
    do i = 1, result%arena%size
        if (.not. is_binary_op(result%arena, i)) cycle
        call get_binary_op_info(result%arena, i, op, left_index, right_index, &
            line, column, error_msg)
        if (len_trim(error_msg) > 0 .or. trim(op) /= '.blend.') cycle
        call query_defined_operator_into(result%arena, i, query)
        call require(query%is_resolved .and. .not. query%is_refused .and. &
            query%selected_procedure_node_index > 0 .and. &
            size(query%candidates) == 2, &
            'out query did not preserve exact operator selection')
        call require(query%candidates(1)%is_match .and. &
            .not. query%candidates(2)%is_match .and. &
            query%candidates(2)%has_conversion, &
            'out query did not preserve conversion refusal facts')
        observed = .true.
        exit
    end do
    call require(observed, 'exact operator expression was not queried')

    print *, 'PASS: NVHPC-safe defined-operator out query boundary'

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

end program test_defined_operator_query_boundary
