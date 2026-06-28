program test_issue_2838_unary_operator_operand
    ! Issue #2838: a unary (prefix) user-defined operator (.negation. x) must
    ! produce an AST whose operand is a resolvable arena node, so a backend can
    ! walk the operator call like any other. The crash was a dangling operand
    ! index reported as "argument index does not reference an AST node".
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, &
        compile_frontend_from_string, &
        is_binary_op, get_binary_op_info, &
        get_identifier_name, INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: source, error_msg, operator, operand_name
    integer :: i, left_index, right_index, line, column
    integer :: found_count, operand_index

    call read_example('examples/f90/issue_2838_unary_defined_operator.f90', source)

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .true.
    options%standardize = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        write (error_unit, '(A)') 'FAIL: frontend rejected source: '// &
            trim(result%diagnostic_text)
        error stop 1
    end if

    found_count = 0
    operand_index = 0
    do i = 1, result%arena%size
        if (.not. is_binary_op(result%arena, i)) cycle
        call get_binary_op_info(result%arena, i, operator, left_index, &
            right_index, line, column, error_msg)
        if (len_trim(error_msg) > 0) cycle
        if (.not. allocated(operator)) cycle
        if (trim(operator) /= '.negation.') cycle
        found_count = found_count + 1
        ! A unary defined operator carries its single operand on the right; the
        ! left slot is the unary marker (0), like .not.
        if (left_index /= 0) then
            write (error_unit, '(A,I0)') &
                'FAIL: unary operator left slot is not the unary marker: ', &
                left_index
            error stop 1
        end if
        operand_index = right_index
    end do

    if (found_count /= 1) then
        write (error_unit, '(A,I0)') &
            'FAIL: expected one .negation. operator node, got ', found_count
        error stop 1
    end if

    ! The exact backend operation that crashed in #2838: resolve the operand.
    call get_identifier_name(result%arena, operand_index, operand_name, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: unary operator operand unresolvable: '// &
            trim(error_msg)
        error stop 1
    end if
    if (trim(operand_name) /= 'x') then
        write (error_unit, '(A)') 'FAIL: operand name is "'//trim(operand_name)// &
            '", expected "x"'
        error stop 1
    end if

    print *, 'PASS: unary defined operator operand resolves to a valid node'

contains

    include 'common/read_example.inc'

end program test_issue_2838_unary_operator_operand
