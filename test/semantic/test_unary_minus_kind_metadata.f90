program test_unary_minus_kind_metadata
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    use ast_nodes_core, only: binary_op_node
    implicit none
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    integer, parameter :: expected_kinds(6) = [1, 1, 1, 2, 4, 8]
    integer :: i, unary_count, subtraction_count
    character(len=1), parameter :: nl = new_line('a')

    ! gfortran KIND(-operand) gives the operand kind, including array operands;
    ! true zero subtraction promotes an integer(1) operand to default integer.
    options%input_mode = INPUT_MODE_STANDARD
    call compile_frontend_from_string('program p'//nl// &
        'integer(1) n'//nl//'integer(2) a(2)'//nl//'real(4) r'//nl// &
        'real(8) d'//nl//'print *, -1_1, -n, 0-n, -(n+1_1), -a, -r, -d'//nl// &
        'end program p', result, options)
    if (.not. result%success()) then
        write (error_unit, '(a)') result%diagnostic_text
        stop 1
    end if

    unary_count = 0
    subtraction_count = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        select type (node => result%arena%entries(i)%node)
            type is (binary_op_node)
            if (node%is_unary_minus) then
                unary_count = unary_count + 1
                call require(unary_count <= size(expected_kinds), 'unary count')
                call require(node%resolved_type_found, 'unary type is resolved')
                call require(node%resolved_kind_value == expected_kinds(unary_count), &
                    'unary minus preserves operand kind')
                if (unary_count == 4) then
                    call require(node%resolved_rank == 1, 'unary array rank')
                end if
                block
                    type(binary_op_node) :: copy
                    copy = node
                    call require(copy%is_unary_minus, 'AST copy preserves unary form')
                end block
            else if (node%operator == '-') then
                subtraction_count = subtraction_count + 1
                call require(node%resolved_kind_value == 4, &
                    'binary subtraction retains numeric promotion')
            end if
        end select
    end do
    call require(unary_count == size(expected_kinds), 'all unary kinds checked')
    call require(subtraction_count == 1, 'binary subtraction checked')
    print *, 'PASS: unary-minus kind and rank metadata'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a)') 'FAIL: '//message
        stop 1
    end subroutine require

end program test_unary_minus_kind_metadata
