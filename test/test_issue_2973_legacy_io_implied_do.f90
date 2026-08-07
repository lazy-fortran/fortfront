program test_issue_2973_legacy_io_implied_do
    ! A legacy array constructor in an I/O list begins with `(/`.  It must
    ! remain an array_literal_node; a genuine `(expr, var=lo, hi)` item must
    ! remain an io_implied_do_node.  This catches the old slash-as-division
    ! recovery path, which left a malformed binary_op_node in the AST.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, io_statement_query_t, query_io_statement, &
        IO_STATEMENT_PRINT
    use ast_nodes_core, only: array_literal_node, identifier_node, literal_node
    use ast_nodes_io, only: io_implied_do_node
    use ast_nodes_loops, only: do_loop_node
    implicit none

    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(io_statement_query_t) :: query
    integer :: print_items(2), print_count, i

    source = 'program p'//new_line('a')// &
        '  integer :: i'//new_line('a')// &
        '  print *, (/(i, i=1, 4)/)'//new_line('a')// &
        '  print *, (i, i=1, 4)'//new_line('a')// &
        'end program p'//new_line('a')

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    options%standardize = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        call fail('frontend rejected valid legacy and I/O implied-do source')
    end if

    print_count = 0
    do i = 1, result%arena%size
        query = query_io_statement(result%arena, i)
        if (.not. query%found .or. query%statement_kind /= IO_STATEMENT_PRINT) cycle
        print_count = print_count + 1
        if (print_count > size(print_items)) call fail('unexpected PRINT count')
        if (size(query%item_node_indices) /= 1) then
            call fail('expected one item in each PRINT statement')
        end if
        print_items(print_count) = query%item_node_indices(1)
    end do
    call require_equal(print_count, 2, 'PRINT count')

    call check_legacy_constructor(result, print_items(1))
    call check_io_implied_do(result, print_items(2))
    print '(A)', 'PASS: #2973 distinguishes legacy array and I/O implied-do ASTs'

contains

    subroutine check_legacy_constructor(frontend, node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index

        if (node_index <= 0 .or. node_index > frontend%arena%size) then
            call fail('legacy constructor item index is invalid')
        end if
        if (.not. allocated(frontend%arena%entries(node_index)%node)) then
            call fail('legacy constructor item is missing')
        end if
        select type (node => frontend%arena%entries(node_index)%node)
            type is (array_literal_node)
            call require_string(node%syntax_style, 'implied_do', &
                'legacy constructor syntax style')
            if (.not. allocated(node%element_indices)) then
                call fail('legacy constructor elements are missing')
            end if
            call require_equal(size(node%element_indices), 1, &
                'legacy constructor element count')
            call check_do_loop(frontend, node%element_indices(1), 'legacy')
        class default
            call fail('legacy constructor was not an array_literal_node')
        end select
    end subroutine check_legacy_constructor

    subroutine check_do_loop(frontend, node_index, label)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: label

        if (.not. allocated(frontend%arena%entries(node_index)%node)) then
            call fail(trim(label)//' implied-do loop is missing')
        end if
        select type (loop => frontend%arena%entries(node_index)%node)
            type is (do_loop_node)
            call require_string(loop%var_name, 'i', trim(label)//' loop variable')
            call require_literal(frontend, loop%start_expr_index, '1', &
                trim(label)//' loop lower bound')
            call require_literal(frontend, loop%end_expr_index, '4', &
                trim(label)//' loop upper bound')
            if (loop%step_expr_index /= 0) call fail(trim(label)// &
                ' loop unexpectedly has a stride')
        class default
            call fail(trim(label)//' constructor element was not a do_loop_node')
        end select
    end subroutine check_do_loop

    subroutine check_io_implied_do(frontend, node_index)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index

        if (.not. allocated(frontend%arena%entries(node_index)%node)) then
            call fail('I/O implied-do item is missing')
        end if
        select type (node => frontend%arena%entries(node_index)%node)
            type is (io_implied_do_node)
            call require_string(node%var_name, 'i', 'I/O loop variable')
            call require_identifier(frontend, node%expr_index, 'i', &
                'I/O implied-do value')
            call require_literal(frontend, node%start_expr_index, '1', &
                'I/O loop lower bound')
            call require_literal(frontend, node%end_expr_index, '4', &
                'I/O loop upper bound')
            if (node%step_expr_index /= 0) then
                call fail('I/O implied-do unexpectedly has a stride')
            end if
        class default
            call fail('genuine I/O implied-do was not an io_implied_do_node')
        end select
    end subroutine check_io_implied_do

    subroutine require_identifier(frontend, node_index, expected, label)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected, label

        if (node_index <= 0 .or. node_index > frontend%arena%size) then
            call fail(trim(label)//' index is invalid')
        end if
        select type (node => frontend%arena%entries(node_index)%node)
            type is (identifier_node)
            call require_string(node%name, expected, label)
        class default
            call fail(trim(label)//' was not an identifier_node')
        end select
    end subroutine require_identifier

    subroutine require_literal(frontend, node_index, expected, label)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected, label

        if (node_index <= 0 .or. node_index > frontend%arena%size) then
            call fail(trim(label)//' index is invalid')
        end if
        select type (node => frontend%arena%entries(node_index)%node)
            type is (literal_node)
            call require_string(node%value, expected, label)
        class default
            call fail(trim(label)//' was not a literal_node')
        end select
    end subroutine require_literal

    subroutine require_equal(actual, expected, label)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: label

        if (actual /= expected) then
            write (error_unit, '(A,I0,A,I0)') trim(label)//': got ', actual, &
                ', expected ', expected
            error stop 1
        end if
    end subroutine require_equal

    subroutine require_string(actual, expected, label)
        character(len=*), intent(in) :: actual, expected, label

        if (trim(actual) /= trim(expected)) then
            call fail(trim(label)//': got "'//trim(actual)//'", expected "'// &
                trim(expected)//'"')
        end if
    end subroutine require_string

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_issue_2973_legacy_io_implied_do
