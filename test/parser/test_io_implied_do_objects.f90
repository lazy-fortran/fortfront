program test_io_implied_do_objects
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, io_statement_query_t, query_io_statement, &
        IO_STATEMENT_PRINT, IO_STATEMENT_WRITE, IO_STATEMENT_READ
    use ast_nodes_io, only: io_implied_do_node
    use ast_nodes_core, only: identifier_node, binary_op_node
    implicit none

    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(io_statement_query_t) :: query
    integer :: i, statement_count

    call read_example('examples/f90/io_implied_do_objects.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    options%standardize = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        call fail('valid nested/multiple-object I/O implied-do rejected: '// &
            result%diagnostic_text)
    end if

    statement_count = 0
    do i = 1, result%arena%size
        query = query_io_statement(result%arena, i)
        if (.not. query%found) cycle
        select case (query%statement_kind)
        case (IO_STATEMENT_PRINT, IO_STATEMENT_READ)
            statement_count = statement_count + 1
            if (size(query%item_node_indices) /= 1) call fail('nested I/O item count')
            call check_nested(query%item_node_indices(1))
        case (IO_STATEMENT_WRITE)
            statement_count = statement_count + 1
            if (size(query%item_node_indices) /= 1) call fail('multiple I/O item count')
            call check_multiple(query%item_node_indices(1))
        end select
    end do
    if (statement_count /= 3) call fail('PRINT/WRITE/READ were not all preserved')
    call check_rejected('print *, (i, -i, i=1,)')
    call check_rejected('print *, (i, -i, i=1,2,)')
    call check_rejected('print *, ((i, i=1,2), j=1,2')
    print '(A)', 'PASS: nested and multiple I/O implied-do objects preserve order'

contains

    subroutine check_rejected(statement)
        character(len=*), intent(in) :: statement
        type(compiler_frontend_result_t) :: invalid

        call compile_frontend_from_string(statement//new_line('a'), invalid, options)
        if (invalid%parse_ok) call fail('malformed implied-do accepted: '//statement)
    end subroutine check_rejected

    subroutine check_nested(node_index)
        integer, intent(in) :: node_index
        integer :: inner_index

        call require_index(node_index)
        select type (node => result%arena%entries(node_index)%node)
            type is (io_implied_do_node)
            if (node%var_name /= 'j') call fail('outer iterator is not j')
            if (.not. allocated(node%object_indices)) call fail('outer objects missing')
            if (size(node%object_indices) /= 1) call fail('outer object count')
            inner_index = node%object_indices(1)
            if (node%expr_index /= inner_index) call fail('outer first-object alias')
        class default
            call fail('outer item is not an I/O implied-do')
        end select
        call require_index(inner_index)
        select type (inner => result%arena%entries(inner_index)%node)
            type is (io_implied_do_node)
            if (inner%var_name /= 'i') call fail('inner iterator is not i')
            if (.not. allocated(inner%object_indices)) call fail('inner objects missing')
            if (size(inner%object_indices) /= 1) call fail('inner object count')
            if (inner%expr_index /= inner%object_indices(1)) then
                call fail('inner first-object alias')
            end if
        class default
            call fail('nested object is not an I/O implied-do')
        end select
    end subroutine check_nested

    subroutine check_multiple(node_index)
        integer, intent(in) :: node_index
        integer :: first_index, second_index

        call require_index(node_index)
        select type (node => result%arena%entries(node_index)%node)
            type is (io_implied_do_node)
            if (node%var_name /= 'i') call fail('multiple-object iterator is not i')
            if (.not. allocated(node%object_indices)) call fail('multiple objects missing')
            if (size(node%object_indices) /= 2) call fail('multiple object count')
            first_index = node%object_indices(1)
            second_index = node%object_indices(2)
            if (node%expr_index /= first_index) call fail('multiple first-object alias')
            if (node%step_expr_index <= 0) call fail('negative stride was dropped')
        class default
            call fail('multiple-object item is not an I/O implied-do')
        end select
        call require_index(first_index)
        select type (first => result%arena%entries(first_index)%node)
            type is (identifier_node)
            if (first%name /= 'i') call fail('first object value')
        class default
            call fail('first object is not an identifier')
        end select
        call require_index(second_index)
        select type (second => result%arena%entries(second_index)%node)
            type is (binary_op_node)
            if (second%operator /= '-') call fail('second object value')
            if (.not. second%is_unary_minus) call fail('second object unary minus')
        class default
            call fail('second object is not a negated expression')
        end select
    end subroutine check_multiple

    subroutine require_index(node_index)
        integer, intent(in) :: node_index

        if (node_index <= 0) call fail('nonpositive AST index')
        if (node_index > result%arena%size) call fail('AST index out of range')
        if (.not. allocated(result%arena%entries(node_index)%node)) then
            call fail('missing AST object')
        end if
    end subroutine require_index

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//message
        error stop 1
    end subroutine fail

    include '../common/read_example.inc'

end program test_io_implied_do_objects
