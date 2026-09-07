program test_inquire_nested_specifiers
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, io_statement_query_t, query_io_statement, &
        IO_STATEMENT_INQUIRE, IO_STATEMENT_PRINT
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
        identifier_node
    use fortfront_transform, only: transform_lazy_fortran_string
    implicit none

    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options
    character(len=:), allocatable :: source, output, error_msg

    options%input_mode = INPUT_MODE_STANDARD
    options%standardize = .false.
    call read_example('examples/f90/inquire_nested_specifiers.f90', source)
    call compile_frontend_from_string(source, result, options)
    call check_statements(result)
    call transform_lazy_fortran_string(source, output, error_msg)
    if (len_trim(error_msg) /= 0) then
        print *, error_msg
        error stop 'INQUIRE source transformation failed'
    end if
    call compile_frontend_from_string(output, result, options)
    call check_statements(result)
    print *, 'PASS: nested INQUIRE specifiers and following statements'

contains

    include '../common/read_example.inc'

    subroutine check_statements(frontend)
        type(compiler_frontend_result_t), intent(in) :: frontend
        type(io_statement_query_t) :: query
        integer :: i, inquiries, prints, assignments

        if (.not. frontend%success()) error stop 'valid INQUIRE was rejected'
        inquiries = 0
        prints = 0
        assignments = 0
        do i = 1, frontend%arena%size
            if (.not. frontend%arena%has_node_at(i)) cycle
            select type (node => frontend%arena%entries(i)%node)
            type is (assignment_node)
                assignments = assignments + 1
            end select
            query = query_io_statement(frontend%arena, i)
            if (.not. query%found) cycle
            select case (query%statement_kind)
            case (IO_STATEMENT_INQUIRE)
                inquiries = inquiries + 1
                call check_specifiers(frontend, query, inquiries)
            case (IO_STATEMENT_PRINT)
                prints = prints + 1
                if (size(query%item_node_indices) /= 1) error stop 'PRINT item lost'
            end select
        end do
        if (inquiries /= 2) error stop 'INQUIRE statement boundary lost'
        if (prints /= 2) error stop 'following PRINT statement lost'
        if (assignments /= 1) error stop 'specifier became a spurious assignment'
    end subroutine check_statements

    subroutine check_specifiers(frontend, query, ordinal)
        type(compiler_frontend_result_t), intent(in) :: frontend
        type(io_statement_query_t), intent(in) :: query
        integer, intent(in) :: ordinal
        integer :: value_index

        if (.not. allocated(query%specifiers)) error stop 'specifiers missing'
        if (size(query%specifiers) /= 2) error stop 'specifier list truncated'
        if (query%specifiers(1)%name /= 'file') error stop 'FILE identity lost'
        if (query%specifiers(2)%name /= 'exist') error stop 'EXIST identity lost'
        if (.not. query%specifiers(1)%has_value_node) error stop 'FILE AST lost'
        if (.not. query%specifiers(2)%has_value_node) error stop 'EXIST AST lost'
        value_index = query%specifiers(2)%value_node_index
        select type (node => frontend%arena%entries(value_index)%node)
        type is (identifier_node)
            if (node%name /= 'found') error stop 'EXIST output identity lost'
        class default
            error stop 'EXIST output is not an identifier'
        end select
        value_index = query%specifiers(1)%value_node_index
        if (ordinal == 1) then
            value_index = check_call(frontend, value_index, 'trim')
            value_index = check_call(frontend, value_index, 'adjustl')
            select type (node => frontend%arena%entries(value_index)%node)
            type is (identifier_node)
                if (node%name /= 'filename') error stop 'nested argument lost'
            class default
                error stop 'nested argument is not an identifier'
            end select
        else
            if (query%specifiers(1)%value /= "')'") then
                error stop 'parenthesis string literal changed'
            end if
        end if
    end subroutine check_specifiers

    integer function check_call(frontend, node_index, name) result(argument)
        type(compiler_frontend_result_t), intent(in) :: frontend
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name

        argument = 0
        select type (node => frontend%arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            if (node%name /= name) error stop 'INQUIRE call identity lost'
            if (.not. allocated(node%arg_indices)) error stop 'call argument lost'
            if (size(node%arg_indices) /= 1) error stop 'wrong call argument count'
            argument = node%arg_indices(1)
        class default
            error stop 'INQUIRE expression is not a call'
        end select
    end function check_call

end program test_inquire_nested_specifiers
