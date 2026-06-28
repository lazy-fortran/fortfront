program test_tmp_debug
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, token_t
    use ast_nodes_control, only: if_node
    use ast_nodes_core, only: assignment_node, identifier_node, literal_node, &
        program_node
    use ast_nodes_misc, only: implicit_statement_node, contains_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node
    use standardizer_core, only: standardize_ast
    use standardizer_declarations_insertion, only: find_declaration_insertion_point, &
        find_declaration_header_end
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: root_index
    character(len=:), allocatable :: lazy_code
    integer :: i

    lazy_code = 'x = 5' // new_line('A') // &
        'function test()' // new_line('A') // &
        '  y = 10' // new_line('A') // &
        '  return y' // new_line('A') // &
        'end function'

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_string(lazy_code, arena, root_index, error_msg, &
        options, tokens)

    call standardize_ast(arena, root_index)

    select type (root => arena%entries(root_index)%node)
        type is (program_node)
        write (*,'(A,I0)') 'implicit_insert_pos=', &
            find_declaration_insertion_point(arena, root)
        write (*,'(A,I0)') 'header_insert_pos=', &
            find_declaration_header_end(arena, root)
        if (allocated(root%body_indices)) then
            write (*,'(A,*(I0,1X))') 'Program body indices:', root%body_indices
        else
            write (*,'(A)') 'Program body indices: <not allocated>'
        end if
    end select

    write (*,'(A,I0)') 'Arena size: ', arena%size

    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (node => arena%entries(i)%node)
            type is (if_node)
            write (*,'(A,I0)') 'Found if_node at index ', i
            type is (assignment_node)
            write (*,'(A,I0,A,I0)') 'Assignment index ', i, ' target=', &
                node%target_index
            write (*,'(A,I0)') '  value_index=', node%value_index
            type is (identifier_node)
            if (allocated(node%name)) then
                write (*,'(A,I0,2A)') 'Identifier index ', i, ' name=', trim(node%name)
            end if
            type is (literal_node)
            if (allocated(node%value)) then
                write (*,'(A,I0,2A)') 'Literal index ', i, ' value=', trim(node%value)
            end if
            type is (implicit_statement_node)
            write (*,'(A,I0)') 'Implicit statement index ', i
            type is (contains_node)
            write (*,'(A,I0)') 'Contains node index ', i
            type is (function_def_node)
            write (*,'(A,I0)') 'Function definition index ', i
            type is (declaration_node)
            write (*,'(A,I0,2A)') 'Declaration index ', i, ' var=', &
                trim(node%var_name)
        class default
            write (*,'(A,I0)') 'Other node at index ', i
        end select
    end do

    stop 0
end program test_tmp_debug
