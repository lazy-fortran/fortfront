program test_derived_type_extends
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: derived_type_node
    use fortfront_utils, only: get_node_type
    use fortfront_types, only: NODE_DERIVED_TYPE
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
        "type, extends(base_type) :: derived_type"//new_line('A')// &
        "    integer :: x"//new_line('A')// &
        "end type derived_type"

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: mod_index, node_type, type_index
    character(len=:), allocatable :: error_msg
    type(derived_type_node), pointer :: dtype

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, mod_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: parse error:", trim(error_msg)
        stop 1
    end if

    if (mod_index <= 0) then
        print *, "FAIL: parse failed, no index returned"
        stop 1
    end if

    if (arena%size < 1) then
        print *, "FAIL: arena is empty"
        stop 1
    end if

    type_index = 0
    block
        integer :: i
        do i = 1, arena%size
            node_type = get_node_type(arena, i)
            if (node_type == NODE_DERIVED_TYPE) then
                type_index = i
                exit
            end if
        end do
    end block

    if (type_index == 0) then
        print *, "FAIL: no derived type node found in arena"
        stop 1
    end if

    if (.not. allocated(arena%entries(type_index)%node)) then
        print *, "FAIL: node not allocated at type_index"
        stop 1
    end if

    select type (node => arena%entries(type_index)%node)
        type is (derived_type_node)
        dtype => node

        if (.not. allocated(dtype%extends_parent)) then
            print *, "FAIL: extends_parent not allocated in AST node"
            stop 1
        end if

        if (trim(dtype%extends_parent) /= "base_type") then
            print *, "FAIL: extends_parent mismatch, expected base_type, got:", &
                trim(dtype%extends_parent)
            stop 1
        end if

        if (trim(dtype%name) /= "derived_type") then
            print *, "FAIL: type name mismatch"
            stop 1
        end if
    class default
        print *, "FAIL: node is not a derived_type_node"
        stop 1
    end select

    print *, "PASS: derived type with EXTENDS parsed and extends_parent set"

end program test_derived_type_extends
