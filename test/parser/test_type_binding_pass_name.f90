program test_type_binding_pass_name
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: type_binding_node
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
                                   "type :: atype"//new_line('A')// &
                                   "   integer :: field"//new_line('A')// &
                                   "contains"//new_line('A')// &
                               "   procedure, pass(other) :: m => fn"//new_line('A')// &
                                   "end type atype"

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: mod_index, i
    character(len=:), allocatable :: error_msg
    logical :: found

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

    found = .false.
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (node => arena%entries(i)%node)
        type is (type_binding_node)
            found = .true.
            if (.not. allocated(node%pass_name)) then
                print *, "FAIL: pass_name not captured"
                stop 1
            end if
            if (trim(node%pass_name) /= "other") then
                print *, "FAIL: expected pass_name 'other', got ", &
                    trim(node%pass_name)
                stop 1
            end if
            exit
        end select
    end do
    if (.not. found) then
        print *, "FAIL: no type_binding_node in arena"
        stop 1
    end if

    print *, "PASS: pass(name) dummy captured on the binding"
end program test_type_binding_pass_name
