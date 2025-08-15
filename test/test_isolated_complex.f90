program test_isolated_complex
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    ! Exact copy of the failing test case code
    character(len=*), parameter :: source = &
        "real(8), dimension(100), intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg
    logical :: found_decl, is_array_type, has_kind_spec, has_intent_attr
    
    print *, "Isolated test of complex attribute combinations..."
    
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "    FAIL: Tokenization failed: ", error_msg
            stop 1
        end if
    end if
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "    FAIL: Parsing failed: ", error_msg
            stop 1
        end if
    end if
    
    found_decl = .false.
    is_array_type = .false.
    has_kind_spec = .false.
    has_intent_attr = .false.
    
    print *, "Searching in arena of size: ", arena%size
    
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type(node => arena%entries(i)%node)
            type is (declaration_node)
                print *, "Found declaration: var_name='", node%var_name, "'"
                if (node%var_name == "data") then
                    found_decl = .true.
                    is_array_type = node%is_array
                    has_kind_spec = node%has_kind
                    has_intent_attr = node%has_intent
                    print *, "  -> This is the 'data' declaration"
                    print *, "  -> is_array: ", is_array_type
                    print *, "  -> has_kind: ", has_kind_spec
                    print *, "  -> has_intent: ", has_intent_attr
                end if
            class default
                print *, "Found other node type at position ", i
            end select
        else
            print *, "Empty node at position ", i
        end if
    end do
    
    if (.not. found_decl) then
        print *, "    FAIL: Declaration not found"
    else if (.not. is_array_type) then
        print *, "    FAIL: Array flag not set"
    else if (.not. has_kind_spec) then
        print *, "    FAIL: Kind specification not recognized"
    else if (.not. has_intent_attr) then
        print *, "    FAIL: Intent attribute not recognized"
    else
        print *, "    PASS: Complex attribute combination parsed correctly"
    end if
    
end program test_isolated_complex