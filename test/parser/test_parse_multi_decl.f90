program test_parse_multi_decl
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_EOF
    use parser_state_module
    use parser_declarations, only: parse_multi_declaration
    use ast_core
    use codegen_core
    implicit none

    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer, allocatable :: decl_indices(:)
    integer :: i
    character(len=:), allocatable :: code

    ! Create test tokens for "real :: a, b, c"
    allocate (tokens(9))

    tokens(1) = token_t(TK_KEYWORD, "real", 1, 1)
    tokens(2) = token_t(TK_OPERATOR, "::", 1, 6)
    tokens(3) = token_t(TK_IDENTIFIER, "a", 1, 9)
    tokens(4) = token_t(TK_OPERATOR, ",", 1, 10)
    tokens(5) = token_t(TK_IDENTIFIER, "b", 1, 12)
    tokens(6) = token_t(TK_OPERATOR, ",", 1, 13)
    tokens(7) = token_t(TK_IDENTIFIER, "c", 1, 15)
    tokens(8) = token_t(TK_EOF, "", 1, 16)
    tokens(9) = token_t(TK_EOF, "", 1, 16)  ! Extra EOF for safety

    ! Create parser
    parser = create_parser_state(tokens)

    ! Parse multi-variable declaration
    decl_indices = parse_multi_declaration(parser, arena)

    print *, "Number of declarations created:", size(decl_indices)
    print *, "Declaration indices:", decl_indices

    ! Generate code for each declaration
    do i = 1, size(decl_indices)
        if (decl_indices(i) > 0) then
            code = generate_code_from_arena(arena, decl_indices(i))
            print *, "Declaration", i, ":", trim(code)
        end if
    end do

    ! Check if declarations are in correct order
    if (size(decl_indices) == 3) then
        print *, ""
        print *, "TEST PASSED: Created 3 declarations"

        ! Check if variable names are correct
        block
            type(declaration_node) :: decl
            select type (node => arena%entries(decl_indices(1))%node)
            type is (declaration_node)
                if (node%var_name == "a") then
                    print *, "TEST PASSED: First variable is 'a'"
                else
                print *, "TEST FAILED: First variable is", node%var_name, "expected 'a'"
                end if
            end select

            select type (node => arena%entries(decl_indices(2))%node)
            type is (declaration_node)
                if (node%var_name == "b") then
                    print *, "TEST PASSED: Second variable is 'b'"
                else
               print *, "TEST FAILED: Second variable is", node%var_name, "expected 'b'"
                end if
            end select

            select type (node => arena%entries(decl_indices(3))%node)
            type is (declaration_node)
                if (node%var_name == "c") then
                    print *, "TEST PASSED: Third variable is 'c'"
                else
                print *, "TEST FAILED: Third variable is", node%var_name, "expected 'c'"
                end if
            end select
        end block
    else
        print *, ""
        print *, "TEST FAILED: Expected 3 declarations, got", size(decl_indices)
    end if

end program test_parse_multi_decl
