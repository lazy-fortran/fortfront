program test_io_issue_497
    ! Test case for Issue #497: I/O parsing generates 'Unknown node type' error
    use lexer_core
    use frontend_parsing, only: parse_tokens
    use codegen_core
    use ast_core
    use error_handling
    implicit none

    call test_simple_read_statement()
    call test_simple_write_statement()
    call test_read_write_combination()
    call test_multiple_io_variables()

contains

    subroutine test_simple_read_statement()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(tokenize_result_t) :: lex_result
        integer :: node_index
        character(len=1024) :: error_msg

        print *, "Test: simple read statement"

        ! Test case from issue
        source = "read(*,*) x"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, node_index, error_msg)
        if (node_index <= 0) then
            print *, "FAIL: Parsing failed: ", trim(error_msg)
            stop 1
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, node_index)

        ! Check that it doesn't generate "Unknown node type"
        if (index(generated, "Unknown node type") > 0) then
            print *, "FAIL: Generated unknown node type comment"
            print *, "Generated: ", generated
            stop 1
        end if

        ! Check that it generates a proper read statement
        if (index(generated, "read") == 0) then
            print *, "FAIL: No read statement generated"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: simple read statement"
    end subroutine test_simple_read_statement

    subroutine test_simple_write_statement()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(tokenize_result_t) :: lex_result
        integer :: node_index
        character(len=1024) :: error_msg

        print *, "Test: simple write statement"

        source = "write(*,*) x"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, node_index, error_msg)
        if (node_index <= 0) then
            print *, "FAIL: Parsing failed: ", trim(error_msg)
            stop 1
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, node_index)

        ! Check that it doesn't generate "Unknown node type"
        if (index(generated, "Unknown node type") > 0) then
            print *, "FAIL: Generated unknown node type comment"
            print *, "Generated: ", generated
            stop 1
        end if

        ! Check that it generates a proper write statement
        if (index(generated, "write") == 0) then
            print *, "FAIL: No write statement generated"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: simple write statement"
    end subroutine test_simple_write_statement

    subroutine test_read_write_combination()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(tokenize_result_t) :: lex_result
        integer :: node_index
        character(len=1024) :: error_msg

        print *, "Test: read/write combination"

        ! Test with implicit program (no explicit program/end)
        source = "read(*,*) x" // new_line('a') // &
                "write(*,*) x"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, node_index, error_msg)
        if (node_index <= 0) then
            print *, "FAIL: Parsing failed: ", trim(error_msg)
            stop 1
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, node_index)

        ! Check that both statements are generated
        if (index(generated, "read") == 0) then
            print *, "FAIL: No read statement generated"
            print *, "Generated: ", generated
            stop 1
        end if

        if (index(generated, "write") == 0) then
            print *, "FAIL: No write statement generated"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: read/write combination"
    end subroutine test_read_write_combination

    subroutine test_multiple_io_variables()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(tokenize_result_t) :: lex_result
        integer :: node_index
        character(len=1024) :: error_msg

        print *, "Test: multiple I/O variables"

        source = "read(*,*) x, y, z"
        
        ! Tokenize
        lex_result = tokenize_safe(source)
        if (lex_result%result%is_failure()) then
            print *, "FAIL: Tokenization failed: ", trim(lex_result%result%error_message)
            stop 1
        end if
        tokens = lex_result%tokens

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, node_index, error_msg)
        if (node_index <= 0) then
            print *, "FAIL: Parsing failed: ", trim(error_msg)
            stop 1
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, node_index)

        ! Check that it generates a proper read with multiple variables
        if (index(generated, "x") == 0 .or. &
            index(generated, "y") == 0 .or. &
            index(generated, "z") == 0) then
            print *, "FAIL: Multiple variables not preserved"
            print *, "Generated: ", generated
            stop 1
        end if

        print *, "PASS: multiple I/O variables"
    end subroutine test_multiple_io_variables

end program test_io_issue_497