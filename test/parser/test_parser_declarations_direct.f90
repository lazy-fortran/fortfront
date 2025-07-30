program test_parser_declarations_direct
    use parser_declarations_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Declarations Direct Tests ==="
    
    ! Test basic declarations
    call test_integer_declaration()
    call test_real_with_kind()
    call test_array_declaration()
    call test_allocatable_declaration()
    
    ! Test derived types
    call test_derived_type()
    
    ! Test multi-variable declarations
    call test_multi_declaration()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser declarations tests passed!"
        stop 0
    else
        print *, "Some parser declarations tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_integer_declaration()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Parse integer declaration")
        
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="n", line=1, column=9)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse integer declaration")
        end if
    end subroutine test_integer_declaration
    
    subroutine test_real_with_kind()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Parse real with kind")
        
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="real", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=5)
        tokens(3) = token_t(kind=TK_NUMBER, text="8", line=1, column=6)
        tokens(4) = token_t(kind=TK_OPERATOR, text=")", line=1, column=7)
        tokens(5) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=9)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse real(8) declaration")
        end if
    end subroutine test_real_with_kind
    
    subroutine test_array_declaration()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Parse array declaration")
        
        allocate(tokens(8))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(3) = token_t(kind=TK_KEYWORD, text="dimension", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=19)
        tokens(5) = token_t(kind=TK_NUMBER, text="10", line=1, column=20)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=22)
        tokens(7) = token_t(kind=TK_OPERATOR, text="::", line=1, column=24)
        tokens(8) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=27)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse array declaration")
        end if
    end subroutine test_array_declaration
    
    subroutine test_allocatable_declaration()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: decl_idx
        
        call test_start("Parse allocatable declaration")
        
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="real", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=",", line=1, column=5)
        tokens(3) = token_t(kind=TK_KEYWORD, text="allocatable", line=1, column=7)
        tokens(4) = token_t(kind=TK_OPERATOR, text="::", line=1, column=19)
        tokens(5) = token_t(kind=TK_IDENTIFIER, text="vec", line=1, column=22)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        decl_idx = parse_declaration(parser, arena)
        
        if (decl_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse allocatable declaration")
        end if
    end subroutine test_allocatable_declaration
    
    subroutine test_derived_type()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: type_idx
        
        call test_start("Parse derived type")
        
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="type", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="point", line=1, column=6)
        tokens(3) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=11)
        tokens(4) = token_t(kind=TK_KEYWORD, text="end", line=2, column=1)
        tokens(5) = token_t(kind=TK_KEYWORD, text="type", line=2, column=5)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        type_idx = parse_derived_type(parser, arena)
        
        if (type_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse derived type")
        end if
    end subroutine test_derived_type
    
    subroutine test_multi_declaration()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer, allocatable :: decl_indices(:)
        
        call test_start("Parse multi-variable declaration")
        
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="integer", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="::", line=1, column=9)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=12)
        tokens(4) = token_t(kind=TK_OPERATOR, text=",", line=1, column=13)
        tokens(5) = token_t(kind=TK_IDENTIFIER, text="j", line=1, column=15)
        tokens(6) = token_t(kind=TK_OPERATOR, text=",", line=1, column=16)
        tokens(7) = token_t(kind=TK_IDENTIFIER, text="k", line=1, column=18)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        decl_indices = parse_multi_declaration(parser, arena)
        
        if (size(decl_indices) == 3) then
            call test_pass()
        else
            call test_fail("Failed to parse multi-variable declaration")
        end if
    end subroutine test_multi_declaration
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_parser_declarations_direct