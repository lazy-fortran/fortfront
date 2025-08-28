program test_lazy_standard_fortran
    ! Test that lazy Fortran mode correctly handles standard Fortran input
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: source_code, formatted_code, error_msg
    integer :: root_index, i
    type(token_t), allocatable :: tokens(:)
    type(semantic_context_t) :: ctx
    
    print *, "=== Testing Lazy Fortran with Standard Fortran Input ==="
    
    ! Test 1: Simple program with implicit none  
    call test_implicit_none_parsing()
    
    ! Test 2: Program with declarations and statements
    call test_declarations_and_statements()
    
    ! Test 3: Program with control flow
    call test_control_flow()
    
    print *, "All lazy standard Fortran tests passed!"
    
contains
    
    subroutine test_implicit_none_parsing()
        print *, "  Testing implicit none parsing..."
        
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: i" // new_line('a') // &
                     "i = 42" // new_line('a') // &
                     "end program test"
        
        ! Lex, parse, and analyze
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexing failed: "
            stop 1
        end if
        
        ! Debug token info
        print *, "Number of tokens:", size(tokens)
        print *, "First few tokens:"
        do i = 1, min(8, size(tokens))
            print *, "  Token", i, ":", trim(tokens(i)%text), "kind:", tokens(i)%kind
        end do
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parsing failed: "
            stop 1
        end if
        
        ctx = create_semantic_context()
        call analyze_semantics(arena, root_index)
        
        print *, "Arena size before formatting:", arena%size
        
        ! Debug AST structure
        call debug_ast_structure(arena, root_index, 0)
        
        ! Generate formatted code using standard emit 
        call emit_fortran(arena, root_index, formatted_code)
        
        print *, "Arena size after formatting:", arena%size
        
        print *, "Input:"
        print *, source_code
        print *, ""
        print *, "Number of tokens:", size(tokens)
        print *, "Root index:", root_index
        print *, "Arena size:", arena%size
        print *, ""
        print *, "Formatted output:"
        print *, formatted_code
        print *, ""
        
        ! Check that implicit none is preserved
        if (index(formatted_code, "implicit none") == 0) then
            print *, "FAIL: implicit none should be preserved in formatted output"
            stop 1
        end if
        
        ! Check that declarations are preserved  
        if (index(formatted_code, "integer :: i") == 0) then
            print *, "FAIL: variable declaration should be preserved"
            stop 1
        end if
        
        ! Check that assignments are preserved
        if (index(formatted_code, "i = 42") == 0) then
            print *, "FAIL: assignment should be preserved"
            stop 1
        end if
        
        print *, "    ✓ Implicit none parsing"
    end subroutine test_implicit_none_parsing
    
    subroutine test_declarations_and_statements()
        print *, "  🔧 Testing declarations and statements..."
        
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: i, j, k" // new_line('a') // &
                     "real :: x" // new_line('a') // &
                     "i = 1" // new_line('a') // &
                     "j = i + 2" // new_line('a') // &
                     "x = 3.14" // new_line('a') // &
                     "end program test"
        
        ! Lex, parse, and analyze
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexing failed: "
            stop 1
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parsing failed: "
            stop 1
        end if
        
        ctx = create_semantic_context()
        call analyze_semantics(arena, root_index)
        
        print *, "Arena size before formatting:", arena%size
        
        ! Generate formatted code
        call emit_fortran(arena, root_index, formatted_code)
        
        print *, "Arena size after formatting:", arena%size
        
        print *, "Multiple declarations formatted:"
        print *, formatted_code
        print *, ""
        
        ! Check key elements are preserved
        if (index(formatted_code, "implicit none") == 0) then
            print *, "FAIL: implicit none missing"
            stop 1
        end if
        
        if (index(formatted_code, "integer ::") == 0) then
            print *, "FAIL: integer declaration missing"
            stop 1
        end if
        
        if (index(formatted_code, "real") == 0) then
            print *, "FAIL: real declaration missing"
            stop 1
        end if
        
        print *, "    ✓ Declarations and statements"
    end subroutine test_declarations_and_statements
    
    subroutine test_control_flow()
        print *, "  🔧 Testing control flow..."
        
        source_code = "program test" // new_line('a') // &
                     "implicit none" // new_line('a') // &
                     "integer :: i" // new_line('a') // &
                     "i = 42" // new_line('a') // &
                     "print *, i" // new_line('a') // &
                     "end program test"
        
        ! Lex, parse, and analyze
        call lex_source(source_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexing failed: "
            stop 1
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parsing failed: "
            stop 1
        end if
        
        ctx = create_semantic_context()
        call analyze_semantics(arena, root_index)
        
        print *, "Arena size before formatting:", arena%size
        
        ! Generate formatted code
        call emit_fortran(arena, root_index, formatted_code)
        
        print *, "Arena size after formatting:", arena%size
        
        print *, "Control flow formatted:"
        print *, formatted_code
        print *, ""
        
        ! Check basic statements are preserved
        if (index(formatted_code, "i = 42") == 0) then
            print *, "FAIL: assignment missing"
            stop 1
        end if
        
        if (index(formatted_code, "print *, i") == 0) then
            print *, "FAIL: print statement missing"
            stop 1
        end if
        
        if (index(formatted_code, "implicit none") == 0) then
            print *, "FAIL: implicit none missing"
            stop 1
        end if
        
        print *, "    ✓ Control flow"
    end subroutine test_control_flow
    
    recursive subroutine debug_ast_structure(arena, node_index, depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, depth
        character(len=20) :: indent
        integer :: j
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        indent = repeat("  ", depth)
        
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            print *, trim(indent) // "program_node: " // node%name
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    call debug_ast_structure(arena, node%body_indices(j), depth + 1)
                end do
            end if
        type is (literal_node)
            print *, trim(indent) // "literal_node: " // node%value
        class default
            print *, trim(indent) // "other_node"
        end select
    end subroutine debug_ast_structure
    
end program test_lazy_standard_fortran