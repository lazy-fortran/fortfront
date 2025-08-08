program test_expression_parentheses_preservation
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Expression Parentheses Preservation ==='
    print *

    ! Test nested parentheses preservation in complex expressions
    if (.not. test_nested_parentheses_preservation()) all_passed = .false.
    if (.not. test_simple_parentheses_preservation()) all_passed = .false.
    if (.not. test_operator_precedence_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All expression parentheses preservation tests passed!"
        stop 0
    else
        print *, "Some expression parentheses preservation tests failed!"
        stop 1
    end if

contains

    function test_nested_parentheses_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "x = (a + b) * (c + d * (e + f * (g + h)))" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "nested_parentheses_preservation"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Tokenization failed: ", error_msg
                return
            end if
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Parsing failed: ", error_msg
                return
            end if
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, result)
        
        ! Check if compilation succeeded
        if (.not. allocated(result)) then
            print *, "ERROR in ", test_name, ": Compilation failed - no result"
            return
        end if
        
        print *, "Generated result:"
        print *, result
        
        ! Check that parentheses are preserved - should NOT be simplified 
        ! The buggy behavior is: x = a + b*c + d*e + f*g + h
        if (index(result, "a + b*c + d*e + f*g + h") > 0) then
            print *, "ERROR in ", test_name, ": Expression incorrectly simplified - parentheses lost!"
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Check that parentheses are preserved in some form
        ! We should see either the original form or at least proper precedence
        if (index(result, "(a + b)") == 0 .or. &
            index(result, "(g + h)") == 0) then
            print *, "ERROR in ", test_name, ": Essential parentheses were lost"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_nested_parentheses_preservation

    function test_simple_parentheses_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "y = (a + b) * c" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "simple_parentheses_preservation"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Tokenization failed: ", error_msg
                return
            end if
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Parsing failed: ", error_msg
                return
            end if
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, result)
        
        ! Check if compilation succeeded
        if (.not. allocated(result)) then
            print *, "ERROR in ", test_name, ": Compilation failed - no result"
            return
        end if
        
        print *, "Generated result:"
        print *, result
        
        ! Should preserve parentheses or maintain correct precedence
        ! Should NOT become: y = a + b * c (which changes the meaning)
        if (index(result, "y = a + b * c") > 0) then
            print *, "ERROR in ", test_name, ": Expression incorrectly simplified - meaning changed!"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_simple_parentheses_preservation

    function test_operator_precedence_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "z = a * (b + c) * d" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "operator_precedence_preservation"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Tokenization failed: ", error_msg
                return
            end if
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERROR in ", test_name, ": Parsing failed: ", error_msg
                return
            end if
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, result)
        
        ! Check if compilation succeeded
        if (.not. allocated(result)) then
            print *, "ERROR in ", test_name, ": Compilation failed - no result"
            return
        end if
        
        print *, "Generated result:"
        print *, result
        
        ! Should preserve parentheses or maintain correct precedence
        ! Should NOT become: z = a * b + c * d (which changes the meaning)
        if (index(result, "z = a * b + c * d") > 0) then
            print *, "ERROR in ", test_name, ": Expression incorrectly simplified - precedence lost!"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_operator_precedence_preservation

end program test_expression_parentheses_preservation