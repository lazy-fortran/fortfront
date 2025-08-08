program test_array_literal_syntax_preservation
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Array Literal Syntax Preservation ==='
    print *

    ! Test that array literal syntax is preserved from input
    if (.not. test_modern_array_syntax_preservation()) all_passed = .false.
    if (.not. test_legacy_array_syntax_preservation()) all_passed = .false.
    if (.not. test_mixed_array_syntax_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All array literal syntax preservation tests passed!"
        stop 0
    else
        print *, "Some array literal syntax preservation tests failed!"
        stop 1
    end if

contains

    function test_modern_array_syntax_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "arr = [1, 2, 3, 4]" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "modern_array_syntax_preservation"
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
        
        print *, "Input: [1, 2, 3, 4]"
        print *, "Generated result:"
        print *, result
        
        ! Should preserve modern syntax - should NOT convert to (/ ... /)
        if (index(result, "[1, 2, 3, 4]") == 0 .and. index(result, "[1,2,3,4]") == 0) then
            print *, "ERROR in ", test_name, ": Modern array syntax not preserved"
            print *, "Expected to contain: [1, 2, 3, 4] or [1,2,3,4]"
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Should NOT convert to legacy syntax
        if (index(result, "(/ 1, 2, 3, 4 /)") > 0 .or. index(result, "(/1,2,3,4/)") > 0) then
            print *, "ERROR in ", test_name, ": Modern syntax incorrectly converted to legacy"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_modern_array_syntax_preservation

    function test_legacy_array_syntax_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "arr = (/ 1, 2, 3, 4 /)" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "legacy_array_syntax_preservation"
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
        
        print *, "Input: (/ 1, 2, 3, 4 /)"
        print *, "Generated result:"
        print *, result
        
        ! Should preserve legacy syntax - should NOT convert to [...] 
        if (index(result, "(/ 1, 2, 3, 4 /)") == 0 .and. index(result, "(/1,2,3,4/)") == 0) then
            print *, "ERROR in ", test_name, ": Legacy array syntax not preserved"
            print *, "Expected to contain: (/ 1, 2, 3, 4 /) or (/1,2,3,4/)"
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Should NOT convert to modern syntax
        if (index(result, "[1, 2, 3, 4]") > 0 .or. index(result, "[1,2,3,4]") > 0) then
            print *, "ERROR in ", test_name, ": Legacy syntax incorrectly converted to modern"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_legacy_array_syntax_preservation

    function test_mixed_array_syntax_preservation() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "arr1 = [1, 2, 3]" // new_line('a') // &
            "arr2 = (/ 4, 5, 6 /)" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "mixed_array_syntax_preservation"
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
        
        print *, "Input mixed: [1, 2, 3] and (/ 4, 5, 6 /)"
        print *, "Generated result:"
        print *, result
        
        ! Both syntaxes should be preserved independently
        if ((index(result, "[1, 2, 3]") == 0 .and. index(result, "[1,2,3]") == 0) .or. &
            (index(result, "(/ 4, 5, 6 /)") == 0 .and. index(result, "(/4,5,6/)") == 0)) then
            print *, "ERROR in ", test_name, ": Mixed array syntax not preserved"
            print *, "Expected both [1,2,3] and (/4,5,6/) formats"
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_mixed_array_syntax_preservation

end program test_array_literal_syntax_preservation