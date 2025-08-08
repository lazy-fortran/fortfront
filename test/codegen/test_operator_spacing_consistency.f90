program test_operator_spacing_consistency
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, '=== Testing Operator Spacing Consistency ==='

    ! Test that /= operator maintains proper spacing (issue #165)
    if (.not. test_not_equal_operator()) all_passed = .false.
    
    ! Test comprehensive operator spacing
    if (.not. test_operator_spacing_rules()) all_passed = .false.

    if (all_passed) then
        print *, "All operator spacing tests passed!"
        stop 0
    else
        print *, "Some operator spacing tests failed!"
        stop 1
    end if

contains

    function test_not_equal_operator() result(passed)
        logical :: passed
        character(len=:), allocatable :: result
        
        result = compile_and_generate("x = w /= 3")
        passed = .false.
        
        ! Should NOT contain "/ =" (bug from issue #165)
        if (index(result, "/ =") > 0) then
            print *, "FAIL: /= operator incorrectly spaced as / ="
            return
        end if
        
        ! Should contain proper "/=" spacing
        if (index(result, " /= ") == 0) then
            print *, "FAIL: /= operator not properly spaced"
            return
        end if
        
        print *, "PASS: /= operator spacing preserved"
        passed = .true.
    end function test_not_equal_operator

    function test_operator_spacing_rules() result(passed)
        logical :: passed
        character(len=:), allocatable :: result
        
        result = compile_and_generate("x = a*b + c/d - e /= f")
        passed = .false.
        
        ! Arithmetic operators (* /) should have no spaces
        if (index(result, "a*b") == 0 .or. index(result, "c/d") == 0) then
            print *, "FAIL: Arithmetic operators missing proper spacing"
            return
        end if
        
        ! Comparison and other operators should have spaces
        if (index(result, " + ") == 0 .or. index(result, " - ") == 0 .or. &
            index(result, " /= ") == 0) then
            print *, "FAIL: Comparison/other operators missing proper spacing"
            return
        end if
        
        ! Should NOT have incorrect spacing
        if (index(result, "/ =") > 0) then
            print *, "FAIL: Found incorrect operator spacing"
            return
        end if
        
        print *, "PASS: Operator spacing rules correctly applied"
        passed = .true.
    end function test_operator_spacing_rules

    function compile_and_generate(source_line) result(output)
        character(len=*), intent(in) :: source_line
        character(len=:), allocatable :: output
        
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        source = source_line // new_line('a')
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Tokenization error: ", error_msg
            output = ""
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            output = ""
            return
        end if
        
        call emit_fortran(arena, prog_index, output)
        if (.not. allocated(output)) output = ""
    end function compile_and_generate

end program test_operator_spacing_consistency