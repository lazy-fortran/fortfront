program test_operator_spacing_consistency
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Operator Spacing Consistency ===', new_line('a')

    ! Test that comparison operators maintain consistent spacing
    if (.not. test_not_equal_operator_spacing()) all_passed = .false.
    if (.not. test_all_comparison_operators_spacing()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All operator spacing tests passed!"
        stop 0
    else
        print *, "Some operator spacing tests failed!"
        stop 1
    end if

contains

    function test_not_equal_operator_spacing() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "if (w /= 3) print *, 'not equal'" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "not_equal_operator_spacing"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        print *, "Testing /= operator spacing preservation..."
        
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
        
        print *, "Input: w /= 3"
        print *, "Generated result:"
        print *, result
        
        ! Should NOT contain "/ =" with space (incorrect)
        if (index(result, "/ =") > 0) then
            print *, "ERROR in ", test_name, ": /= operator incorrectly spaced as / ="
            print *, "Found incorrect spacing: / ="
            print *, "Got result:"
            print *, result
            return
        end if
        
        ! Should contain "/=" without space (correct)
        if (index(result, "/=") == 0) then
            print *, "ERROR in ", test_name, ": /= operator not found in output"
            print *, "Expected to contain: /="
            print *, "Got result:"
            print *, result
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_not_equal_operator_spacing

    function test_all_comparison_operators_spacing() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            "if (a == b .and. c /= d .and. e <= f .and. g >= h .and. i < j .and. k > l) then" // &
            new_line('a') // &
            "    print *, 'all comparisons'" // new_line('a') // &
            "end if" // new_line('a')
        character(len=:), allocatable :: result, error_msg
        character(len=*), parameter :: test_name = "all_comparison_operators_spacing"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        passed = .false.
        
        print *, "Testing all comparison operators spacing consistency..."
        
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
        
        print *, "Input: a == b .and. c /= d .and. e <= f .and. g >= h .and. i < j .and. k > l"
        print *, "Generated result:"
        print *, result
        
        ! Check that all operators are preserved correctly without incorrect spacing
        block
            character(len=*), parameter :: bad_operators(*) = [character(len=10) :: "/ =", "= =", "< =", "> ="]
            character(len=*), parameter :: good_operators(*) = [character(len=10) :: "/=", "==", "<=", ">="]
            integer :: i
            
            ! Check for incorrect spacing
            do i = 1, size(bad_operators)
                if (index(result, trim(bad_operators(i))) > 0) then
                    print *, "ERROR in ", test_name, ": Found incorrectly spaced operator: '", &
                        trim(bad_operators(i)), "'"
                    print *, "Got result:"
                    print *, result
                    return
                end if
            end do
            
            ! Check that correct operators are present
            do i = 1, size(good_operators)
                if (index(result, trim(good_operators(i))) == 0) then
                    print *, "WARNING in ", test_name, ": Expected operator not found: '", &
                        trim(good_operators(i)), "'"
                    ! Don't fail for this, just warn
                end if
            end do
        end block

        print *, "PASS: ", test_name
        passed = .true.
    end function test_all_comparison_operators_spacing

end program test_operator_spacing_consistency