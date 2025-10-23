program test_issue_1260
    use lexer_api, only: lex_source, lex_file
    use parser_api, only: parse_tokens, parse_tokens_safe
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, compile_source
    implicit none

    logical :: test_passed

    test_passed = test_subroutine_parsing()

    if (test_passed) then
        print *, "PASS: Issue #1260 subroutine parsing test - no extra unnamed_subroutine"
    else
        print *, "FAIL: Issue #1260 - extra unnamed_subroutine generated"
        stop 1
    end if

contains

    function test_subroutine_parsing() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.

        ! Test case from issue #1260 - subroutine without name in end statement
        source = &
            "subroutine my_sub()" // new_line('a') // &
            "    print *, 'Hello from subroutine'" // new_line('a') // &
            "end subroutine"

        print *, "===== Testing Issue 1260: Extra unnamed_subroutine generation ====="
        print *, "Input code:"
        print *, trim(source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Failed to parse: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "ERROR: No output generated"
            passed = .false.
        else
            print *, "Generated output:"
            print *, trim(output)

            ! Check that we don't have "unnamed_subroutine" in the output
            if (index(output, "unnamed_subroutine") /= 0) then
                print *, "ERROR: Found extra unnamed_subroutine in output"
                passed = .false.
            else
                print *, "Good: No extra unnamed_subroutine in output"
            end if
        end if

        ! Test with name in end statement
        source = &
            "subroutine another_sub(x)" // new_line('a') // &
            "    real :: x" // new_line('a') // &
            "    x = x + 1.0" // new_line('a') // &
            "end subroutine another_sub"

        print *, ""
        print *, "Testing with named end statement:"
        print *, trim(source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Failed to parse with named end: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "ERROR: No output generated for named end case"
            passed = .false.
        else
            print *, "Generated output:"
            print *, trim(output)

            if (index(output, "unnamed_subroutine") /= 0) then
                print *, "ERROR: Found extra unnamed_subroutine with named end"
                passed = .false.
            else
                print *, "Good: No extra unnamed_subroutine with named end"
            end if
        end if

    end function test_subroutine_parsing

end program test_issue_1260
