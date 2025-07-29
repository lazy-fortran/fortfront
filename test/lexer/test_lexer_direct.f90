program test_lexer_direct
    use lexer_core
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_simple_tokenization()) all_passed = .false.

    if (all_passed) then
        print *, "All lexer tests passed"
        stop 0
    else
        print *, "Some lexer tests failed"
        stop 1
    end if

contains

    logical function test_simple_tokenization()
        character(len=*), parameter :: source = "x = 1"
        type(token_t), allocatable :: tokens(:)

        test_simple_tokenization = .true.
        print *, "Testing simple tokenization..."

        ! Tokenize directly
        call tokenize_core(source, tokens)

        if (allocated(tokens)) then
            print *, "PASS: Tokenization completed, got", size(tokens), "tokens"
            if (size(tokens) > 0) then
                print *, "First token:", tokens(1)%text
            end if
        else
            print *, "FAIL: Tokenization failed - no tokens allocated"
            test_simple_tokenization = .false.
        end if

    end function test_simple_tokenization

end program test_lexer_direct
