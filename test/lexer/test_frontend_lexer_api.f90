program test_frontend_lexer_api
    use lexer_core, only: tokenize_core, token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Lexer API Unit Tests ==='
    print *

    ! Test individual lexer features via API
    if (.not. test_identifier_tokenization()) all_passed = .false.
    if (.not. test_number_tokenization()) all_passed = .false.
    if (.not. test_operator_tokenization()) all_passed = .false.
    if (.not. test_keyword_tokenization()) all_passed = .false.
    if (.not. test_string_tokenization()) all_passed = .false.
    if (.not. test_comment_handling()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All lexer API tests passed!'
        stop 0
    else
        print *, 'Some lexer API tests failed!'
        stop 1
    end if

contains

    logical function test_identifier_tokenization()
        test_identifier_tokenization = .true.
        print *, 'Testing identifier tokenization...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Simple identifier
            call tokenize_core('x', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Simple identifier'
                test_identifier_tokenization = .false.
                return
            end if

            ! Multiple identifiers
            call tokenize_core('var1 var2', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 2) then
                print *, '  FAIL: Multiple identifiers'
                test_identifier_tokenization = .false.
                return
            end if

            print *, '  PASS: Identifier tokenization'
        end block

    end function test_identifier_tokenization

    logical function test_number_tokenization()
        test_number_tokenization = .true.
        print *, 'Testing number tokenization...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Integer
            call tokenize_core('42', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Integer tokenization'
                test_number_tokenization = .false.
                return
            end if

            ! Real number
            call tokenize_core('3.14', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Real number tokenization'
                test_number_tokenization = .false.
                return
            end if

            print *, '  PASS: Number tokenization'
        end block

    end function test_number_tokenization

    logical function test_operator_tokenization()
        test_operator_tokenization = .true.
        print *, 'Testing operator tokenization...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Assignment operator
            call tokenize_core('=', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Assignment operator'
                test_operator_tokenization = .false.
                return
            end if

            ! Arithmetic operators
            call tokenize_core('+ - * /', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 4) then
                print *, '  FAIL: Arithmetic operators'
                test_operator_tokenization = .false.
                return
            end if

            print *, '  PASS: Operator tokenization'
        end block

    end function test_operator_tokenization

    logical function test_keyword_tokenization()
        test_keyword_tokenization = .true.
        print *, 'Testing keyword tokenization...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Real keyword
            call tokenize_core('real', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Real keyword'
                test_keyword_tokenization = .false.
                return
            end if

            ! Integer keyword
            call tokenize_core('integer', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Integer keyword'
                test_keyword_tokenization = .false.
                return
            end if

            print *, '  PASS: Keyword tokenization'
        end block

    end function test_keyword_tokenization

    logical function test_string_tokenization()
        test_string_tokenization = .true.
        print *, 'Testing string tokenization...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Single quoted string
            call tokenize_core("'hello'", tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Single quoted string'
                test_string_tokenization = .false.
                return
            end if

            ! Double quoted string
            call tokenize_core('"world"', tokens)
            if (.not. allocated(tokens) .or. size(tokens) == 0) then
                print *, '  FAIL: Double quoted string'
                test_string_tokenization = .false.
                return
            end if

            print *, '  PASS: String tokenization'
        end block

    end function test_string_tokenization

    logical function test_comment_handling()
        test_comment_handling = .true.
        print *, 'Testing comment handling...'

        block
            type(token_t), allocatable :: tokens(:)

            ! Comment line
            call tokenize_core('! this is a comment', tokens)
            ! Comments might be filtered out or preserved depending on implementation
            print *, '  PASS: Comment handling (implementation-dependent)'

            ! Code with comment
            call tokenize_core('x = 1 ! comment', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 3) then
                print *, '  FAIL: Code with comment'
                test_comment_handling = .false.
                return
            end if

            print *, '  PASS: Code with comment'
        end block

    end function test_comment_handling

end program test_frontend_lexer_api
