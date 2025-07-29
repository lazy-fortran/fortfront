program test_lexer_keywords
    use lexer_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run comprehensive keyword tokenization tests
    if (.not. test_program_keywords()) all_passed = .false.
    if (.not. test_control_keywords()) all_passed = .false.
    if (.not. test_type_keywords()) all_passed = .false.
    if (.not. test_io_keywords()) all_passed = .false.
    if (.not. test_case_insensitive()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All keyword tokenization tests passed"
        stop 0
    else
        print '(a)', "Some keyword tokenization tests failed"
        stop 1
    end if

contains

    logical function test_program_keywords()
        type(token_t), allocatable :: tokens(:)
        character(len=20), dimension(4) :: prog_keywords = [ &
            "program   ", "end       ", "function  ", "subroutine" &
        ]
        integer :: i

        test_program_keywords = .true.
        print '(a)', "Testing program structure keyword tokenization..."

        do i = 1, size(prog_keywords)
            call tokenize_core(trim(prog_keywords(i)), tokens)
            if (size(tokens) /= 2) then  ! keyword + EOF
                print '(a,a)', "FAIL: Wrong token count for keyword: ", trim(prog_keywords(i))
                test_program_keywords = .false.
                return
            end if

            if (tokens(1)%kind /= TK_KEYWORD) then
                print '(a,a)', "FAIL: Expected KEYWORD token for: ", trim(prog_keywords(i))
                test_program_keywords = .false.
                return
            end if

            if (tokens(1)%text /= trim(prog_keywords(i))) then
                print '(a,a,a,a)', "FAIL: Wrong text for keyword: expected '", &
                    trim(prog_keywords(i)), "' got '", tokens(1)%text, "'"
                test_program_keywords = .false.
                return
            end if
        end do

        print '(a)', "PASS: Program structure keyword tokenization"
    end function test_program_keywords

    logical function test_control_keywords()
        type(token_t), allocatable :: tokens(:)
        character(len=20), dimension(5) :: ctrl_keywords = [ &
            "if    ", "then  ", "else  ", "endif ", "do    " &
        ]
        integer :: i

        test_control_keywords = .true.
        print '(a)', "Testing control flow keyword tokenization..."

        do i = 1, size(ctrl_keywords)
            call tokenize_core(trim(ctrl_keywords(i)), tokens)
            if (tokens(1)%kind /= TK_KEYWORD) then
                print '(a,a)', "FAIL: Expected KEYWORD token for: ", trim(ctrl_keywords(i))
                test_control_keywords = .false.
                return
            end if

            if (tokens(1)%text /= trim(ctrl_keywords(i))) then
                print '(a,a)', "FAIL: Wrong text for keyword: ", trim(ctrl_keywords(i))
                test_control_keywords = .false.
                return
            end if
        end do

        print '(a)', "PASS: Control flow keyword tokenization"
    end function test_control_keywords

    logical function test_type_keywords()
        type(token_t), allocatable :: tokens(:)
        character(len=20), dimension(6) :: type_keywords = [ &
            "implicit ", "none     ", "integer  ", "real     ", "logical  ", "character" &
        ]
        integer :: i

        test_type_keywords = .true.
        print '(a)', "Testing type keyword tokenization..."

        do i = 1, size(type_keywords)
            call tokenize_core(trim(type_keywords(i)), tokens)
            if (tokens(1)%kind /= TK_KEYWORD) then
                print '(a,a)', "FAIL: Expected KEYWORD token for: ", trim(type_keywords(i))
                test_type_keywords = .false.
                return
            end if

            if (tokens(1)%text /= trim(type_keywords(i))) then
                print '(a,a)', "FAIL: Wrong text for keyword: ", trim(type_keywords(i))
                test_type_keywords = .false.
                return
            end if
        end do

        print '(a)', "PASS: Type keyword tokenization"
    end function test_type_keywords

    logical function test_io_keywords()
        type(token_t), allocatable :: tokens(:)
        character(len=20), dimension(4) :: io_keywords = [ &
            "print", "read ", "write", "call " &
        ]
        integer :: i

        test_io_keywords = .true.
        print '(a)', "Testing I/O keyword tokenization..."

        do i = 1, size(io_keywords)
            call tokenize_core(trim(io_keywords(i)), tokens)
            if (tokens(1)%kind /= TK_KEYWORD) then
                print '(a,a)', "FAIL: Expected KEYWORD token for: ", trim(io_keywords(i))
                test_io_keywords = .false.
                return
            end if

            if (tokens(1)%text /= trim(io_keywords(i))) then
                print '(a,a)', "FAIL: Wrong text for keyword: ", trim(io_keywords(i))
                test_io_keywords = .false.
                return
            end if
        end do

        print '(a)', "PASS: I/O keyword tokenization"
    end function test_io_keywords

    logical function test_case_insensitive()
        type(token_t), allocatable :: tokens(:)

        test_case_insensitive = .true.
        print '(a)', "Testing case-insensitive keyword recognition..."

        ! Test uppercase
        call tokenize_core("PROGRAM", tokens)
        if (tokens(1)%kind /= TK_KEYWORD .or. tokens(1)%text /= "PROGRAM") then
            print '(a)', "FAIL: Uppercase PROGRAM not recognized as keyword"
            test_case_insensitive = .false.
            return
        end if

        ! Test mixed case
        call tokenize_core("Program", tokens)
        if (tokens(1)%kind /= TK_KEYWORD .or. tokens(1)%text /= "Program") then
            print '(a)', "FAIL: Mixed case Program not recognized as keyword"
            test_case_insensitive = .false.
            return
        end if

        ! Test identifier that's not a keyword
        call tokenize_core("variable", tokens)
        if (tokens(1)%kind /= TK_IDENTIFIER .or. tokens(1)%text /= "variable") then
            print '(a)', "FAIL: Non-keyword not recognized as identifier"
            test_case_insensitive = .false.
            return
        end if

        print '(a)', "PASS: Case-insensitive keyword recognition"
    end function test_case_insensitive

end program test_lexer_keywords
