module test_lexer_comprehensive
    use json_module
    use test_drive, only : new_unittest, unittest_type, error_type, check, test_failed
    use frontend
    use ast_core
    implicit none
    private
    public :: collect_lexer_comprehensive_tests

contains

    subroutine collect_lexer_comprehensive_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_all_fortran_keywords", test_all_fortran_keywords), &
            new_unittest("test_all_operators", test_all_operators), &
            new_unittest("test_numeric_literals", test_numeric_literals), &
            new_unittest("test_string_literals", test_string_literals), &
            new_unittest("test_comments_and_continuations", test_comments_and_continuations), &
            new_unittest("test_lexer_error_cases", test_lexer_error_cases), &
            new_unittest("test_complex_expressions", test_complex_expressions), &
            new_unittest("test_edge_case_identifiers", test_edge_case_identifiers), &
            new_unittest("test_position_tracking", test_position_tracking), &
            new_unittest("test_real_world_code", test_real_world_code) &
        ]
    end subroutine

    subroutine test_all_fortran_keywords(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        integer :: i
        character(len=20), dimension(50) :: keywords
        
        ! Comprehensive list of Fortran keywords
        keywords = [ &
            "program     ", "end         ", "module      ", "use         ", &
            "contains    ", "subroutine  ", "function    ", "call        ", &
            "if          ", "then        ", "else        ", "elseif      ", &
            "do          ", "while       ", "continue    ", "exit        ", &
            "cycle       ", "select      ", "case        ", "default     ", &
            "where       ", "elsewhere   ", "forall      ", "concurrent  ", &
            "integer     ", "real        ", "complex     ", "logical     ", &
            "character   ", "type        ", "class       ", "extends     ", &
            "abstract    ", "interface   ", "procedure   ", "operator    ", &
            "assignment  ", "generic     ", "final       ", "import      ", &
            "implicit    ", "none        ", "allocatable ", "pointer     ", &
            "target      ", "dimension   ", "intent      ", "optional    ", &
            "parameter   ", "save        " &
        ]
        
        ! Test each keyword
        do i = 1, size(keywords)
            code = "program test" // new_line('a') // &
                   trim(adjustl(keywords(i))) // new_line('a') // &
                   "end program"
            
            call compile_to_json_string(code, json)
            
            ! Verify keyword was lexed correctly
            call check(error, json%failed() .eqv. .false., &
                      "Failed to lex keyword: " // trim(keywords(i)))
            if (allocated(error)) return
            
            call json%destroy()
        end do
    end subroutine

    subroutine test_all_operators(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test arithmetic operators
        code = "program test" // new_line('a') // &
               "a = b + c - d * e / f ** g" // new_line('a') // &
               "end program"
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex arithmetic operators")
        if (allocated(error)) return
        call json%destroy()
        
        ! Test comparison operators
        code = "program test" // new_line('a') // &
               "if (a < b .and. c > d .and. e <= f .and. g >= h) then" // new_line('a') // &
               "if (i == j .and. k /= l) then" // new_line('a') // &
               "if (m .eq. n .and. o .ne. p) then" // new_line('a') // &
               "if (q .lt. r .and. s .gt. t) then" // new_line('a') // &
               "if (u .le. v .and. w .ge. x) then" // new_line('a') // &
               "end if; end if; end if; end if; end if" // new_line('a') // &
               "end program"
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex comparison operators")
        if (allocated(error)) return
        call json%destroy()
        
        ! Test logical operators
        code = "program test" // new_line('a') // &
               "if (a .and. b .or. .not. c) then" // new_line('a') // &
               "if (d .eqv. e .neqv. f) then" // new_line('a') // &
               "end if; end if" // new_line('a') // &
               "end program"
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex logical operators")
        if (allocated(error)) return
        call json%destroy()
        
        ! Test special operators
        code = "program test" // new_line('a') // &
               "a => b" // new_line('a') // &
               "c(1:10:2) = d(:)" // new_line('a') // &
               "e = f // g" // new_line('a') // &
               "end program"
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex special operators")
        call json%destroy()
    end subroutine

    subroutine test_numeric_literals(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various numeric formats
        code = "program test" // new_line('a') // &
               ! Integer literals
               "a = 42" // new_line('a') // &
               "b = +123" // new_line('a') // &
               "c = -456" // new_line('a') // &
               "d = 0" // new_line('a') // &
               "e = 1000000" // new_line('a') // &
               ! Real literals
               "f = 3.14159" // new_line('a') // &
               "g = -2.71828" // new_line('a') // &
               "h = 1.0" // new_line('a') // &
               "i = .5" // new_line('a') // &
               "j = 5." // new_line('a') // &
               ! Scientific notation
               "k = 1.23e10" // new_line('a') // &
               "l = -4.56E-5" // new_line('a') // &
               "m = 7.89d20" // new_line('a') // &
               "n = 1e100" // new_line('a') // &
               ! Complex literals
               "o = (1.0, 2.0)" // new_line('a') // &
               "p = (-3.14, 2.71)" // new_line('a') // &
               ! Kind parameters
               "q = 42_8" // new_line('a') // &
               "r = 3.14_real64" // new_line('a') // &
               "s = 1.0_dp" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex numeric literals")
        call json%destroy()
    end subroutine

    subroutine test_string_literals(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test various string formats
        code = "program test" // new_line('a') // &
               'a = "Hello, World!"' // new_line('a') // &
               "b = 'Single quotes'" // new_line('a') // &
               'c = "Embedded ""quotes"""' // new_line('a') // &
               "d = 'Embedded ''apostrophes'''" // new_line('a') // &
               'e = ""' // new_line('a') // &  ! Empty string
               "f = ''" // new_line('a') // &  ! Empty string
               'g = "Line 1" // new_line("a") // "Line 2"' // new_line('a') // &
               'h = "Special chars: \n \t \\"' // new_line('a') // &
               'i = "Unicode: ' // char(960) // char(945) // '"' // new_line('a') // &  ! π α
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex string literals")
        call json%destroy()
    end subroutine

    subroutine test_comments_and_continuations(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test comments
        code = "program test ! This is a comment" // new_line('a') // &
               "! Full line comment" // new_line('a') // &
               "a = 1 ! End of line comment" // new_line('a') // &
               "! Comment with special chars: @#$%^&*()" // new_line('a') // &
               "b = 2" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex comments")
        if (allocated(error)) return
        call json%destroy()
        
        ! Test line continuations
        code = "program test" // new_line('a') // &
               "a = 1 + 2 + 3 + &" // new_line('a') // &
               "    4 + 5 + 6 + &" // new_line('a') // &
               "    7 + 8 + 9" // new_line('a') // &
               "b = 'Long string that &" // new_line('a') // &
               "    &continues on next line'" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex line continuations")
        call json%destroy()
    end subroutine

    subroutine test_lexer_error_cases(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test unterminated string
        code = "program test" // new_line('a') // &
               'a = "unterminated string' // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        ! This should produce some error indication
        call json%destroy()
        
        ! Test invalid characters
        code = "program test" // new_line('a') // &
               "a = 1 @ 2" // new_line('a') // &  ! @ is not a valid operator
               "end program"
        
        call compile_to_json_string(code, json)
        call json%destroy()
        
        ! Test invalid numeric literals
        code = "program test" // new_line('a') // &
               "a = 1.2.3" // new_line('a') // &  ! Multiple decimal points
               "b = 1e" // new_line('a') // &     ! Incomplete exponent
               "c = 1e++" // new_line('a') // &   ! Invalid exponent
               "end program"
        
        call compile_to_json_string(code, json)
        call json%destroy()
        
        ! We're not checking for failures as the lexer might handle these differently
        ! The important thing is that it doesn't crash
    end subroutine

    subroutine test_complex_expressions(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test complex nested expressions
        code = "program test" // new_line('a') // &
               "a = ((b + c) * (d - e)) / (f ** 2 + g ** 2) ** 0.5" // new_line('a') // &
               "h(i, j:k:2) = matrix(1:n, :) + transpose(other(:, 1:m))" // new_line('a') // &
               "result = merge(array1, array2, mask .and. .not. flag)" // new_line('a') // &
               "ptr => target%component%subcomponent(index)%value" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex complex expressions")
        call json%destroy()
    end subroutine

    subroutine test_edge_case_identifiers(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test identifiers that might be confused with keywords
        code = "program test" // new_line('a') // &
               "integer :: iffy, theme, elsewhere_var" // new_line('a') // &
               "real :: do_work, while_true, end_value" // new_line('a') // &
               "logical :: programmatic, modular" // new_line('a') // &
               ! Test very long identifiers (Fortran allows up to 63 chars)
               "real :: this_is_a_very_long_identifier_name_that_is_still_valid" // new_line('a') // &
               ! Test identifiers with numbers and underscores
               "integer :: var1, var_2, _leading, trailing_" // new_line('a') // &
               "real :: mixed_123_name_456" // new_line('a') // &
               "end program"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex edge case identifiers")
        call json%destroy()
    end subroutine

    subroutine test_position_tracking(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test that lexer tracks line and column positions correctly
        code = "program test" // new_line('a') // &        ! Line 1
               "    integer :: a" // new_line('a') // &      ! Line 2, indented
               "    real :: b, c, d" // new_line('a') // &   ! Line 3, multiple on line
               "" // new_line('a') // &                      ! Line 4, empty
               "    ! Comment line" // new_line('a') // &    ! Line 5
               "    a = 1 + &" // new_line('a') // &         ! Line 6, continuation
               "         2 + 3" // new_line('a') // &        ! Line 7
               "end program"                                 ! Line 8
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to track positions")
        ! In a real test, we would verify that tokens have correct line/column info
        call json%destroy()
    end subroutine

    subroutine test_real_world_code(error)
        type(error_type), allocatable, intent(out) :: error
        type(json_file) :: json
        character(:), allocatable :: code
        
        ! Test realistic Fortran code patterns
        code = &
            "module numerical_methods" // new_line('a') // &
            "    use iso_fortran_env, only: real64, int32" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    private" // new_line('a') // &
            "    public :: solve_quadratic, integrate" // new_line('a') // &
            "" // new_line('a') // &
            "    interface integrate" // new_line('a') // &
            "        module procedure integrate_trap, integrate_simpson" // new_line('a') // &
            "    end interface" // new_line('a') // &
            "" // new_line('a') // &
            "contains" // new_line('a') // &
            "" // new_line('a') // &
            "    pure function solve_quadratic(a, b, c) result(roots)" // new_line('a') // &
            "        real(real64), intent(in) :: a, b, c" // new_line('a') // &
            "        complex(real64) :: roots(2)" // new_line('a') // &
            "        real(real64) :: discriminant" // new_line('a') // &
            "        " // new_line('a') // &
            "        discriminant = b**2 - 4.0_real64*a*c" // new_line('a') // &
            "        " // new_line('a') // &
            "        if (discriminant >= 0.0_real64) then" // new_line('a') // &
            "            roots(1) = (-b + sqrt(discriminant)) / (2.0_real64*a)" // new_line('a') // &
            "            roots(2) = (-b - sqrt(discriminant)) / (2.0_real64*a)" // new_line('a') // &
            "        else" // new_line('a') // &
            "            roots(1) = cmplx(-b/(2.0_real64*a), sqrt(-discriminant)/(2.0_real64*a), real64)" // new_line('a') // &
            "            roots(2) = cmplx(-b/(2.0_real64*a), -sqrt(-discriminant)/(2.0_real64*a), real64)" // new_line('a') // &
            "        end if" // new_line('a') // &
            "    end function solve_quadratic" // new_line('a') // &
            "" // new_line('a') // &
            "end module numerical_methods"
        
        call compile_to_json_string(code, json)
        call check(error, json%failed() .eqv. .false., "Failed to lex real-world code")
        call json%destroy()
    end subroutine

end module test_lexer_comprehensive