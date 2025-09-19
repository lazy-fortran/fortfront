! Test for issue #926: Module function/subroutine bodies missing from generated code
program test_module_contains_functions
    use frontend, only: transform_lazy_fortran_string
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none
    
    call test_module_with_function()
    call test_module_with_subroutine()
    call test_module_with_multiple_procedures()
    print *, ""
    print *, "All module contains tests completed."
    
contains
    
    subroutine test_module_with_function()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        ! Test module with function
        input_code = "module test_mod" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "function add(a, b) result(c)" // new_line('A') // &
                     "integer :: a, b, c" // new_line('A') // &
                     "c = a + b" // new_line('A') // &
                     "end function add" // new_line('A') // &
                     "end module test_mod"
        
        print *, ""
        print *, "Test: Module with function"
        print *, "Input:"
        print *, trim(input_code)
        
        ! Parse the code
        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            error stop 1
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, output_code)
        
        print *, "Output:"
        print *, trim(output_code)
        
        ! Check that the contains section and function are preserved
        if (index(output_code, 'contains') == 0) then
            print *, "FAIL: 'contains' keyword missing from output"
            error stop 1
        end if
        
        if (index(output_code, 'function add') == 0) then
            print *, "FAIL: 'function add' missing from output"
            error stop 1
        end if
        
        if (.not. contains_without_spaces(output_code, 'c=a+b')) then
            print *, "FAIL: Function body 'c = a + b' missing from output"
            error stop 1
        end if
        
        ! Ensure mixed declaration is split: parameters vs local
        if (.not. contains_without_spaces(output_code, 'integer::a,b')) then
            print *, "FAIL: Parameter declaration 'integer :: a, b' missing or not grouped"
            error stop 1
        end if
        if (.not. contains_without_spaces(output_code, 'integer::c')) then
            print *, "FAIL: Local variable 'c' missing from separate declaration"
            error stop 1
        end if
        
        print *, "[PASS] Module with function"
    end subroutine test_module_with_function
    
    subroutine test_module_with_subroutine()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        ! Test module with subroutine
        input_code = "module math_mod" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "subroutine swap(x, y)" // new_line('A') // &
                     "real :: x, y, temp" // new_line('A') // &
                     "temp = x" // new_line('A') // &
                     "x = y" // new_line('A') // &
                     "y = temp" // new_line('A') // &
                     "end subroutine swap" // new_line('A') // &
                     "end module math_mod"
        
        print *, ""
        print *, "Test: Module with subroutine"
        print *, "Input:"
        print *, trim(input_code)
        
        ! Parse the code
        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            error stop 1
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, output_code)
        
        print *, "Output:"
        print *, trim(output_code)
        
        ! Check that the contains section and subroutine are preserved
        if (index(output_code, 'contains') == 0) then
            print *, "FAIL: 'contains' keyword missing from output"
            error stop 1
        end if
        
        if (index(output_code, 'subroutine swap') == 0) then
            print *, "FAIL: 'subroutine swap' missing from output"
            error stop 1
        end if
        
        if (.not. contains_without_spaces(output_code, 'temp=x')) then
            print *, "FAIL: Subroutine body missing from output"
            error stop 1
        end if

        ! Ensure local variable from mixed declaration is preserved
        if (index(output_code, 'real(8) :: temp') == 0) then
            print *, "FAIL: Local variable 'temp' missing from output declaration"
            error stop 1
        end if
        
        print *, "[PASS] Module with subroutine"
    end subroutine test_module_with_subroutine
    
    subroutine test_module_with_multiple_procedures()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        
        ! Test module with multiple procedures
        input_code = "module utils_mod" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "function square(x) result(res)" // new_line('A') // &
                     "real :: x, res" // new_line('A') // &
                     "res = x * x" // new_line('A') // &
                     "end function square" // new_line('A') // &
                     "subroutine print_value(val)" // new_line('A') // &
                     "real :: val" // new_line('A') // &
                     "print *, val" // new_line('A') // &
                     "end subroutine print_value" // new_line('A') // &
                     "end module utils_mod"
        
        print *, ""
        print *, "Test: Module with multiple procedures"
        
        ! Parse the code
        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            error stop 1
        end if
        
        ! Generate code
        call emit_fortran(arena, prog_index, output_code)
        
        print *, "Output:"
        print *, trim(output_code)
        
        ! Check that both procedures are preserved
        if (index(output_code, 'function square') == 0) then
            print *, "FAIL: 'function square' missing from output"
            error stop 1
        end if
        
        ! Check for function body - allow either with or without spaces around *
        if (.not. contains_without_spaces(output_code, 'res=x*x')) then
            print *, "FAIL: Function body missing from output"
            error stop 1
        end if
        
        if (index(output_code, 'subroutine print_value') == 0) then
            print *, "FAIL: 'subroutine print_value' missing from output"
            error stop 1
        end if
        
        print *, "[PASS] Module with multiple procedures"
    end subroutine test_module_with_multiple_procedures
    
    logical function contains_without_spaces(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compressed
        integer :: i

        compressed = ''
        do i = 1, len_trim(text)
            if (text(i:i) /= ' ') compressed = compressed // text(i:i)
        end do
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

end program test_module_contains_functions
