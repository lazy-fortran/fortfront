! Debug test to understand module boundary detection
program test_module_boundary_debug
    use frontend_parsing, only: find_program_unit_boundary, parse_tokens
    use frontend_core, only: lex_source, emit_fortran
    use lexer_core, only: token_t, TK_KEYWORD, TK_EOF
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    call test_simple_module()
    print *, "Debug test completed."

contains

    subroutine test_simple_module()
        character(:), allocatable :: source_code
        character(:), allocatable :: error_msg
        character(:), allocatable :: output_code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: i, unit_start, unit_end

        source_code = 'module m' // new_line('a') // &
                      '  implicit none' // new_line('a') // &
                      'contains' // new_line('a') // &
                      '  function add(a,b) result(c)' // new_line('a') // &
                      '    integer :: a,b,c' // new_line('a') // &
                      '    c = a + b' // new_line('a') // &
                      '  end function add' // new_line('a') // &
                      'end module m'

        print *, "Testing module boundary detection"
        print *, "================================="
        print *, ""
        print *, "Source code:"
        print *, trim(source_code)
        print *, ""

        ! Lex the source
        call lex_source(source_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            return
        end if

        ! Print tokens
        print *, "Tokens:"
        do i = 1, min(size(tokens), 20)
            if (tokens(i)%kind == TK_KEYWORD) then
                print '(A,I3,A,A,A)', "Token ", i, ": KEYWORD '", trim(tokens(i)%text), "'"
            else if (tokens(i)%kind == TK_EOF) then
                print '(A,I3,A)', "Token ", i, ": EOF"
            end if
        end do
        print *, ""

        ! Find module boundary
        call find_program_unit_boundary(tokens, 1, unit_start, unit_end)
        print *, "Module boundary: start=", unit_start, " end=", unit_end
        print *, "Total tokens:", size(tokens)
        
        if (unit_end > 0 .and. unit_end <= size(tokens)) then
            print *, "Last token in unit:"
            if (tokens(unit_end)%kind == TK_KEYWORD) then
                print *, "  KEYWORD '", trim(tokens(unit_end)%text), "'"
            else if (tokens(unit_end)%kind == TK_EOF) then
                print *, "  EOF"
            else
                print *, "  Other type"
            end if
        end if
        print *, ""

        ! Parse and emit
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", trim(error_msg)
            return
        end if

        call emit_fortran(arena, prog_index, output_code)
        print *, "Generated code:"
        print *, trim(output_code)
        print *, ""

        ! Check for wrapping
        if (index(output_code, 'program main') > 0) then
            print *, "WARNING: Module was wrapped in 'program main'"
        else
            print *, "OK: Module was not wrapped"
        end if

    end subroutine test_simple_module

end program test_module_boundary_debug