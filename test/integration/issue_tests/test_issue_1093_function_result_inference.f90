program test_issue_1093_function_result_inference
    ! Regression test for Issue #1093:
    ! Functions without an explicit result(...) and under implicit none
    ! should infer the result variable from first assignment target,
    ! add result(<name>) to the header, and insert a matching declaration.

    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: error_msg, code, source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    logical :: ok

    print *, "=== Testing Issue #1093: Function result inference ==="

    ok = .true.

    ! Note: no explicit result(...) and no explicit declaration for the
    ! function name. Standardizer should add implicit none, infer result
    ! name from first assignment target, and emit a declaration.
    call read_example('examples/f90/issue_1093_function_result_inference.f90', &
                      source)

    call initialize_codegen()
    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: lexing error:', trim(error_msg)
        stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: parsing error:', trim(error_msg)
        stop 1
    end if

    call standardize_ast(arena, root_index)

    code = codegen_core_generate_arena(arena, root_index)

    ! When result variable name equals function name, Fortran does not allow a
    ! result() clause in the signature.
    ! Instead, the return type should be in the function signature
    if (index(code, 'integer function incr') <= 0 .and. &
        index(code, 'result(incr)') > 0) then
        print *, 'FAIL: function has invalid result(incr) when result name ' // &
            'equals function name'
        print *, trim(code)
        ok = .false.
    end if

    ! Check that return type is present (either in signature OR as separate declaration)
    if (index(code, 'integer function incr') <= 0 .and. &
        index(code, ':: incr') <= 0) then
        print *, &
            'FAIL: function lacks return type (neither in signature nor declaration)'
        print *, trim(code)
        ok = .false.
    end if

    if (index(code, 'implicit none') <= 0) then
        print *, 'FAIL: implicit none not inserted in function scope'
        print *, trim(code)
        ok = .false.
    end if

    if (.not. ok) stop 1
contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example
end program test_issue_1093_function_result_inference
