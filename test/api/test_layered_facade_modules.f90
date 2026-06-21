program test_layered_facade_modules
    use fortfront_ast, only: ast_arena_t
    use fortfront_compiler, only: compiler_frontend_options_t, &
                                  compiler_frontend_result_t, &
                                  compile_frontend_from_string, &
                                  INPUT_MODE_STANDARD
    use fortfront_lexer, only: token_t
    use fortfront_tooling, only: tooling_load_ast_from_string
    use fortfront_transform, only: transform_lazy_fortran_string
    implicit none

    call test_transform_facade()
    call test_tooling_facade()
    call test_compiler_facade()
    print *, 'PASS: layered facade modules'

contains

    subroutine test_transform_facade()
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call transform_lazy_fortran_string('x = 1', output, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop 'fortfront_transform returned an error'
        end if
        if (.not. allocated(output)) then
            error stop 'fortfront_transform did not allocate output'
        end if
    end subroutine test_transform_facade

    subroutine test_tooling_facade()
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg

        call tooling_load_ast_from_string('program p'//new_line('a')// &
                                          'end program p', arena, root_index, &
                                          error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop 'fortfront_tooling returned an error'
        end if
        if (root_index <= 0) error stop 'fortfront_tooling returned no root'
    end subroutine test_tooling_facade

    subroutine test_compiler_facade()
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(token_t), allocatable :: tokens(:)

        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string('program p'//new_line('a')// &
                                          'end program p', result, options)
        if (.not. result%success()) then
            error stop 'fortfront_compiler returned an error'
        end if

        allocate (tokens(size(result%tokens)))
        tokens = result%tokens
        if (size(tokens) == 0) error stop 'fortfront_lexer token type unusable'
    end subroutine test_compiler_facade

end program test_layered_facade_modules
