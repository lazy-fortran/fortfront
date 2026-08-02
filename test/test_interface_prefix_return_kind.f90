program test_interface_prefix_return_kind
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_procedure, only: function_def_node
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index, i
    logical :: found

    source = &
        "module c_api" // new_line('A') // &
        "    use, intrinsic :: iso_c_binding, only: c_long_long" // &
        new_line('A') // &
        "    interface" // new_line('A') // &
        "        integer(c_long_long) function get_value() bind(C)" // &
        new_line('A') // &
        "        end function get_value" // new_line('A') // &
        "    end interface" // new_line('A') // &
        "end module c_api" // new_line('A')

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)
    call assert_no_error(error_msg, "lexing")

    call parse_tokens(tokens, arena, root_index, error_msg)
    call assert_no_error(error_msg, "parsing")

    found = .false.
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (node => arena%entries(i)%node)
            type is (function_def_node)
            if (.not. allocated(node%name)) cycle
            if (node%name /= "get_value") cycle

            found = .true.
            if (.not. allocated(node%return_type)) then
                call fail("get_value has no return type")
            else if (node%return_type /= "integer(c_long_long)") then
                call fail("return type was '" // node%return_type // &
                    "' instead of 'integer(c_long_long)'")
            end if
        end select
    end do

    if (.not. found) call fail("interface function get_value was not parsed")
    write (*, '(A)') "PASS: interface return kind preserved in the AST"

contains

    subroutine assert_no_error(message, phase)
        character(len=:), allocatable, intent(in) :: message
        character(len=*), intent(in) :: phase

        if (allocated(message)) then
            if (len_trim(message) > 0) call fail(trim(phase) // " error: " // &
                trim(message))
        end if
    end subroutine assert_no_error

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') "FAIL: " // trim(message)
        error stop 1
    end subroutine fail

end program test_interface_prefix_return_kind
