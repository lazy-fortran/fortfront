module json_writer
    use json_module
    implicit none

contains

    ! Write tokens array to JSON file
    subroutine json_write_tokens_to_file(tokens, filename)
        use lexer_core, only: token_t, token_type_name
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: filename

        type(json_core) :: json
        type(json_value), pointer :: root, token_array, token_obj
        integer :: i

        ! Initialize JSON
        call json%initialize()

        ! Create root object
        call json%create_object(root, '')

        ! Create tokens array
        call json%create_array(token_array, 'tokens')
        call json%add(root, token_array)

        ! Add each token to array
        do i = 1, size(tokens)
            ! Create token object
            call json%create_object(token_obj, '')
            call json%add(token_obj, 'type', token_type_name(tokens(i)%kind))
            call json%add(token_obj, 'text', tokens(i)%text)
            call json%add(token_obj, 'line', tokens(i)%line)
            call json%add(token_obj, 'column', tokens(i)%column)

            ! Add to array
            call json%add(token_array, token_obj)
        end do

        ! Write to file
        call json%print(root, filename)

        ! Clean up
        call json%destroy(root)
    end subroutine json_write_tokens_to_file

    ! Convert tokens array to JSON string
    function json_write_tokens_to_string(tokens) result(json_str)
        use lexer_core, only: token_t, token_type_name
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: json_str

        type(json_core) :: json
        type(json_value), pointer :: root, token_array, token_obj
        integer :: i

        ! Initialize JSON
        call json%initialize()

        ! Create root object
        call json%create_object(root, '')

        ! Create tokens array
        call json%create_array(token_array, 'tokens')
        call json%add(root, token_array)

        ! Add each token to array
        do i = 1, size(tokens)
            ! Create token object
            call json%create_object(token_obj, '')
            call json%add(token_obj, 'type', token_type_name(tokens(i)%kind))
            call json%add(token_obj, 'text', tokens(i)%text)
            call json%add(token_obj, 'line', tokens(i)%line)
            call json%add(token_obj, 'column', tokens(i)%column)

            ! Add to array
            call json%add(token_array, token_obj)
        end do

        ! Convert to string
        call json%print_to_string(root, json_str)

        ! Clean up
        call json%destroy(root)
    end function json_write_tokens_to_string

    ! Write AST to JSON file
    subroutine json_write_ast_to_file(ast, filename)
        use ast_base, only: ast_node
        class(ast_node), intent(in) :: ast
        character(len=*), intent(in) :: filename

        type(json_core) :: json
        type(json_value), pointer :: root

        ! Initialize JSON
        call json%initialize()

        ! Create root object
        call json%create_object(root, '')

        ! Add AST to root
        call ast%to_json(json, root)

        ! Write to file
        call json%print(root, filename)

        ! Clean up
        call json%destroy(root)
    end subroutine json_write_ast_to_file

    ! Convert AST to JSON string
    function json_write_ast_to_string(ast) result(json_str)
        use ast_base, only: ast_node
        class(ast_node), intent(in) :: ast
        character(len=:), allocatable :: json_str

        type(json_core) :: json
        type(json_value), pointer :: root

        ! Initialize JSON
        call json%initialize()

        ! Create root object
        call json%create_object(root, '')

        ! Add AST to root
        call ast%to_json(json, root)

        ! Convert to string
        call json%print_to_string(root, json_str)

        ! Clean up
        call json%destroy(root)
    end function json_write_ast_to_string

end module json_writer
