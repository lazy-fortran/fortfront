program test_type_contains_bindings
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: derived_type_node, type_binding_node
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = &
                                   "type :: atype"//new_line('A')// &
                                   "   integer :: field"//new_line('A')// &
                                   "contains"//new_line('A')// &
                              "   procedure :: method => impl_method"//new_line('A')// &
                                   "   procedure :: another"//new_line('A')// &
                                   "end type atype"

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: mod_index
    character(len=:), allocatable :: error_msg
    type(derived_type_node) :: dtype
    type(type_binding_node) :: binding

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, mod_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: parse error:", trim(error_msg)
        stop 1
    end if

    if (mod_index <= 0) then
        print *, "FAIL: parse failed, no index returned"
        stop 1
    end if

    if (arena%size < 1) then
        print *, "FAIL: arena is empty"
        stop 1
    end if

    block
        integer :: i
        logical :: found_dtype
        found_dtype = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (derived_type_node)
                dtype = node
                found_dtype = .true.
                exit
            end select
        end do
        if (.not. found_dtype) then
            print *, "FAIL: derived_type_node not found in arena"
            stop 1
        end if
    end block

    if (.true.) then
        dtype = dtype
        if (.not. dtype%has_contains) then
            print *, "FAIL: has_contains should be true"
            stop 1
        end if

        if (.not. allocated(dtype%binding_indices)) then
            print *, "FAIL: binding_indices not allocated"
            stop 1
        end if

        if (size(dtype%binding_indices) /= 2) then
            print *, "FAIL: expected 2 bindings, got", size(dtype%binding_indices)
            stop 1
        end if

        select type (b1 => arena%entries(dtype%binding_indices(1))%node)
        type is (type_binding_node)
            binding = b1
            if (binding%binding_name /= "method") then
                print *, "FAIL: first binding name should be method, got", &
                    binding%binding_name
                stop 1
            end if
            if (.not. allocated(binding%implementation)) then
                print *, "FAIL: first binding implementation not allocated"
                stop 1
            end if
            if (binding%implementation /= "impl_method") then
                print *, "FAIL: first binding implementation should be impl_method"
                stop 1
            end if
        class default
            print *, "FAIL: first binding is not a type_binding_node"
            stop 1
        end select

        select type (b2 => arena%entries(dtype%binding_indices(2))%node)
        type is (type_binding_node)
            binding = b2
            if (binding%binding_name /= "another") then
                print *, "FAIL: second binding name should be another"
                stop 1
            end if
        class default
            print *, "FAIL: second binding is not a type_binding_node"
            stop 1
        end select
    end if

    print *, "PASS: type CONTAINS section with bindings parsed correctly"

end program test_type_contains_bindings
