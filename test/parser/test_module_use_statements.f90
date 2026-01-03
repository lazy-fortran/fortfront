program test_module_use_statements
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: module_node
    use ast_nodes_misc, only: use_statement_node
    use lexer_core, only: token_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    character(len=*), parameter :: source = &
                                   "module module_use_demo" // new_line('a') // &
                                   "    use, intrinsic :: iso_fortran_env, only: " // &
                                   "dp => real64, error_unit" // new_line('a') // &
                                   "    implicit none" // new_line('a') // &
                                   "contains" // new_line('a') // &
                                   "    subroutine touch()" // new_line('a') // &
                                   "        real(dp) :: value" // new_line('a') // &
                                   "        value = 1.0_dp" // new_line('a') // &
                                   "        print *, value" // new_line('a') // &
                                   "    end subroutine touch" // new_line('a') // &
                                   "end module module_use_demo"

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg
    logical :: found_use

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: parser error:", trim(error_msg)
        stop 1
    end if

    if (root_index <= 0) then
        print *, "FAIL: parser did not return a module root index"
        stop 1
    end if

    if (.not. allocated(arena%entries(root_index)%node)) then
        print *, "FAIL: module node not allocated in arena"
        stop 1
    end if

    found_use = .false.
    select type (mod => arena%entries(root_index)%node)
    type is (module_node)
        if (.not. allocated(mod%declaration_indices)) then
            print *, "FAIL: module has no declaration indices"
            stop 1
        end if

        call verify_use_entries(arena, mod%declaration_indices, found_use)
    class default
        print *, "FAIL: root node is not a module"
        stop 1
    end select

    if (.not. found_use) then
        print *, "FAIL: parser did not emit a use_statement_node"
        stop 1
    end if

    print *, "PASS: module-level USE statements preserved in AST"

contains

    subroutine verify_use_entries(arena, decl_indices, found_use)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_indices(:)
        logical, intent(inout) :: found_use
        integer :: i, idx

        do i = 1, size(decl_indices)
            idx = decl_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (use_node => arena%entries(idx)%node)
            type is (use_statement_node)
                call assert_use_contents(use_node)
                found_use = .true.
            end select
        end do
    end subroutine verify_use_entries

    subroutine assert_use_contents(node)
        type(use_statement_node), intent(in) :: node

        if (.not. allocated(node%module_name)) then
            print *, "FAIL: use statement missing module name"
            stop 1
        end if

        if (trim(node%module_name) /= "iso_fortran_env") then
            print *, "FAIL: module name mismatch: ", trim(node%module_name)
            stop 1
        end if

        if (.not. node%has_only) then
            print *, "FAIL: use statement lost ONLY qualifier"
            stop 1
        end if

        if (.not. allocated(node%rename_list)) then
            print *, "FAIL: rename list not allocated"
            stop 1
        end if

        if (size(node%rename_list) /= 2) then
            print *, "FAIL: rename list size mismatch"
            stop 1
        end if

        if (.not. allocated(node%rename_list(1)%s) .or. &
            trim(node%rename_list(1)%s) /= "dp") then
            print *, "FAIL: rename list local name missing"
            stop 1
        end if

        if (.not. allocated(node%rename_list(2)%s) .or. &
            trim(node%rename_list(2)%s) /= "real64") then
            print *, "FAIL: rename list remote name missing"
            stop 1
        end if

        if (.not. allocated(node%only_list)) then
            print *, "FAIL: ONLY list not allocated"
            stop 1
        end if

        if (size(node%only_list) < 1) then
            print *, "FAIL: ONLY list unexpectedly empty"
            stop 1
        end if

        if (.not. allocated(node%only_list(1)%s)) then
            print *, "FAIL: ONLY list entry not allocated"
            stop 1
        end if

        if (trim(node%only_list(1)%s) /= "error_unit") then
            print *, "FAIL: ONLY list lost error_unit entry"
            stop 1
        end if
    end subroutine assert_use_contents

end program test_module_use_statements
