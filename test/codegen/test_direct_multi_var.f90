program test_direct_multi_var
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_factory
    use codegen_declarations
    implicit none

    type(ast_arena_t) :: arena
    type(declaration_node) :: decl
    character(len=:), allocatable :: code
    integer :: decl_index, copied_index

    ! Create a multi-variable declaration node manually
    decl%type_name = "integer"
    decl%is_multi_declaration = .true.

    ! Allocate and set variable names
    allocate (character(len=64) :: decl%var_names(3))
    decl%var_names(1) = "x"
    decl%var_names(2) = "y"
    decl%var_names(3) = "z"

    ! Set primary variable name
    decl%var_name = "x"

    ! Push to arena
    call arena%push(decl, "test_declaration", 0)
    decl_index = arena%size

    ! Generate code
    code = generate_code_declaration(arena, decl, decl_index)

    print *, 'Generated code:'
    print *, '"', code, '"'

    ! Check result
    if (index(code, 'integer :: x, y, z') > 0) then
        print *, 'SUCCESS: Multi-variable codegen works correctly'
    else
        print *, 'FAILURE: Multi-variable codegen broken'
        print *, 'Expected: integer :: x, y, z'
        print *, 'Got: ', code
    end if

    ! The parser/factory path uses a deferred-length character array.  Keep
    ! this separate from the fixed-length hand-built node above: it catches
    ! loss of later names when the polymorphic arena stores the node.
    copied_index = push_declaration(arena, "integer", &
        [character(len=1) :: "a", "b", "c"])
    if (copied_index <= 0) error stop "multi declaration factory failed"
    select type (copied => arena%entries(copied_index)%node)
    type is (declaration_node)
        if (.not. allocated(copied%var_names)) then
            error stop "multi declaration names were not stored"
        end if
        if (size(copied%var_names) /= 3) then
            error stop "multi declaration name count changed"
        end if
        if (trim(copied%var_names(1)) /= "a" .or. &
            trim(copied%var_names(2)) /= "b" .or. &
            trim(copied%var_names(3)) /= "c") then
            error stop "multi declaration names were corrupted"
        end if
    class default
        error stop "multi declaration node type changed"
    end select

end program test_direct_multi_var
