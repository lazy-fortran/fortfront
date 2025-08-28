program test_direct_multi_var
    use ast_core
    use ast_factory
    use codegen_declarations
    implicit none
    
    type(ast_arena_t) :: arena
    type(declaration_node) :: decl
    character(len=:), allocatable :: code
    integer :: decl_index
    
    ! Create a multi-variable declaration node manually
    decl%type_name = "integer"
    decl%is_multi_declaration = .true.
    
    ! Allocate and set variable names
    allocate(character(len=64) :: decl%var_names(3))
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
    
end program test_direct_multi_var