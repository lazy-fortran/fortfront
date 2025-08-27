program test_manual_517
    use ast_factory
    use ast_arena_modern
    use codegen_core
    implicit none
    
    ! Manually create a multi-unit container to test codegen
    type(ast_arena_t) :: arena
    integer :: decl_index, prog_index, multi_index
    character(len=:), allocatable :: result_code
    
    print *, "Manual test of multi-unit container for Issue #517"
    
    ! Create arena
    arena = create_ast_arena(100)
    
    ! Create a simple declaration (simulate type declaration)
    decl_index = push_declaration(arena, "integer", "x", line=1, column=1)
    
    ! Create a simple program  
    prog_index = push_program(arena, "main", [integer :: ], 1, 1)
    
    ! Create multi-unit container
    multi_index = push_program(arena, "__MULTI_UNIT__", [decl_index, prog_index], 1, 1)
    
    print *, "Created multi-unit container at index:", multi_index
    print *, "Arena size:", arena%size
    
    ! Generate code
    result_code = generate_code_from_arena(arena, multi_index)
    
    print *, ""
    print *, "Generated code:"
    print *, result_code
    
end program test_manual_517