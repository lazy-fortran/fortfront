program test_cst_converter_basic
    ! Basic compilation test for CST to AST converter
    ! ===============================================
    ! Simple smoke test to ensure modules compile and basic usage works
    
    use, intrinsic :: iso_fortran_env, only: int64
    use cst_to_ast_converter, only: cst_to_ast_converter_t, create_converter
    use node_mapping, only: node_mapping_t, create_node_mapping
    use uid_generator, only: init_uid_generator
    implicit none
    
    type(cst_to_ast_converter_t) :: converter
    type(node_mapping_t) :: mapping
    
    ! Initialize UID generator
    call init_uid_generator()
    
    print '(A)', "=== Basic CST Converter Test ==="
    
    ! Test converter creation
    print '(A)', "Creating converter..."
    converter = create_converter()
    print '(A)', "✓ Converter created successfully"
    
    ! Test mapping creation  
    print '(A)', "Creating node mapping..."
    mapping = create_node_mapping()
    print '(A)', "✓ Node mapping created successfully"
    
    print '(A)', ""
    print '(A)', "✓ All basic tests passed!"
    print '(A)', "CST to AST converter system compiles correctly"

end program test_cst_converter_basic