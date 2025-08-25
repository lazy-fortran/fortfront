program test_node_mapping
    ! Tests for Node Mapping System
    ! =============================
    ! Tests bidirectional CST/AST UID mapping functionality
    
    use, intrinsic :: iso_fortran_env, only: int64
    use node_mapping, only: node_mapping_t, create_node_mapping, lookup_result_t
    use uid_generator, only: uid_t, init_uid_generator
    use error_handling, only: result_t
    implicit none
    
    type(node_mapping_t) :: mapping
    type(uid_t) :: cst_uid1, cst_uid2, ast_uid1, ast_uid2
    type(lookup_result_t) :: lookup_res
    logical :: test_passed
    
    ! Initialize UID generator
    call init_uid_generator()
    
    print '(A)', "=== Node Mapping Tests ==="
    test_passed = .true.
    
    ! Test 1: Create mapping
    print '(A)', "Test 1: Creating node mapping..."
    mapping = create_node_mapping()
    if (mapping%is_empty()) then
        print '(A)', "✓ Empty mapping created successfully"
    else
        print '(A)', "✗ Mapping should be empty initially"
        test_passed = .false.
    end if
    
    ! Test 2: Add mappings
    print '(A)', "Test 2: Adding mappings..."
    cst_uid1%value = 1001_int64
    ast_uid1%value = 2001_int64
    cst_uid2%value = 1002_int64
    ast_uid2%value = 2002_int64
    
    block
        type(result_t) :: add_result
        add_result = mapping%add_mapping(cst_uid1, ast_uid1, 10, 20)
        if (add_result%is_success()) then
            print '(A)', "✓ First mapping added successfully"
        else
            print '(A)', "✗ Failed to add first mapping"
            test_passed = .false.
        end if
        
        add_result = mapping%add_mapping(cst_uid2, ast_uid2, 30, 40)
        if (add_result%is_success()) then
            print '(A)', "✓ Second mapping added successfully"
        else
            print '(A)', "✗ Failed to add second mapping"
            test_passed = .false.
        end if
    end block
    
    ! Test 3: CST to AST lookup
    print '(A)', "Test 3: CST to AST lookup..."
    lookup_res = mapping%cst_to_ast(cst_uid1)
    if (lookup_res%found .and. lookup_res%found_uid%value == 2001_int64) then
        print '(A)', "✓ CST to AST lookup successful"
    else
        print '(A)', "✗ CST to AST lookup failed"
        test_passed = .false.
    end if
    
    ! Test 4: AST to CST lookup
    print '(A)', "Test 4: AST to CST lookup..."
    lookup_res = mapping%ast_to_cst(ast_uid2)
    if (lookup_res%found .and. lookup_res%found_uid%value == 1002_int64) then
        print '(A)', "✓ AST to CST lookup successful"
    else
        print '(A)', "✗ AST to CST lookup failed"
        test_passed = .false.
    end if
    
    ! Test 5: Size check
    print '(A)', "Test 5: Size verification..."
    if (mapping%get_size() == 2) then
        print '(A)', "✓ Mapping size correct"
    else
        print '(A)', "✗ Incorrect mapping size"
        test_passed = .false.
    end if
    
    ! Test 6: Clear mapping
    print '(A)', "Test 6: Clear mapping..."
    call mapping%clear()
    if (mapping%is_empty()) then
        print '(A)', "✓ Mapping cleared successfully"
    else
        print '(A)', "✗ Mapping should be empty after clear"
        test_passed = .false.
    end if
    
    print '(A)', ""
    if (test_passed) then
        print '(A)', "✓ All node mapping tests passed!"
    else
        print '(A)', "✗ Some node mapping tests failed"
        error stop 1
    end if

end program test_node_mapping