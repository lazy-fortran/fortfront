program test_simple_uid
    use uid_generator
    use ast_core
    implicit none
    
    type(identifier_node) :: id1, id2
    type(literal_node) :: lit
    character(len=32) :: uid_str1, uid_str2, uid_str3
    
    ! Initialize UID generator
    call init_uid_generator()
    
    ! Create nodes
    id1 = create_identifier("x", 1, 1)
    id2 = create_identifier("y", 1, 5)
    lit = create_literal("42", LITERAL_INTEGER, 1, 10)
    
    ! Convert UIDs to strings
    uid_str1 = uid_to_string(id1%uid)
    uid_str2 = uid_to_string(id2%uid)
    uid_str3 = uid_to_string(lit%uid)
    
    ! Print UIDs
    print *, "ID1 UID: ", trim(uid_str1)
    print *, "ID2 UID: ", trim(uid_str2)
    print *, "LIT UID: ", trim(uid_str3)
    
    ! Check validity
    print *, "ID1 valid: ", id1%uid%is_valid()
    print *, "ID2 valid: ", id2%uid%is_valid()
    print *, "LIT valid: ", lit%uid%is_valid()
    
    ! Check uniqueness
    print *, "ID1 != ID2: ", .not. uid_equal(id1%uid, id2%uid)
    print *, "ID1 != LIT: ", .not. uid_equal(id1%uid, lit%uid)
    print *, "ID2 != LIT: ", .not. uid_equal(id2%uid, lit%uid)
    
end program test_simple_uid