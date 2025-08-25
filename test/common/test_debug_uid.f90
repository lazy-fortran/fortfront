program test_debug_uid
    use uid_generator
    implicit none
    
    type(uid_t) :: uid1, uid2, uid3
    
    print *, "Before init:"
    uid1 = generate_uid()
    print *, "UID1 value:", uid1%value, "generation:", uid1%generation
    
    print *, "After init:"
    call init_uid_generator()
    uid2 = generate_uid()
    print *, "UID2 value:", uid2%value, "generation:", uid2%generation
    
    uid3 = generate_uid()
    print *, "UID3 value:", uid3%value, "generation:", uid3%generation
    
end program test_debug_uid