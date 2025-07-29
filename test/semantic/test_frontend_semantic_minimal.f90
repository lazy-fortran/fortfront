program test_frontend_semantic_minimal
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    implicit none

    type(semantic_context_t) :: ctx

    print *, "Testing semantic context creation..."

    ! This should not crash
    ctx = create_semantic_context()

    print *, "PASS: Semantic context created successfully"
    print *, "next_var_id =", ctx%next_var_id

    stop 0

end program test_frontend_semantic_minimal
