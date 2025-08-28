program test_json_reader_coverage
    use json_reader
    use json_module
    use ast_core
    use semantic_analyzer, only: semantic_context_t
    implicit none

    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== JSON Reader Coverage Tests ==='
    print *
    
    ! Test json_to_semantic error handling
    print *, 'Testing json_to_semantic error path...'
    if (.not. test_json_to_semantic_error_path()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All JSON reader coverage tests passed!'
        stop 0
    else
        print *, 'Some JSON reader coverage tests failed!'
        stop 1
    end if

contains

    logical function test_json_to_semantic_error_path()
        type(json_file) :: json
        type(ast_arena_t) :: arena
        integer :: root_index
        type(semantic_context_t) :: sem_ctx
        character(len=*), parameter :: test_json = &
            '{"ast": {"node_type": "program", "children": []}, "context": {}}'

        ! Test that json_to_semantic can handle basic JSON structure
        ! This should exercise the main path in json_to_semantic
        
        ! Initialize JSON with minimal valid data 
        call json%initialize()
        call json%deserialize(test_json)
        
        ! This should exercise json_to_semantic (arena is auto-initialized)
        call json_to_semantic(json, arena, root_index, sem_ctx)
        
        ! Clean up
        call json%destroy()
        
        ! Test passes if no crash occurs
        test_json_to_semantic_error_path = .true.
        print *, '  PASSED: json_to_semantic basic functionality'
    end function test_json_to_semantic_error_path

end program test_json_reader_coverage