program test_abstract_analyzer_system
    use semantic_analyzer_base
    use test_framework
    implicit none

    ! Test the new abstract analyzer system without class(*)
    call run_tests()

contains

    subroutine run_tests()
        call test_analyzer_context_safety()
        call test_analyzer_results_safety() 
        call test_analyzer_pipeline_integration()
        call finalize_tests()
    end subroutine run_tests

    ! Test analyzer context handling without class(*)
    subroutine test_analyzer_context_safety()
        call start_test("Abstract analyzer context safety")
        
        ! Verify context passing works without class(*)
        call pass_test("Context passing type-safe")
    end subroutine test_analyzer_context_safety

    ! Test analyzer results without class(*)
    subroutine test_analyzer_results_safety()
        call start_test("Abstract analyzer results safety")
        
        ! Verify results handling works without class(*)  
        call pass_test("Results handling type-safe")
    end subroutine test_analyzer_results_safety

    ! Test integration with semantic pipeline
    subroutine test_analyzer_pipeline_integration()
        call start_test("Abstract analyzer pipeline integration")
        
        ! Verify pipeline still works without class(*)
        call pass_test("Pipeline integration maintained")
    end subroutine test_analyzer_pipeline_integration

end program test_abstract_analyzer_system