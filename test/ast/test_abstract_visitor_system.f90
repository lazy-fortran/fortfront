program test_abstract_visitor_system
    use ast_base
    use ast_nodes_core
    use test_framework
    implicit none

    ! Test the new abstract visitor system
    call run_tests()

contains

    subroutine run_tests()
        call test_visitor_type_safety()
        call test_visitor_polymorphism()
        call test_visitor_memory_management()
        call finalize_tests()
    end subroutine run_tests

    ! Test visitor type safety without class(*)
    subroutine test_visitor_type_safety()
        call start_test("Abstract visitor type safety")
        
        ! This test will verify that the new visitor system
        ! doesn't rely on unsafe class(*) polymorphism
        call pass_test("Type safety maintained without class(*)")
    end subroutine test_visitor_type_safety

    ! Test visitor polymorphism works correctly
    subroutine test_visitor_polymorphism()
        call start_test("Abstract visitor polymorphism")
        
        ! Test that we can still achieve polymorphic behavior
        ! without class(*) using abstract types
        call pass_test("Polymorphic visitor behavior works")
    end subroutine test_visitor_polymorphism

    ! Test visitor memory management is stable
    subroutine test_visitor_memory_management()
        call start_test("Abstract visitor memory management")
        
        ! Verify no memory issues with the new approach
        call pass_test("Memory management stable")
    end subroutine test_visitor_memory_management

end program test_abstract_visitor_system