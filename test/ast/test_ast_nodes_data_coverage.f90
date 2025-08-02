program test_ast_nodes_data_coverage
    use ast_nodes_data
    use json_module
    use type_system_hm, only: mono_type_t
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== AST Nodes Data Coverage Tests ==="
    print *, ""

    ! Test factory functions
    call test_start("create_declaration factory basic")
    call test_create_declaration_basic()

    call test_start("create_declaration factory full")
    call test_create_declaration_full()

    call test_start("create_derived_type factory")
    call test_create_derived_type()

    ! Test declaration_node
    call test_start("declaration_node assignment basic")
    call test_declaration_assign_basic()

    call test_start("declaration_node assignment comprehensive")
    call test_declaration_assign_comprehensive()

    call test_start("declaration_node accept visitor")
    call test_declaration_accept()

    call test_start("declaration_node to_json")
    call test_declaration_to_json()

    ! Test parameter_declaration_node
    call test_start("parameter_declaration_node assignment")
    call test_parameter_declaration_assign()

    call test_start("parameter_declaration_node accept visitor")
    call test_parameter_declaration_accept()

    call test_start("parameter_declaration_node to_json")
    call test_parameter_declaration_to_json()

    ! Test module_node
    call test_start("module_node assignment")
    call test_module_assign()

    call test_start("module_node accept visitor")
    call test_module_accept()

    call test_start("module_node to_json")
    call test_module_to_json()

    ! Test derived_type_node
    call test_start("derived_type_node assignment")
    call test_derived_type_assign()

    call test_start("derived_type_node accept visitor")
    call test_derived_type_accept()

    call test_start("derived_type_node to_json")
    call test_derived_type_to_json()

    ! Test edge cases
    call test_start("declaration with var_names array")
    call test_declaration_var_names()

    call test_start("module with contains section")
    call test_module_with_contains()

    call test_start("derived_type with parameters")
    call test_derived_type_with_params()

    call test_start("assignment with inferred_type")
    call test_assignment_with_type()

    call print_results()

contains

    subroutine test_create_declaration_basic()
        type(declaration_node) :: node

        ! Test basic declaration
        node = create_declaration("integer", "x")
        
        if (node%type_name /= "integer") then
            call test_fail("type_name should be 'integer'")
            return
        end if
        
        if (node%var_name /= "x") then
            call test_fail("var_name should be 'x'")
            return
        end if
        
        if (node%has_kind) then
            call test_fail("has_kind should be false by default")
            return
        end if
        
        if (node%has_initializer) then
            call test_fail("has_initializer should be false by default")
            return
        end if

        call test_pass()
    end subroutine test_create_declaration_basic

    subroutine test_create_declaration_full()
        type(declaration_node) :: node
        integer, parameter :: dims(2) = [10, 20]

        ! Test full declaration with all parameters
        node = create_declaration("real", "arr", kind_value=8, &
                                 initializer_index=5, &
                                 dimension_indices=dims, &
                                 is_allocatable=.true., &
                                 is_pointer=.false., &
                                 is_target=.true., &
                                 line=15, column=25)
        
        if (node%type_name /= "real") then
            call test_fail("type_name should be 'real'")
            return
        end if
        
        if (.not. node%has_kind .or. node%kind_value /= 8) then
            call test_fail("kind should be 8")
            return
        end if
        
        if (.not. node%has_initializer .or. node%initializer_index /= 5) then
            call test_fail("initializer_index should be 5")
            return
        end if
        
        if (.not. node%is_array) then
            call test_fail("should be marked as array")
            return
        end if
        
        if (.not. allocated(node%dimension_indices)) then
            call test_fail("dimension_indices should be allocated")
            return
        end if
        
        if (size(node%dimension_indices) /= 2) then
            call test_fail("dimension_indices should have size 2")
            return
        end if
        
        if (.not. node%is_allocatable) then
            call test_fail("should be allocatable")
            return
        end if
        
        if (node%is_pointer) then
            call test_fail("should not be pointer")
            return
        end if
        
        if (.not. node%is_target) then
            call test_fail("should be target")
            return
        end if
        
        if (node%line /= 15 .or. node%column /= 25) then
            call test_fail("line/column should be 15/25")
            return
        end if

        call test_pass()
    end subroutine test_create_declaration_full

    subroutine test_create_derived_type()
        type(derived_type_node) :: node
        integer, parameter :: comps(3) = [1, 2, 3]
        integer, parameter :: params(2) = [10, 20]

        ! Test basic derived type
        node = create_derived_type("my_type")
        if (node%name /= "my_type") then
            call test_fail("name should be 'my_type'")
            return
        end if

        ! Test with components and parameters
        node = create_derived_type("complex_type", &
                                  component_indices=comps, &
                                  param_indices=params, &
                                  line=5, column=10)
        
        if (allocated(node%component_indices)) then
            if (size(node%component_indices) /= 3) then
                call test_fail("component_indices should have size 3")
                return
            end if
        else
            call test_fail("component_indices should be allocated")
            return
        end if
        
        if (allocated(node%param_indices)) then
            if (size(node%param_indices) /= 2) then
                call test_fail("param_indices should have size 2")
                return
            end if
        else
            call test_fail("param_indices should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_create_derived_type

    subroutine test_declaration_assign_basic()
        type(declaration_node) :: lhs, rhs

        ! Set up rhs
        rhs%type_name = "real"
        rhs%var_name = "y"
        rhs%kind_value = 4
        rhs%has_kind = .true.
        rhs%intent = "inout"
        rhs%has_intent = .true.
        rhs%initializer_index = 10
        rhs%has_initializer = .true.
        rhs%is_array = .true.
        rhs%is_allocatable = .true.
        rhs%is_pointer = .false.
        rhs%is_target = .true.

        ! Test assignment
        lhs = rhs

        if (lhs%type_name /= "real") then
            call test_fail("type_name should be copied")
            return
        end if
        
        if (lhs%var_name /= "y") then
            call test_fail("var_name should be copied")
            return
        end if
        
        if (lhs%kind_value /= 4 .or. .not. lhs%has_kind) then
            call test_fail("kind should be copied")
            return
        end if
        
        if (lhs%intent /= "inout" .or. .not. lhs%has_intent) then
            call test_fail("intent should be copied")
            return
        end if
        
        if (lhs%initializer_index /= 10 .or. .not. lhs%has_initializer) then
            call test_fail("initializer should be copied")
            return
        end if
        
        if (.not. lhs%is_array .or. .not. lhs%is_allocatable .or. &
            lhs%is_pointer .or. .not. lhs%is_target) then
            call test_fail("attributes should be copied")
            return
        end if

        call test_pass()
    end subroutine test_declaration_assign_basic

    subroutine test_declaration_assign_comprehensive()
        type(declaration_node) :: lhs, rhs
        integer, parameter :: dims(3) = [5, 10, 15]

        ! Test with dimension_indices
        allocate(rhs%dimension_indices(3))
        rhs%dimension_indices = dims
        rhs%type_name = "integer"
        rhs%var_name = "arr"

        lhs = rhs

        if (.not. allocated(lhs%dimension_indices)) then
            call test_fail("dimension_indices should be allocated")
            return
        end if
        
        if (size(lhs%dimension_indices) /= 3) then
            call test_fail("dimension_indices should have size 3")
            return
        end if
        
        if (lhs%dimension_indices(2) /= 10) then
            call test_fail("dimension_indices(2) should be 10")
            return
        end if

        ! Test reassignment with deallocation
        deallocate(rhs%dimension_indices)
        allocate(rhs%dimension_indices(1))
        rhs%dimension_indices(1) = 100

        lhs = rhs

        if (size(lhs%dimension_indices) /= 1) then
            call test_fail("dimension_indices should be reallocated")
            return
        end if
        
        if (lhs%dimension_indices(1) /= 100) then
            call test_fail("dimension_indices(1) should be 100")
            return
        end if

        call test_pass()
    end subroutine test_declaration_assign_comprehensive

    subroutine test_declaration_accept()
        type(declaration_node) :: node
        integer :: dummy_visitor

        ! Test accept (stub implementation)
        call node%accept(dummy_visitor)

        call test_pass()
    end subroutine test_declaration_accept

    subroutine test_declaration_to_json()
        type(declaration_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        ! Test to_json (stub implementation)
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call test_pass()
    end subroutine test_declaration_to_json

    subroutine test_parameter_declaration_assign()
        type(parameter_declaration_node) :: lhs, rhs
        integer, parameter :: dims(2) = [3, 6]

        ! Set up rhs
        rhs%name = "param"
        rhs%type_name = "character"
        rhs%kind_value = 1
        rhs%has_kind = .true.
        rhs%intent = "in"
        rhs%has_intent = .true.
        rhs%is_array = .true.
        allocate(rhs%dimension_indices(2))
        rhs%dimension_indices = dims

        lhs = rhs

        if (lhs%name /= "param") then
            call test_fail("name should be copied")
            return
        end if
        
        if (lhs%type_name /= "character") then
            call test_fail("type_name should be copied")
            return
        end if
        
        if (lhs%kind_value /= 1 .or. .not. lhs%has_kind) then
            call test_fail("kind should be copied")
            return
        end if
        
        if (lhs%intent /= "in" .or. .not. lhs%has_intent) then
            call test_fail("intent should be copied")
            return
        end if
        
        if (.not. lhs%is_array) then
            call test_fail("is_array should be copied")
            return
        end if
        
        if (.not. allocated(lhs%dimension_indices)) then
            call test_fail("dimension_indices should be allocated")
            return
        end if
        
        if (size(lhs%dimension_indices) /= 2) then
            call test_fail("dimension_indices should have size 2")
            return
        end if

        call test_pass()
    end subroutine test_parameter_declaration_assign

    subroutine test_parameter_declaration_accept()
        type(parameter_declaration_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_parameter_declaration_accept

    subroutine test_parameter_declaration_to_json()
        type(parameter_declaration_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)
        call test_pass()
    end subroutine test_parameter_declaration_to_json

    subroutine test_module_assign()
        type(module_node) :: lhs, rhs
        integer, parameter :: decls(2) = [1, 2]
        integer, parameter :: procs(3) = [10, 20, 30]

        ! Set up rhs
        rhs%name = "test_module"
        allocate(rhs%declaration_indices(2))
        rhs%declaration_indices = decls
        allocate(rhs%procedure_indices(3))
        rhs%procedure_indices = procs
        rhs%has_contains = .true.

        lhs = rhs

        if (lhs%name /= "test_module") then
            call test_fail("name should be copied")
            return
        end if
        
        if (.not. allocated(lhs%declaration_indices)) then
            call test_fail("declaration_indices should be allocated")
            return
        end if
        
        if (size(lhs%declaration_indices) /= 2) then
            call test_fail("declaration_indices should have size 2")
            return
        end if
        
        if (.not. allocated(lhs%procedure_indices)) then
            call test_fail("procedure_indices should be allocated")
            return
        end if
        
        if (size(lhs%procedure_indices) /= 3) then
            call test_fail("procedure_indices should have size 3")
            return
        end if
        
        if (.not. lhs%has_contains) then
            call test_fail("has_contains should be copied")
            return
        end if

        call test_pass()
    end subroutine test_module_assign

    subroutine test_module_accept()
        type(module_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_module_accept

    subroutine test_module_to_json()
        type(module_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)
        call test_pass()
    end subroutine test_module_to_json

    subroutine test_derived_type_assign()
        type(derived_type_node) :: lhs, rhs
        integer, parameter :: comps(2) = [5, 10]
        integer, parameter :: params(1) = [15]

        ! Set up rhs
        rhs%name = "my_type"
        allocate(rhs%component_indices(2))
        rhs%component_indices = comps
        rhs%has_parameters = .true.
        allocate(rhs%param_indices(1))
        rhs%param_indices = params

        lhs = rhs

        if (lhs%name /= "my_type") then
            call test_fail("name should be copied")
            return
        end if
        
        if (.not. allocated(lhs%component_indices)) then
            call test_fail("component_indices should be allocated")
            return
        end if
        
        if (size(lhs%component_indices) /= 2) then
            call test_fail("component_indices should have size 2")
            return
        end if
        
        if (.not. lhs%has_parameters) then
            call test_fail("has_parameters should be copied")
            return
        end if
        
        if (.not. allocated(lhs%param_indices)) then
            call test_fail("param_indices should be allocated")
            return
        end if
        
        if (size(lhs%param_indices) /= 1) then
            call test_fail("param_indices should have size 1")
            return
        end if

        call test_pass()
    end subroutine test_derived_type_assign

    subroutine test_derived_type_accept()
        type(derived_type_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_derived_type_accept

    subroutine test_derived_type_to_json()
        type(derived_type_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)
        call test_pass()
    end subroutine test_derived_type_to_json

    subroutine test_declaration_var_names()
        type(declaration_node) :: lhs, rhs

        ! Test var_names array field
        allocate(character(len=10) :: rhs%var_names(3))
        rhs%var_names(1) = "a"
        rhs%var_names(2) = "b"
        rhs%var_names(3) = "c"
        rhs%is_multi_declaration = .true.
        rhs%type_name = "integer"

        lhs = rhs

        if (.not. lhs%is_multi_declaration) then
            call test_fail("is_multi_declaration should be copied")
            return
        end if

        call test_pass()
    end subroutine test_declaration_var_names

    subroutine test_module_with_contains()
        type(module_node) :: node

        ! Test module with contains section
        node%name = "test_mod"
        node%has_contains = .true.
        
        if (.not. node%has_contains) then
            call test_fail("has_contains should be true")
            return
        end if

        call test_pass()
    end subroutine test_module_with_contains

    subroutine test_derived_type_with_params()
        type(derived_type_node) :: node

        ! Test derived type with parameters
        node%name = "parameterized_type"
        node%has_parameters = .true.
        
        if (.not. node%has_parameters) then
            call test_fail("has_parameters should be true")
            return
        end if

        call test_pass()
    end subroutine test_derived_type_with_params

    subroutine test_assignment_with_type()
        type(declaration_node) :: lhs, rhs
        type(mono_type_t) :: test_type

        ! Test assignment with inferred_type (will fail due to missing base field copying)
        rhs%type_name = "real"
        rhs%line = 42
        rhs%column = 15
        allocate(rhs%inferred_type)
        rhs%inferred_type = test_type

        lhs = rhs

        ! These tests will fail until we fix the assignment operators
        if (lhs%line == 42 .and. lhs%column == 15) then
            call test_pass()
        else
            call test_fail("Base fields not copied - assignment operators need fixing")
        end if
    end subroutine test_assignment_with_type

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A)', advance='no') "Testing " // test_name // "... "
    end subroutine test_start

    subroutine test_pass()
        passed_tests = passed_tests + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(message)
        character(len=*), intent(in) :: message
        print *, "FAIL: " // message
    end subroutine test_fail

    subroutine print_results()
        print *, ""
        print *, "=== Test Results ==="
        write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
        if (passed_tests == total_tests) then
            print *, "All AST nodes data coverage tests passed!"
        else
            print *, "Some tests failed!"
            stop 1
        end if
    end subroutine print_results

end program test_ast_nodes_data_coverage