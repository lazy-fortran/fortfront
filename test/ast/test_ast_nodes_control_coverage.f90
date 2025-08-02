program test_ast_nodes_control_coverage
    use ast_nodes_control
    use json_module
    use type_system_hm, only: mono_type_t
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== AST Nodes Control Coverage Tests ==="
    print *, ""

    ! Test if_node
    call test_start("if_node assignment basic")
    call test_if_assign_basic()

    call test_start("if_node assignment with elseif")
    call test_if_assign_with_elseif()

    call test_start("if_node assignment with type deallocation")
    call test_if_assign_with_type_deallocation()

    call test_start("if_node accept visitor")
    call test_if_accept()

    call test_start("if_node to_json")
    call test_if_to_json()

    ! Test do_loop_node
    call test_start("do_loop_node assignment basic")
    call test_do_loop_assign_basic()

    call test_start("do_loop_node assignment with body")
    call test_do_loop_assign_with_body()

    call test_start("do_loop_node accept visitor")
    call test_do_loop_accept()

    call test_start("do_loop_node to_json")
    call test_do_loop_to_json()

    call test_start("do_loop_node factory function")
    call test_do_loop_factory()

    ! Test do_while_node
    call test_start("do_while_node assignment")
    call test_do_while_assign()

    call test_start("do_while_node accept visitor")
    call test_do_while_accept()

    call test_start("do_while_node to_json")
    call test_do_while_to_json()

    call test_start("do_while_node factory function")
    call test_do_while_factory()

    ! Test forall_node
    call test_start("forall_node assignment basic")
    call test_forall_assign_basic()

    call test_start("forall_node assignment comprehensive")
    call test_forall_assign_comprehensive()

    call test_start("forall_node accept visitor")
    call test_forall_accept()

    call test_start("forall_node to_json")
    call test_forall_to_json()

    ! Test select_case_node
    call test_start("select_case_node assignment")
    call test_select_case_assign()

    call test_start("select_case_node accept visitor")
    call test_select_case_accept()

    call test_start("select_case_node to_json")
    call test_select_case_to_json()

    call test_start("select_case_node factory function")
    call test_select_case_factory()

    ! Test case_block_node
    call test_start("case_block_node assignment")
    call test_case_block_assign()

    call test_start("case_block_node accept visitor")
    call test_case_block_accept()

    call test_start("case_block_node to_json")
    call test_case_block_to_json()

    ! Test case_range_node
    call test_start("case_range_node assignment")
    call test_case_range_assign()

    call test_start("case_range_node accept visitor")
    call test_case_range_accept()

    call test_start("case_range_node to_json")
    call test_case_range_to_json()

    ! Test case_default_node
    call test_start("case_default_node assignment")
    call test_case_default_assign()

    call test_start("case_default_node accept visitor")
    call test_case_default_accept()

    call test_start("case_default_node to_json")
    call test_case_default_to_json()

    ! Test where_node
    call test_start("where_node assignment basic")
    call test_where_assign_basic()

    call test_start("where_node assignment with elsewhere")
    call test_where_assign_with_elsewhere()

    call test_start("where_node accept visitor")
    call test_where_accept()

    call test_start("where_node to_json")
    call test_where_to_json()

    ! Test where_stmt_node
    call test_start("where_stmt_node assignment")
    call test_where_stmt_assign()

    call test_start("where_stmt_node accept visitor")
    call test_where_stmt_accept()

    call test_start("where_stmt_node to_json")
    call test_where_stmt_to_json()

    ! Test cycle_node
    call test_start("cycle_node assignment")
    call test_cycle_assign()

    call test_start("cycle_node accept visitor")
    call test_cycle_accept()

    call test_start("cycle_node to_json")
    call test_cycle_to_json()

    ! Test exit_node
    call test_start("exit_node assignment")
    call test_exit_assign()

    call test_start("exit_node accept visitor")
    call test_exit_accept()

    call test_start("exit_node to_json")
    call test_exit_to_json()

    ! Test stop_node
    call test_start("stop_node assignment")
    call test_stop_assign()

    call test_start("stop_node accept visitor")
    call test_stop_accept()

    call test_start("stop_node to_json")
    call test_stop_to_json()

    ! Test return_node
    call test_start("return_node assignment")
    call test_return_assign()

    call test_start("return_node accept visitor")
    call test_return_accept()

    call test_start("return_node to_json")
    call test_return_to_json()

    ! Test associate_node
    call test_start("associate_node assignment")
    call test_associate_assign()

    call test_start("associate_node accept visitor")
    call test_associate_accept()

    call test_start("associate_node to_json")
    call test_associate_to_json()

    call test_start("associate_node factory function")
    call test_associate_factory()

    ! Test if factory function
    call test_start("if_node factory function")
    call test_if_factory()

    call print_results()

contains

    ! if_node tests
    subroutine test_if_assign_basic()
        type(if_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 10
        rhs%column = 5
        rhs%condition_index = 1
        allocate(rhs%then_body_indices(2))
        rhs%then_body_indices = [2, 3]
        allocate(rhs%else_body_indices(1))
        rhs%else_body_indices = [4]

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 10 .or. lhs%column /= 5) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%condition_index /= 1) then
            call test_fail("condition_index should be copied")
            return
        end if
        
        if (.not. allocated(lhs%then_body_indices)) then
            call test_fail("then_body_indices should be allocated")
            return
        end if
        
        if (size(lhs%then_body_indices) /= 2) then
            call test_fail("then_body_indices size should be 2")
            return
        end if
        
        if (.not. allocated(lhs%else_body_indices)) then
            call test_fail("else_body_indices should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_if_assign_basic

    subroutine test_if_assign_with_elseif()
        type(if_node) :: lhs, rhs
        type(elseif_wrapper) :: elseif_block

        ! Set up elseif block
        elseif_block%condition_index = 5
        allocate(elseif_block%body_indices(2))
        elseif_block%body_indices = [6, 7]

        ! Set up rhs with elseif
        rhs%condition_index = 1
        allocate(rhs%elseif_blocks(1))
        rhs%elseif_blocks(1) = elseif_block

        ! Test assignment
        lhs = rhs

        if (.not. allocated(lhs%elseif_blocks)) then
            call test_fail("elseif_blocks should be allocated")
            return
        end if
        
        if (size(lhs%elseif_blocks) /= 1) then
            call test_fail("elseif_blocks size should be 1")
            return
        end if
        
        if (lhs%elseif_blocks(1)%condition_index /= 5) then
            call test_fail("elseif condition_index should be copied")
            return
        end if

        call test_pass()
    end subroutine test_if_assign_with_elseif

    subroutine test_if_assign_with_type_deallocation()
        type(if_node) :: lhs, rhs

        ! Set up lhs with allocated inferred_type
        lhs%line = 1
        allocate(lhs%inferred_type)
        
        ! Set up rhs without inferred_type
        rhs%line = 2
        rhs%condition_index = 10

        ! Test assignment - should deallocate lhs%inferred_type
        lhs = rhs

        if (lhs%line /= 2) then
            call test_fail("line should be updated")
            return
        end if
        
        if (allocated(lhs%inferred_type)) then
            call test_fail("inferred_type should be deallocated")
            return
        end if

        call test_pass()
    end subroutine test_if_assign_with_type_deallocation

    subroutine test_if_accept()
        type(if_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_if_accept

    subroutine test_if_to_json()
        type(if_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 15
        node%column = 3
        node%condition_index = 1

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        ! Verify JSON was created
        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_if_to_json

    ! do_loop_node tests
    subroutine test_do_loop_assign_basic()
        type(do_loop_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 20
        rhs%column = 1
        rhs%var_name = "i"
        rhs%start_expr_index = 1
        rhs%end_expr_index = 2
        rhs%step_expr_index = 3
        rhs%label = "loop1"

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 20 .or. lhs%column /= 1) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%var_name)) then
            call test_fail("var_name should be allocated")
            return
        end if
        
        if (lhs%var_name /= "i") then
            call test_fail("var_name should be 'i'")
            return
        end if
        
        if (lhs%start_expr_index /= 1) then
            call test_fail("start_expr_index should be 1")
            return
        end if
        
        if (lhs%end_expr_index /= 2) then
            call test_fail("end_expr_index should be 2")
            return
        end if
        
        if (lhs%step_expr_index /= 3) then
            call test_fail("step_expr_index should be 3")
            return
        end if
        
        if (.not. allocated(lhs%label)) then
            call test_fail("label should be allocated")
            return
        end if
        
        if (lhs%label /= "loop1") then
            call test_fail("label should be 'loop1'")
            return
        end if

        call test_pass()
    end subroutine test_do_loop_assign_basic

    subroutine test_do_loop_assign_with_body()
        type(do_loop_node) :: lhs, rhs
        integer, parameter :: body(3) = [10, 11, 12]

        ! Set up rhs with body
        rhs%var_name = "j"
        allocate(rhs%body_indices(3))
        rhs%body_indices = body

        ! Test assignment
        lhs = rhs

        if (.not. allocated(lhs%body_indices)) then
            call test_fail("body_indices should be allocated")
            return
        end if
        
        if (size(lhs%body_indices) /= 3) then
            call test_fail("body_indices size should be 3")
            return
        end if
        
        if (lhs%body_indices(2) /= 11) then
            call test_fail("body_indices(2) should be 11")
            return
        end if

        call test_pass()
    end subroutine test_do_loop_assign_with_body

    subroutine test_do_loop_accept()
        type(do_loop_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_do_loop_accept

    subroutine test_do_loop_to_json()
        type(do_loop_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 25
        node%column = 5
        node%var_name = "k"
        node%start_expr_index = 1
        node%end_expr_index = 10

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        ! Verify JSON was created
        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_do_loop_to_json

    subroutine test_do_loop_factory()
        type(do_loop_node) :: node
        integer, parameter :: body_indices(2) = [5, 6]

        node = create_do_loop("idx", 1, 100, 2, body_indices, 30, 10)

        if (node%var_name /= "idx") then
            call test_fail("var_name should be 'idx'")
            return
        end if
        
        if (node%start_expr_index /= 1) then
            call test_fail("start_expr_index should be 1")
            return
        end if
        
        if (node%end_expr_index /= 100) then
            call test_fail("end_expr_index should be 100")
            return
        end if
        
        if (node%step_expr_index /= 2) then
            call test_fail("step_expr_index should be 2")
            return
        end if
        
        if (node%line /= 30 .or. node%column /= 10) then
            call test_fail("line/column should be 30/10")
            return
        end if

        call test_pass()
    end subroutine test_do_loop_factory

    ! do_while_node tests
    subroutine test_do_while_assign()
        type(do_while_node) :: lhs, rhs
        integer, parameter :: body(2) = [20, 21]

        ! Set up rhs
        rhs%line = 30
        rhs%column = 5
        rhs%condition_index = 15
        allocate(rhs%body_indices(2))
        rhs%body_indices = body

        ! Test assignment
        lhs = rhs

        if (lhs%condition_index /= 15) then
            call test_fail("condition_index should be copied")
            return
        end if
        
        if (.not. allocated(lhs%body_indices)) then
            call test_fail("body_indices should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_do_while_assign

    subroutine test_do_while_accept()
        type(do_while_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_do_while_accept

    subroutine test_do_while_to_json()
        type(do_while_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 35
        node%column = 10
        node%condition_index = 5

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_do_while_to_json

    subroutine test_do_while_factory()
        type(do_while_node) :: node
        integer, parameter :: body_indices(1) = [10]

        node = create_do_while(5, body_indices, 40, 15)

        if (node%condition_index /= 5) then
            call test_fail("condition_index should be 5")
            return
        end if
        
        if (node%line /= 40 .or. node%column /= 15) then
            call test_fail("line/column should be 40/15")
            return
        end if

        call test_pass()
    end subroutine test_do_while_factory

    ! forall_node tests
    subroutine test_forall_assign_basic()
        type(forall_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 40
        rhs%column = 1
        rhs%num_indices = 2
        allocate(character(len=5) :: rhs%index_names(2))
        rhs%index_names(1) = "i"
        rhs%index_names(2) = "j"
        allocate(rhs%lower_bound_indices(2))
        rhs%lower_bound_indices = [1, 1]
        allocate(rhs%upper_bound_indices(2))
        rhs%upper_bound_indices = [10, 20]
        rhs%has_mask = .true.
        rhs%mask_expr_index = 5
        rhs%has_dependencies = .false.
        rhs%is_parallel_safe = .true.

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 40 .or. lhs%column /= 1) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%num_indices /= 2) then
            call test_fail("num_indices should be 2")
            return
        end if
        
        if (.not. allocated(lhs%index_names)) then
            call test_fail("index_names should be allocated")
            return
        end if
        
        if (lhs%index_names(1) /= "i") then
            call test_fail("index_names(1) should be 'i'")
            return
        end if
        
        if (.not. lhs%has_mask) then
            call test_fail("has_mask should be true")
            return
        end if
        
        if (lhs%mask_expr_index /= 5) then
            call test_fail("mask_expr_index should be 5")
            return
        end if
        
        if (.not. lhs%is_parallel_safe) then
            call test_fail("is_parallel_safe should be true")
            return
        end if

        call test_pass()
    end subroutine test_forall_assign_basic

    subroutine test_forall_assign_comprehensive()
        type(forall_node) :: lhs, rhs
        integer, parameter :: strides(2) = [2, 3]
        integer, parameter :: dep_pairs(2,1) = reshape([100, 101], [2, 1])

        ! Set up rhs with stride and dependency pairs
        rhs%num_indices = 2
        allocate(rhs%stride_indices(2))
        rhs%stride_indices = strides
        allocate(rhs%dependency_pairs(2, 1))
        rhs%dependency_pairs = dep_pairs

        ! Test assignment
        lhs = rhs

        if (.not. allocated(lhs%stride_indices)) then
            call test_fail("stride_indices should be allocated")
            return
        end if
        
        if (size(lhs%stride_indices) /= 2) then
            call test_fail("stride_indices size should be 2")
            return
        end if
        
        if (lhs%stride_indices(1) /= 2) then
            call test_fail("stride_indices(1) should be 2")
            return
        end if
        
        if (.not. allocated(lhs%dependency_pairs)) then
            call test_fail("dependency_pairs should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_forall_assign_comprehensive

    subroutine test_forall_accept()
        type(forall_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_forall_accept

    subroutine test_forall_to_json()
        type(forall_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 45
        node%column = 5
        node%num_indices = 1
        node%has_mask = .false.
        node%is_parallel_safe = .true.

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_forall_to_json

    ! select_case_node tests
    subroutine test_select_case_assign()
        type(select_case_node) :: lhs, rhs
        integer, parameter :: cases(3) = [10, 11, 12]

        ! Set up rhs
        rhs%line = 50
        rhs%column = 10
        rhs%selector_index = 5
        allocate(rhs%case_indices(3))
        rhs%case_indices = cases
        rhs%default_index = 15

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 50 .or. lhs%column /= 10) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%selector_index /= 5) then
            call test_fail("selector_index should be 5")
            return
        end if
        
        if (.not. allocated(lhs%case_indices)) then
            call test_fail("case_indices should be allocated")
            return
        end if
        
        if (size(lhs%case_indices) /= 3) then
            call test_fail("case_indices size should be 3")
            return
        end if
        
        if (lhs%default_index /= 15) then
            call test_fail("default_index should be 15")
            return
        end if

        call test_pass()
    end subroutine test_select_case_assign

    subroutine test_select_case_accept()
        type(select_case_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_select_case_accept

    subroutine test_select_case_to_json()
        type(select_case_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 55
        node%column = 15
        node%selector_index = 10
        node%default_index = 20

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_select_case_to_json

    subroutine test_select_case_factory()
        type(select_case_node) :: node
        integer, parameter :: case_indices(2) = [5, 6]

        node = create_select_case(10, case_indices, 7, 60, 20)

        if (node%selector_index /= 10) then
            call test_fail("selector_index should be 10")
            return
        end if
        
        if (node%default_index /= 7) then
            call test_fail("default_index should be 7")
            return
        end if
        
        if (node%line /= 60 .or. node%column /= 20) then
            call test_fail("line/column should be 60/20")
            return
        end if

        call test_pass()
    end subroutine test_select_case_factory

    ! case_block_node tests
    subroutine test_case_block_assign()
        type(case_block_node) :: lhs, rhs
        integer, parameter :: values(2) = [100, 200]
        integer, parameter :: body(3) = [10, 11, 12]

        ! Set up rhs
        rhs%line = 60
        rhs%column = 5
        allocate(rhs%value_indices(2))
        rhs%value_indices = values
        allocate(rhs%body_indices(3))
        rhs%body_indices = body

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 60 .or. lhs%column /= 5) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%value_indices)) then
            call test_fail("value_indices should be allocated")
            return
        end if
        
        if (size(lhs%value_indices) /= 2) then
            call test_fail("value_indices size should be 2")
            return
        end if
        
        if (.not. allocated(lhs%body_indices)) then
            call test_fail("body_indices should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_case_block_assign

    subroutine test_case_block_accept()
        type(case_block_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_case_block_accept

    subroutine test_case_block_to_json()
        type(case_block_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 65
        node%column = 10

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_case_block_to_json

    ! case_range_node tests
    subroutine test_case_range_assign()
        type(case_range_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 70
        rhs%column = 15
        rhs%start_value = 1
        rhs%end_value = 10

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 70 .or. lhs%column /= 15) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%start_value /= 1) then
            call test_fail("start_value should be 1")
            return
        end if
        
        if (lhs%end_value /= 10) then
            call test_fail("end_value should be 10")
            return
        end if

        call test_pass()
    end subroutine test_case_range_assign

    subroutine test_case_range_accept()
        type(case_range_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_case_range_accept

    subroutine test_case_range_to_json()
        type(case_range_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 75
        node%column = 20
        node%start_value = 5
        node%end_value = 15

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_case_range_to_json

    ! case_default_node tests
    subroutine test_case_default_assign()
        type(case_default_node) :: lhs, rhs
        integer, parameter :: body(2) = [30, 31]

        ! Set up rhs
        rhs%line = 80
        rhs%column = 5
        allocate(rhs%body_indices(2))
        rhs%body_indices = body

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 80 .or. lhs%column /= 5) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%body_indices)) then
            call test_fail("body_indices should be allocated")
            return
        end if
        
        if (size(lhs%body_indices) /= 2) then
            call test_fail("body_indices size should be 2")
            return
        end if

        call test_pass()
    end subroutine test_case_default_assign

    subroutine test_case_default_accept()
        type(case_default_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_case_default_accept

    subroutine test_case_default_to_json()
        type(case_default_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 85
        node%column = 10

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_case_default_to_json

    ! where_node tests
    subroutine test_where_assign_basic()
        type(where_node) :: lhs, rhs
        integer, parameter :: body(2) = [40, 41]

        ! Set up rhs
        rhs%line = 90
        rhs%column = 1
        rhs%mask_expr_index = 10
        allocate(rhs%where_body_indices(2))
        rhs%where_body_indices = body
        rhs%mask_is_simple = .true.
        rhs%can_vectorize = .true.

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 90 .or. lhs%column /= 1) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%mask_expr_index /= 10) then
            call test_fail("mask_expr_index should be 10")
            return
        end if
        
        if (.not. allocated(lhs%where_body_indices)) then
            call test_fail("where_body_indices should be allocated")
            return
        end if
        
        if (.not. lhs%mask_is_simple) then
            call test_fail("mask_is_simple should be true")
            return
        end if
        
        if (.not. lhs%can_vectorize) then
            call test_fail("can_vectorize should be true")
            return
        end if

        call test_pass()
    end subroutine test_where_assign_basic

    subroutine test_where_assign_with_elsewhere()
        type(where_node) :: lhs, rhs
        type(elsewhere_clause_t) :: clause
        integer, parameter :: body(1) = [50]

        ! Set up elsewhere clause
        clause%mask_index = 20
        allocate(clause%body_indices(1))
        clause%body_indices = body

        ! Set up rhs with elsewhere
        rhs%mask_expr_index = 10
        allocate(rhs%elsewhere_clauses(1))
        rhs%elsewhere_clauses(1) = clause

        ! Test assignment
        lhs = rhs

        if (.not. allocated(lhs%elsewhere_clauses)) then
            call test_fail("elsewhere_clauses should be allocated")
            return
        end if
        
        if (size(lhs%elsewhere_clauses) /= 1) then
            call test_fail("elsewhere_clauses size should be 1")
            return
        end if
        
        if (lhs%elsewhere_clauses(1)%mask_index /= 20) then
            call test_fail("elsewhere clause mask_index should be 20")
            return
        end if

        call test_pass()
    end subroutine test_where_assign_with_elsewhere

    subroutine test_where_accept()
        type(where_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_where_accept

    subroutine test_where_to_json()
        type(where_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 95
        node%column = 5
        node%mask_expr_index = 15
        node%mask_is_simple = .false.
        node%can_vectorize = .true.

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_where_to_json

    ! where_stmt_node tests
    subroutine test_where_stmt_assign()
        type(where_stmt_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 100
        rhs%column = 10
        rhs%mask_expr_index = 25
        rhs%assignment_index = 26

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 100 .or. lhs%column /= 10) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%mask_expr_index /= 25) then
            call test_fail("mask_expr_index should be 25")
            return
        end if
        
        if (lhs%assignment_index /= 26) then
            call test_fail("assignment_index should be 26")
            return
        end if

        call test_pass()
    end subroutine test_where_stmt_assign

    subroutine test_where_stmt_accept()
        type(where_stmt_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_where_stmt_accept

    subroutine test_where_stmt_to_json()
        type(where_stmt_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 105
        node%column = 15
        node%mask_expr_index = 30
        node%assignment_index = 31

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_where_stmt_to_json

    ! cycle_node tests
    subroutine test_cycle_assign()
        type(cycle_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 110
        rhs%column = 5
        rhs%label = "outer_loop"

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 110 .or. lhs%column /= 5) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%label)) then
            call test_fail("label should be allocated")
            return
        end if
        
        if (lhs%label /= "outer_loop") then
            call test_fail("label should be 'outer_loop'")
            return
        end if

        call test_pass()
    end subroutine test_cycle_assign

    subroutine test_cycle_accept()
        type(cycle_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_cycle_accept

    subroutine test_cycle_to_json()
        type(cycle_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 115
        node%column = 10
        node%label = "my_loop"

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_cycle_to_json

    ! exit_node tests
    subroutine test_exit_assign()
        type(exit_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 120
        rhs%column = 15
        rhs%label = "inner_loop"

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 120 .or. lhs%column /= 15) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%label)) then
            call test_fail("label should be allocated")
            return
        end if
        
        if (lhs%label /= "inner_loop") then
            call test_fail("label should be 'inner_loop'")
            return
        end if

        call test_pass()
    end subroutine test_exit_assign

    subroutine test_exit_accept()
        type(exit_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_exit_accept

    subroutine test_exit_to_json()
        type(exit_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 125
        node%column = 20

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_exit_to_json

    ! stop_node tests
    subroutine test_stop_assign()
        type(stop_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 130
        rhs%column = 1
        rhs%stop_code_index = 99
        rhs%stop_message = "Error occurred"

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 130 .or. lhs%column /= 1) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (lhs%stop_code_index /= 99) then
            call test_fail("stop_code_index should be 99")
            return
        end if
        
        if (.not. allocated(lhs%stop_message)) then
            call test_fail("stop_message should be allocated")
            return
        end if
        
        if (lhs%stop_message /= "Error occurred") then
            call test_fail("stop_message should be 'Error occurred'")
            return
        end if

        call test_pass()
    end subroutine test_stop_assign

    subroutine test_stop_accept()
        type(stop_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_stop_accept

    subroutine test_stop_to_json()
        type(stop_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 135
        node%column = 5
        node%stop_code_index = 1
        node%stop_message = "Fatal error"

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_stop_to_json

    ! return_node tests
    subroutine test_return_assign()
        type(return_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 140
        rhs%column = 10

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 140 .or. lhs%column /= 10) then
            call test_fail("line/column should be copied")
            return
        end if

        call test_pass()
    end subroutine test_return_assign

    subroutine test_return_accept()
        type(return_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_return_accept

    subroutine test_return_to_json()
        type(return_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 145
        node%column = 15

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_return_to_json

    ! associate_node tests
    subroutine test_associate_assign()
        type(associate_node) :: lhs, rhs
        type(association_t) :: assoc
        integer, parameter :: body(2) = [60, 61]

        ! Set up association
        assoc%name = "x"
        assoc%expr_index = 50

        ! Set up rhs
        rhs%line = 150
        rhs%column = 1
        allocate(rhs%associations(1))
        rhs%associations(1) = assoc
        allocate(rhs%body_indices(2))
        rhs%body_indices = body

        ! Test assignment
        lhs = rhs

        if (lhs%line /= 150 .or. lhs%column /= 1) then
            call test_fail("line/column should be copied")
            return
        end if
        
        if (.not. allocated(lhs%associations)) then
            call test_fail("associations should be allocated")
            return
        end if
        
        if (size(lhs%associations) /= 1) then
            call test_fail("associations size should be 1")
            return
        end if
        
        if (lhs%associations(1)%name /= "x") then
            call test_fail("association name should be 'x'")
            return
        end if
        
        if (.not. allocated(lhs%body_indices)) then
            call test_fail("body_indices should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_associate_assign

    subroutine test_associate_accept()
        type(associate_node) :: node
        integer :: dummy_visitor

        call node%accept(dummy_visitor)
        call test_pass()
    end subroutine test_associate_accept

    subroutine test_associate_to_json()
        type(associate_node) :: node
        type(association_t) :: assoc
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up association
        assoc%name = "y"
        assoc%expr_index = 70

        ! Set up node
        node%line = 155
        node%column = 5
        allocate(node%associations(1))
        node%associations(1) = assoc

        ! Test to_json
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_associate_to_json

    subroutine test_associate_factory()
        type(associate_node) :: node
        type(association_t) :: associations(2)
        integer, parameter :: body_indices(1) = [80]

        ! Set up associations
        associations(1)%name = "a"
        associations(1)%expr_index = 90
        associations(2)%name = "b"
        associations(2)%expr_index = 91

        node = create_associate(associations, body_indices, 160, 10)

        if (.not. allocated(node%associations)) then
            call test_fail("associations should be allocated")
            return
        end if
        
        if (size(node%associations) /= 2) then
            call test_fail("associations size should be 2")
            return
        end if
        
        if (node%line /= 160 .or. node%column /= 10) then
            call test_fail("line/column should be 160/10")
            return
        end if

        call test_pass()
    end subroutine test_associate_factory

    ! if factory function test
    subroutine test_if_factory()
        type(if_node) :: node
        type(elseif_wrapper) :: elseif_blocks(1)
        integer, parameter :: then_body(2) = [10, 11]
        integer, parameter :: else_body(1) = [12]

        ! Set up elseif block
        elseif_blocks(1)%condition_index = 20
        allocate(elseif_blocks(1)%body_indices(1))
        elseif_blocks(1)%body_indices = [21]

        node = create_if(5, then_body, elseif_blocks, else_body, 165, 15)

        if (node%condition_index /= 5) then
            call test_fail("condition_index should be 5")
            return
        end if
        
        if (.not. allocated(node%elseif_blocks)) then
            call test_fail("elseif_blocks should be allocated")
            return
        end if
        
        if (node%line /= 165 .or. node%column /= 15) then
            call test_fail("line/column should be 165/15")
            return
        end if

        call test_pass()
    end subroutine test_if_factory

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
            print *, "All AST nodes control coverage tests passed!"
        else
            print *, "Some tests failed!"
            stop 1
        end if
    end subroutine print_results

end program test_ast_nodes_control_coverage