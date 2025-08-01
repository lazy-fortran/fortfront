program test_array_bounds
    use ast_core
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed = .true.

    call test_basic_array_bounds_creation()
    call test_range_expression_creation()

    if (all_tests_passed) then
        print *, "All array bounds tests passed!"
    else
        print *, "Some array bounds tests failed!"
        stop 1
    end if

contains

    subroutine test_basic_array_bounds_creation()
        type(array_bounds_node) :: bounds
        type(array_slice_node) :: slice
        integer :: bounds_indices(1)
        
        print *, "Testing basic array bounds node creation..."
        
        ! Create basic bounds node
        bounds = create_array_bounds(1, 10)
        
        if (bounds%lower_bound_index /= 1 .or. bounds%upper_bound_index /= 10) then
            print *, "FAIL: array_bounds_node creation"
            all_tests_passed = .false.
        else
            print *, "PASS: array_bounds_node creation"
        end if
        
        ! Create array slice node
        bounds_indices(1) = 1
        slice = create_array_slice(1, bounds_indices, 1)
        
        if (slice%array_index /= 1 .or. slice%num_dimensions /= 1) then
            print *, "FAIL: array_slice_node creation"
            all_tests_passed = .false.
        else
            print *, "PASS: array_slice_node creation"
        end if
    end subroutine

    subroutine test_range_expression_creation()
        type(range_expression_node) :: range
        
        print *, "Testing range expression creation..."
        
        ! Create range 1:10
        range = create_range_expression(1, 10)
        
        if (range%start_index /= 1 .or. range%end_index /= 10) then
            print *, "FAIL: range_expression_node creation"
            all_tests_passed = .false.
        else
            print *, "PASS: range_expression_node creation"
        end if
        
        ! Create range with stride 1:10:2
        range = create_range_expression(1, 10, 2)
        
        if (range%start_index /= 1 .or. range%end_index /= 10 .or. range%stride_index /= 2) then
            print *, "FAIL: range_expression_node with stride creation"
            all_tests_passed = .false.
        else
            print *, "PASS: range_expression_node with stride creation"
        end if
    end subroutine

end program test_array_bounds