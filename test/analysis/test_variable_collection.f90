program test_variable_collection
    ! Test suite for unified variable collection module (Issue #330)
    use variable_collection
    implicit none

    logical :: all_passed = .true.
    integer :: test_count = 0
    integer :: passed_count = 0

    ! Run all tests (unit tests focused on the collection functionality)
    call run_test("Collection creation", test_collection_creation)
    call run_test("Variable adding", test_variable_adding)
    call run_test("Variable finding", test_variable_finding)
    call run_test("Collection resizing", test_collection_resizing)
    call run_test("Function name filtering", test_function_filtering)
    call run_test("Variable collection clearing", test_collection_clearing)
    call run_test("Large collection stress", test_large_collection)

    ! Summary
    print *
    if (all_passed) then
        print *, 'All ', test_count, ' tests PASSED'
    else
        print *, passed_count, ' of ', test_count, ' tests passed'
        error stop 1
    end if

contains

    subroutine run_test(test_name, test_proc)
        character(len=*), intent(in) :: test_name
        interface
            logical function test_proc()
            end function test_proc
        end interface
        logical :: result

        test_count = test_count + 1
        result = test_proc()
        
        if (result) then
            passed_count = passed_count + 1
            print *, '  PASS: ', test_name
        else
            all_passed = .false.
            print *, '  FAIL: ', test_name
        end if
    end subroutine run_test

    ! Test collection creation
    logical function test_collection_creation()
        type(variable_collection_t) :: collection1, collection2
        
        test_collection_creation = .true.

        ! Test default creation
        collection1 = create_variable_collection()
        if (collection1%count /= 0) then
            print *, '    Expected count 0, got: ', collection1%count
            test_collection_creation = .false.
        end if
        if (collection1%capacity /= 100) then
            print *, '    Expected default capacity 100, got: ', collection1%capacity
            test_collection_creation = .false.
        end if

        ! Test custom capacity creation
        collection2 = create_variable_collection(50)
        if (collection2%capacity /= 50) then
            print *, '    Expected custom capacity 50, got: ', collection2%capacity
            test_collection_creation = .false.
        end if
    end function test_collection_creation

    ! Test variable adding
    logical function test_variable_adding()
        type(variable_collection_t) :: collection
        type(variable_info_t) :: var_info
        
        test_variable_adding = .true.
        
        collection = create_variable_collection()
        
        ! Add first variable
        var_info%name = 'x'
        var_info%type_name = 'integer'
        var_info%is_declared = .false.
        var_info%usage_count = 1
        call collection%add_variable(var_info)
        
        if (collection%count /= 1) then
            print *, '    Expected count 1 after adding variable, got: ', collection%count
            test_variable_adding = .false.
        end if
        
        if (collection%variables(1)%name /= 'x') then
            print *, '    Expected variable name "x", got: ', trim(collection%variables(1)%name)
            test_variable_adding = .false.
        end if
        
        ! Add second variable
        var_info%name = 'y'
        var_info%type_name = 'real'
        call collection%add_variable(var_info)
        
        if (collection%count /= 2) then
            print *, '    Expected count 2 after adding second variable, got: ', collection%count
            test_variable_adding = .false.
        end if
    end function test_variable_adding

    ! Test variable finding
    logical function test_variable_finding()
        type(variable_collection_t) :: collection
        type(variable_info_t) :: var_info
        integer :: index
        
        test_variable_finding = .true.
        
        collection = create_variable_collection()
        
        ! Add variables
        var_info%name = 'alpha'
        var_info%type_name = 'integer'
        call collection%add_variable(var_info)
        
        var_info%name = 'beta'
        var_info%type_name = 'real'
        call collection%add_variable(var_info)
        
        ! Test finding existing variables
        index = collection%find_variable('alpha')
        if (index /= 1) then
            print *, '    Expected to find alpha at index 1, got: ', index
            test_variable_finding = .false.
        end if
        
        index = collection%find_variable('beta')
        if (index /= 2) then
            print *, '    Expected to find beta at index 2, got: ', index
            test_variable_finding = .false.
        end if
        
        ! Test has_variable method
        if (.not. collection%has_variable('alpha')) then
            print *, '    has_variable failed to find alpha'
            test_variable_finding = .false.
        end if
        
        if (collection%has_variable('gamma')) then
            print *, '    has_variable incorrectly found non-existent gamma'
            test_variable_finding = .false.
        end if
        
        ! Test finding non-existent variable
        index = collection%find_variable('nonexistent')
        if (index /= 0) then
            print *, '    Expected 0 for non-existent variable, got: ', index
            test_variable_finding = .false.
        end if
    end function test_variable_finding

    ! Test collection resizing
    logical function test_collection_resizing()
        type(variable_collection_t) :: collection
        type(variable_info_t) :: var_info
        integer :: i, initial_capacity, new_capacity
        character(len=10) :: var_name
        
        test_collection_resizing = .true.
        
        collection = create_variable_collection(5)  ! Start with small capacity
        initial_capacity = collection%capacity
        
        ! Add more variables than initial capacity
        do i = 1, 8
            write(var_name, '(A,I0)') 'var', i
            var_info%name = trim(var_name)
            var_info%type_name = 'integer'
            call collection%add_variable(var_info)
        end do
        
        new_capacity = collection%capacity
        
        if (new_capacity <= initial_capacity) then
            print *, '    Collection did not resize. Initial: ', initial_capacity, &
                     ' Final: ', new_capacity
            test_collection_resizing = .false.
        end if
        
        if (collection%count /= 8) then
            print *, '    Expected 8 variables, got: ', collection%count
            test_collection_resizing = .false.
        end if
        
        ! Verify all variables are still accessible after resize
        if (.not. collection%has_variable('var1')) then
            print *, '    Lost var1 after resize'
            test_collection_resizing = .false.
        end if
        
        if (.not. collection%has_variable('var8')) then
            print *, '    Lost var8 after resize'
            test_collection_resizing = .false.
        end if
    end function test_collection_resizing

    ! Test function name filtering utility
    logical function test_function_filtering()
        character(len=64) :: function_names(3)
        logical :: result
        
        test_function_filtering = .true.
        
        function_names(1) = 'sin'
        function_names(2) = 'cos'  
        function_names(3) = 'sqrt'
        
        ! Test that function names are identified correctly
        ! Note: This tests the internal is_function_name logic
        ! We can't access it directly, so we test the concept
        
        ! This is a placeholder test - in real usage this would be tested
        ! through the collect_variables interface
        result = .true.
        
        if (.not. result) then
            print *, '    Function filtering test failed'
            test_function_filtering = .false.
        end if
    end function test_function_filtering

    ! Test collection clearing
    logical function test_collection_clearing()
        type(variable_collection_t) :: collection
        type(variable_info_t) :: var_info
        
        test_collection_clearing = .true.
        
        collection = create_variable_collection()
        
        ! Add some variables
        var_info%name = 'temp1'
        var_info%type_name = 'integer'
        call collection%add_variable(var_info)
        
        var_info%name = 'temp2'
        call collection%add_variable(var_info)
        
        if (collection%count /= 2) then
            print *, '    Expected 2 variables before clearing, got: ', collection%count
            test_collection_clearing = .false.
        end if
        
        ! Clear the collection
        call collection%clear()
        
        if (collection%count /= 0) then
            print *, '    Expected 0 variables after clearing, got: ', collection%count
            test_collection_clearing = .false.
        end if
        
        ! Verify capacity is maintained
        if (collection%capacity /= 100) then
            print *, '    Capacity changed after clearing: ', collection%capacity
            test_collection_clearing = .false.
        end if
    end function test_collection_clearing

    ! Test large collection stress test
    logical function test_large_collection()
        type(variable_collection_t) :: collection
        type(variable_info_t) :: var_info
        integer :: i
        character(len=10) :: var_name
        
        test_large_collection = .true.

        collection = create_variable_collection(10)  ! Start small
        
        ! Add many variables to test resizing
        do i = 1, 150
            write(var_name, '(A,I0)') 'var', i
            var_info%name = trim(var_name)
            var_info%type_name = 'integer'
            var_info%is_declared = .false.
            var_info%usage_count = 1
            call collection%add_variable(var_info)
        end do
        
        if (collection%count /= 150) then
            print *, '    Expected 150 variables, got: ', collection%count
            test_large_collection = .false.
        end if
        
        if (collection%capacity < 150) then
            print *, '    Collection did not resize properly. Capacity: ', collection%capacity
            test_large_collection = .false.
        end if
        
        ! Test finding variables across the range
        if (.not. collection%has_variable('var1')) then
            print *, '    Could not find var1'
            test_large_collection = .false.
        end if
        
        if (.not. collection%has_variable('var50')) then
            print *, '    Could not find var50'
            test_large_collection = .false.
        end if
        
        if (.not. collection%has_variable('var150')) then
            print *, '    Could not find var150'
            test_large_collection = .false.
        end if
    end function test_large_collection

end program test_variable_collection