program test_issue_1623_select_type
    ! Test SELECT TYPE construct support
    implicit none

    type :: base_type
        integer :: id
    end type base_type

    type, extends(base_type) :: derived_type
        real :: value
    end type derived_type

    type, extends(base_type) :: other_derived_type
        character(len=20) :: name
    end type other_derived_type

    class(base_type), allocatable :: obj
    integer :: result_flag

    ! Test 1: TYPE IS with integer
    block
        class(*), allocatable :: generic_obj
        allocate (integer :: generic_obj)
        select type (generic_obj)
            type is (integer)
            result_flag = 1
            type is (real)
            result_flag = 2
        class default
            result_flag = 0
        end select
        if (result_flag /= 1) error stop "Test 1 failed: integer type not matched"
        deallocate (generic_obj)
    end block

    ! Test 2: TYPE IS with real
    block
        class(*), allocatable :: generic_obj
        allocate (real :: generic_obj)
        select type (generic_obj)
            type is (integer)
            result_flag = 1
            type is (real)
            result_flag = 2
        class default
            result_flag = 0
        end select
        if (result_flag /= 2) error stop "Test 2 failed: real type not matched"
        deallocate (generic_obj)
    end block

    ! Test 3: CLASS IS with derived type
    allocate (derived_type :: obj)
    select type (obj)
    class is (derived_type)
        result_flag = 3
    class is (other_derived_type)
        result_flag = 4
    class default
        result_flag = 0
    end select
    if (result_flag /= 3) error stop "Test 3 failed: derived_type not matched"
    deallocate (obj)

    ! Test 4: CLASS DEFAULT fallback
    allocate (base_type :: obj)
    select type (obj)
    class is (derived_type)
        result_flag = 3
    class is (other_derived_type)
        result_flag = 4
    class default
        result_flag = 5
    end select
    if (result_flag /= 5) error stop "Test 4 failed: class default not matched"
    deallocate (obj)

    ! Test 5: Nested SELECT TYPE
    block
        class(*), allocatable :: outer_obj
        allocate (integer :: outer_obj)
        select type (outer_obj)
            type is (integer)
            block
                class(*), allocatable :: inner_obj
                allocate (real :: inner_obj)
                select type (inner_obj)
                    type is (real)
                    result_flag = 6
                class default
                    result_flag = 0
                end select
                deallocate (inner_obj)
            end block
        class default
            result_flag = 0
        end select
        if (result_flag /= 6) error stop "Test 5 failed: nested select type"
        deallocate (outer_obj)
    end block

    print *, "All SELECT TYPE tests passed!"

end program test_issue_1623_select_type
