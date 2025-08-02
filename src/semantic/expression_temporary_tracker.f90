module expression_temporary_tracker_module
    use iso_fortran_env, only: error_unit
    implicit none
    private

    ! Public interface
    public :: temp_tracker_t, create_temp_tracker, temp_info_t
    public :: allocate_temporary, release_temporary, get_temporary_info
    public :: get_temporaries_for_expression, mark_expression_temporaries

    ! Type to store information about a temporary variable
    type :: temp_info_t
        integer :: temp_id
        character(len=:), allocatable :: type_info
        integer :: size_in_bytes
        integer :: created_at_node    ! AST node where created
        integer :: released_at_node   ! AST node where released (-1 if active)
        logical :: is_active
        logical :: is_reusable
    end type temp_info_t

    ! Main temporary tracker type
    type :: temp_tracker_t
        type(temp_info_t), allocatable :: temporaries(:)
        integer :: next_temp_id = 0
        integer :: active_count = 0
        integer :: total_allocated = 0
        integer, allocatable :: expr_to_temps(:,:)  ! Maps expression nodes to temp IDs
        integer :: max_expressions = 1000
    contains
        procedure :: allocate_temp => tracker_allocate_temporary
        procedure :: release_temp => tracker_release_temporary
        procedure :: get_temp_info => tracker_get_temporary_info
        procedure :: get_temps_for_expr => tracker_get_temporaries_for_expression
        procedure :: mark_expr_temps => tracker_mark_expression_temporaries
        procedure :: get_active_count => tracker_get_active_count
        procedure :: get_total_count => tracker_get_total_count
        procedure :: find_reusable_temp => tracker_find_reusable_temporary
        procedure :: deep_copy => temp_tracker_deep_copy
        procedure :: assign => temp_tracker_assign
        generic :: assignment(=) => assign
    end type temp_tracker_t

contains

    ! Create a new temporary tracker
    function create_temp_tracker() result(tracker)
        type(temp_tracker_t) :: tracker
        allocate(tracker%temporaries(0))
        allocate(tracker%expr_to_temps(tracker%max_expressions, 10))
        tracker%expr_to_temps = -1
        tracker%next_temp_id = 0
        tracker%active_count = 0
        tracker%total_allocated = 0
    end function create_temp_tracker

    ! Allocate a new temporary variable
    function allocate_temporary(tracker, type_info, size_bytes, created_at_node) &
             result(temp_id)
        type(temp_tracker_t), intent(inout) :: tracker
        character(len=*), intent(in) :: type_info
        integer, intent(in) :: size_bytes
        integer, intent(in) :: created_at_node
        integer :: temp_id

        type(temp_info_t) :: new_temp
        type(temp_info_t), allocatable :: temp_array(:)
        integer :: i, reusable_id

        ! First check if we can reuse an existing temporary
        reusable_id = tracker%find_reusable_temp(type_info, size_bytes)
        if (reusable_id > 0) then
            temp_id = reusable_id
            ! Mark as active again
            do i = 1, size(tracker%temporaries)
                if (tracker%temporaries(i)%temp_id == temp_id) then
                    tracker%temporaries(i)%is_active = .true.
                    tracker%temporaries(i)%created_at_node = created_at_node
                    tracker%temporaries(i)%released_at_node = -1
                    exit
                end if
            end do
            tracker%active_count = tracker%active_count + 1
            return
        end if

        ! Create new temporary
        tracker%next_temp_id = tracker%next_temp_id + 1
        temp_id = tracker%next_temp_id

        new_temp%temp_id = temp_id
        new_temp%type_info = type_info
        new_temp%size_in_bytes = size_bytes
        new_temp%created_at_node = created_at_node
        new_temp%released_at_node = -1
        new_temp%is_active = .true.
        new_temp%is_reusable = .true.

        ! Expand temporaries array
        allocate(temp_array(size(tracker%temporaries) + 1))
        if (size(tracker%temporaries) > 0) then
            temp_array(1:size(tracker%temporaries)) = tracker%temporaries
        end if
        temp_array(size(temp_array)) = new_temp
        call move_alloc(temp_array, tracker%temporaries)

        tracker%active_count = tracker%active_count + 1
        tracker%total_allocated = tracker%total_allocated + 1

    end function allocate_temporary

    ! Release a temporary variable
    subroutine release_temporary(tracker, temp_id, released_at_node)
        type(temp_tracker_t), intent(inout) :: tracker
        integer, intent(in) :: temp_id
        integer, intent(in) :: released_at_node

        integer :: i

        do i = 1, size(tracker%temporaries)
            if (tracker%temporaries(i)%temp_id == temp_id .and. &
                tracker%temporaries(i)%is_active) then
                tracker%temporaries(i)%is_active = .false.
                tracker%temporaries(i)%released_at_node = released_at_node
                tracker%active_count = tracker%active_count - 1
                exit
            end if
        end do

    end subroutine release_temporary

    ! Get information about a temporary
    function get_temporary_info(tracker, temp_id) result(info)
        type(temp_tracker_t), intent(in) :: tracker
        integer, intent(in) :: temp_id
        type(temp_info_t) :: info

        integer :: i

        ! Initialize with invalid values
        info%temp_id = -1
        info%is_active = .false.

        do i = 1, size(tracker%temporaries)
            if (tracker%temporaries(i)%temp_id == temp_id) then
                info = tracker%temporaries(i)
                exit
            end if
        end do

    end function get_temporary_info

    ! Get list of temporaries for an expression node
    function get_temporaries_for_expression(tracker, expr_node_id) result(temp_ids)
        type(temp_tracker_t), intent(in) :: tracker
        integer, intent(in) :: expr_node_id
        integer, allocatable :: temp_ids(:)

        integer :: i, count, j

        ! Count valid entries
        count = 0
        if (expr_node_id > 0 .and. expr_node_id <= tracker%max_expressions) then
            do i = 1, 10
                if (tracker%expr_to_temps(expr_node_id, i) /= -1) then
                    count = count + 1
                else
                    exit
                end if
            end do
        end if

        ! Allocate and fill result
        allocate(temp_ids(count))
        if (count > 0) then
            do i = 1, count
                temp_ids(i) = tracker%expr_to_temps(expr_node_id, i)
            end do
        end if

    end function get_temporaries_for_expression

    ! Mark temporaries used by an expression
    subroutine mark_expression_temporaries(tracker, expr_node_id, temp_ids)
        type(temp_tracker_t), intent(inout) :: tracker
        integer, intent(in) :: expr_node_id
        integer, intent(in) :: temp_ids(:)

        integer :: i

        if (expr_node_id > 0 .and. expr_node_id <= tracker%max_expressions) then
            ! Clear existing entries
            tracker%expr_to_temps(expr_node_id, :) = -1

            ! Add new entries
            do i = 1, min(size(temp_ids), 10)
                tracker%expr_to_temps(expr_node_id, i) = temp_ids(i)
            end do
        end if

    end subroutine mark_expression_temporaries

    ! Tracker methods
    function tracker_allocate_temporary(this, type_info, size_bytes, &
                                      created_at_node) result(temp_id)
        class(temp_tracker_t), intent(inout) :: this
        character(len=*), intent(in) :: type_info
        integer, intent(in) :: size_bytes
        integer, intent(in) :: created_at_node
        integer :: temp_id

        temp_id = allocate_temporary(this, type_info, size_bytes, created_at_node)
    end function tracker_allocate_temporary

    subroutine tracker_release_temporary(this, temp_id, released_at_node)
        class(temp_tracker_t), intent(inout) :: this
        integer, intent(in) :: temp_id
        integer, intent(in) :: released_at_node

        call release_temporary(this, temp_id, released_at_node)
    end subroutine tracker_release_temporary

    function tracker_get_temporary_info(this, temp_id) result(info)
        class(temp_tracker_t), intent(in) :: this
        integer, intent(in) :: temp_id
        type(temp_info_t) :: info

        info = get_temporary_info(this, temp_id)
    end function tracker_get_temporary_info

    function tracker_get_temporaries_for_expression(this, expr_node_id) &
             result(temp_ids)
        class(temp_tracker_t), intent(in) :: this
        integer, intent(in) :: expr_node_id
        integer, allocatable :: temp_ids(:)

        temp_ids = get_temporaries_for_expression(this, expr_node_id)
    end function tracker_get_temporaries_for_expression

    subroutine tracker_mark_expression_temporaries(this, expr_node_id, temp_ids)
        class(temp_tracker_t), intent(inout) :: this
        integer, intent(in) :: expr_node_id
        integer, intent(in) :: temp_ids(:)

        call mark_expression_temporaries(this, expr_node_id, temp_ids)
    end subroutine tracker_mark_expression_temporaries

    function tracker_get_active_count(this) result(count)
        class(temp_tracker_t), intent(in) :: this
        integer :: count
        count = this%active_count
    end function tracker_get_active_count

    function tracker_get_total_count(this) result(count)
        class(temp_tracker_t), intent(in) :: this
        integer :: count
        count = this%total_allocated
    end function tracker_get_total_count

    function tracker_find_reusable_temporary(this, type_info, size_bytes) &
             result(temp_id)
        class(temp_tracker_t), intent(in) :: this
        character(len=*), intent(in) :: type_info
        integer, intent(in) :: size_bytes
        integer :: temp_id

        integer :: i

        temp_id = -1

        ! Look for inactive temporary with matching type and size
        do i = 1, size(this%temporaries)
            if (.not. this%temporaries(i)%is_active .and. &
                this%temporaries(i)%is_reusable) then
                ! Check type_info is allocated before comparison
                if (allocated(this%temporaries(i)%type_info)) then
                    if (this%temporaries(i)%type_info == type_info .and. &
                        this%temporaries(i)%size_in_bytes == size_bytes) then
                        temp_id = this%temporaries(i)%temp_id
                        exit
                    end if
                end if
            end if
        end do

    end function tracker_find_reusable_temporary

    ! Deep copy for temp_tracker_t
    subroutine temp_tracker_deep_copy(dst, src)
        class(temp_tracker_t), intent(out) :: dst
        class(temp_tracker_t), intent(in) :: src

        integer :: i

        dst%next_temp_id = src%next_temp_id
        dst%active_count = src%active_count
        dst%total_allocated = src%total_allocated
        dst%max_expressions = src%max_expressions

        ! Deep copy temporaries array
        if (allocated(src%temporaries)) then
            allocate(dst%temporaries(size(src%temporaries)))
            do i = 1, size(src%temporaries)
                dst%temporaries(i)%temp_id = src%temporaries(i)%temp_id
                if (allocated(src%temporaries(i)%type_info)) then
                    dst%temporaries(i)%type_info = src%temporaries(i)%type_info
                end if
                dst%temporaries(i)%size_in_bytes = src%temporaries(i)%size_in_bytes
                dst%temporaries(i)%created_at_node = src%temporaries(i)%created_at_node
                dst%temporaries(i)%released_at_node = &
                    src%temporaries(i)%released_at_node
                dst%temporaries(i)%is_active = src%temporaries(i)%is_active
                dst%temporaries(i)%is_reusable = src%temporaries(i)%is_reusable
            end do
        end if

        ! Deep copy expr_to_temps array
        if (allocated(src%expr_to_temps)) then
            allocate(dst%expr_to_temps(size(src%expr_to_temps, 1), &
                                       size(src%expr_to_temps, 2)))
            dst%expr_to_temps = src%expr_to_temps
        end if

    end subroutine temp_tracker_deep_copy

    ! Assignment operator
    subroutine temp_tracker_assign(dst, src)
        class(temp_tracker_t), intent(out) :: dst
        class(temp_tracker_t), intent(in) :: src
        call temp_tracker_deep_copy(dst, src)
    end subroutine temp_tracker_assign

end module expression_temporary_tracker_module