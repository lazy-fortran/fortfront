module parameter_tracker
    implicit none
    private

    ! Parameter info type
    type, public :: parameter_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent  ! "in", "out", "inout", or ""
        logical :: is_optional = .false.
    contains
        procedure :: assign => parameter_info_assign
        generic :: assignment(=) => assign
    end type parameter_info_t

    ! Parameter tracker for current scope
    type, public :: parameter_tracker_t
        type(parameter_info_t), allocatable :: params(:)
        integer :: count = 0
    contains
        procedure :: add_parameter
        procedure :: get_parameter_intent
        procedure :: is_parameter
        procedure :: clear
        procedure :: assign => parameter_tracker_assign
        generic :: assignment(=) => assign
    end type parameter_tracker_t

contains

    ! Add a parameter to the tracker
    subroutine add_parameter(this, name, intent, is_optional)
        class(parameter_tracker_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: intent
        logical, intent(in), optional :: is_optional
        
        type(parameter_info_t), allocatable :: temp(:)
        integer :: new_size
        
        ! Resize array if needed
        if (.not. allocated(this%params)) then
            allocate(this%params(10))
        else if (this%count >= size(this%params)) then
            new_size = size(this%params) * 2
            allocate(temp(new_size))
            temp(1:this%count) = this%params(1:this%count)
            call move_alloc(temp, this%params)
        end if
        
        ! Add parameter
        this%count = this%count + 1
        this%params(this%count)%name = name
        
        if (present(intent)) then
            this%params(this%count)%intent = intent
        else
            this%params(this%count)%intent = ""
        end if
        
        if (present(is_optional)) then
            this%params(this%count)%is_optional = is_optional
        else
            this%params(this%count)%is_optional = .false.
        end if
    end subroutine add_parameter
    
    ! Get intent attribute for a parameter
    function get_parameter_intent(this, name) result(intent)
        class(parameter_tracker_t), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: intent
        integer :: i
        
        intent = ""
        
        if (.not. allocated(this%params)) return
        
        do i = 1, this%count
            if (this%params(i)%name == name) then
                intent = this%params(i)%intent
                return
            end if
        end do
    end function get_parameter_intent
    
    ! Check if a variable is a parameter
    function is_parameter(this, name) result(is_param)
        class(parameter_tracker_t), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: is_param
        integer :: i
        
        is_param = .false.
        
        if (.not. allocated(this%params)) return
        
        do i = 1, this%count
            if (this%params(i)%name == name) then
                is_param = .true.
                return
            end if
        end do
    end function is_parameter
    
    ! Clear all parameters
    subroutine clear(this)
        class(parameter_tracker_t), intent(inout) :: this
        
        if (allocated(this%params)) then
            deallocate(this%params)
        end if
        this%count = 0
    end subroutine clear

    ! Assignment operator for parameter_tracker_t (deep copy)
    subroutine parameter_tracker_assign(lhs, rhs)
        class(parameter_tracker_t), intent(out) :: lhs
        type(parameter_tracker_t), intent(in) :: rhs
        integer :: i

        lhs%count = rhs%count

        if (allocated(rhs%params)) then
            allocate(lhs%params(size(rhs%params)))
            do i = 1, rhs%count
                lhs%params(i)%name = rhs%params(i)%name
                lhs%params(i)%intent = rhs%params(i)%intent
                lhs%params(i)%is_optional = rhs%params(i)%is_optional
            end do
        end if
    end subroutine parameter_tracker_assign

    ! Assignment operator for parameter_info_t (deep copy)
    subroutine parameter_info_assign(lhs, rhs)
        class(parameter_info_t), intent(out) :: lhs
        type(parameter_info_t), intent(in) :: rhs

        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        end if
        if (allocated(rhs%intent)) then
            lhs%intent = rhs%intent
        end if
        lhs%is_optional = rhs%is_optional
    end subroutine parameter_info_assign

end module parameter_tracker