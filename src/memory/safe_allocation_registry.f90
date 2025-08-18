module safe_allocation_registry
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private
    
    public :: safe_allocate_and_copy, error_placeholder_t
    
    ! Error placeholder for unknown types - safer than source= allocation
    type, public :: error_placeholder_t
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: original_type
        integer :: error_code = -1
    contains
        procedure :: cleanup => error_placeholder_cleanup
    end type
    
contains

    ! Safe allocation that always uses proper assignment operators
    subroutine safe_allocate_and_copy(target, source, success)
        class(*), allocatable, intent(out) :: target
        class(*), intent(in) :: source
        logical, intent(out) :: success
        
        success = .true.
        
        ! Handle all known types with proper assignment operators
        select type(source)
        type is (semantic_context_t)
            allocate(semantic_context_t :: target)
            select type(target)
            type is (semantic_context_t)
                target = source  ! Use proper assignment operator for deep copy
            end select
        type is (integer)
            allocate(integer :: target)
            select type(target)
            type is (integer)
                target = source
            end select
        type is (logical)
            allocate(logical :: target)  
            select type(target)
            type is (logical)
                target = source
            end select
        type is (real)
            allocate(real :: target)
            select type(target)
            type is (real)
                target = source
            end select
        type is (character(*))
            allocate(character(len=len(source)) :: target)
            select type(target)
            type is (character(*))
                target = source
            end select
        class default
            ! Safe error handling instead of unsafe source= allocation
            call create_error_placeholder(target, "Unknown type in safe_allocate_and_copy")
            success = .false.
        end select
    end subroutine

    ! Create error placeholder instead of unsafe allocation
    subroutine create_error_placeholder(target, message, type_info)
        class(*), allocatable, intent(out) :: target
        character(*), intent(in) :: message
        character(*), intent(in), optional :: type_info
        
        allocate(error_placeholder_t :: target)
        select type(target)
        type is (error_placeholder_t)
            target%error_message = message
            if (present(type_info)) then
                target%original_type = type_info
            else
                target%original_type = "unknown"
            end if
            target%error_code = -1
        end select
    end subroutine

    ! Cleanup for error placeholder
    subroutine error_placeholder_cleanup(this)
        class(error_placeholder_t), intent(inout) :: this
        if (allocated(this%error_message)) deallocate(this%error_message)
        if (allocated(this%original_type)) deallocate(this%original_type)
        this%error_code = 0
    end subroutine

end module safe_allocation_registry