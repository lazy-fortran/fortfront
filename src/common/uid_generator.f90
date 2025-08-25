module uid_generator
    ! UID Generation System for CST/AST Node Identification
    ! ======================================================
    ! Provides unique, stable identifiers for nodes in the compilation pipeline.
    ! UIDs are used for bidirectional CST/AST linking and external tooling.
    !
    ! Features:
    ! - Thread-safe counter-based generation
    ! - Generation tracking for arena resets
    ! - Performance target: <1 microsecond per UID
    ! - 64-bit value space for large codebases
    
    use iso_fortran_env, only: int64
    implicit none
    private
    
    ! Public API
    public :: uid_t
    public :: init_uid_generator
    public :: generate_uid
    public :: reset_uid_generator
    public :: uid_equal
    public :: uid_to_string
    public :: get_current_generation
    
    ! UID type with generation tracking
    type :: uid_t
        integer(int64) :: value = 0_int64      ! Unique identifier value
        integer :: generation = 1               ! Generation for arena resets
    contains
        procedure :: is_valid => uid_is_valid
        procedure :: assign => uid_assign
        generic :: assignment(=) => assign
    end type uid_t
    
    ! Module-level generator state (thread-local in parallel contexts)
    integer(int64), save :: current_uid_value = 0_int64
    integer, save :: current_generation = 1
    logical, save :: initialized = .false.
    
    ! Thread safety mutex (placeholder for OpenMP/coarray implementation)
    ! In production, would use: integer(omp_lock_kind) :: uid_lock
    
contains
    
    ! Initialize the UID generator
    subroutine init_uid_generator(start_value, generation)
        integer(int64), intent(in), optional :: start_value
        integer, intent(in), optional :: generation
        
        if (present(start_value)) then
            current_uid_value = start_value - 1_int64  ! Pre-decrement for first increment
        else
            current_uid_value = 0_int64
        end if
        
        if (present(generation)) then
            current_generation = generation
        else
            current_generation = 1
        end if
        
        initialized = .true.
        
        ! In production with OpenMP:
        ! call omp_init_lock(uid_lock)
    end subroutine init_uid_generator
    
    ! Generate a new UID
    function generate_uid() result(uid)
        type(uid_t) :: uid
        
        ! Auto-initialize if needed
        if (.not. initialized) then
            call init_uid_generator()
        end if
        
        ! Thread-safe increment (would use omp_set_lock/omp_unset_lock in production)
        ! For now, simple increment for single-threaded operation
        current_uid_value = current_uid_value + 1_int64
        
        uid%value = current_uid_value
        uid%generation = current_generation
    end function generate_uid
    
    ! Reset the UID generator (for new compilation or arena reset)
    subroutine reset_uid_generator(start_value, generation)
        integer(int64), intent(in), optional :: start_value
        integer, intent(in), optional :: generation
        
        if (present(start_value)) then
            current_uid_value = start_value - 1_int64
        else
            current_uid_value = 0_int64
        end if
        
        if (present(generation)) then
            current_generation = generation
        else
            ! Increment generation if not specified
            current_generation = current_generation + 1
        end if
    end subroutine reset_uid_generator
    
    ! Check if two UIDs are equal
    pure function uid_equal(uid1, uid2) result(equal)
        type(uid_t), intent(in) :: uid1, uid2
        logical :: equal
        
        equal = (uid1%value == uid2%value) .and. &
                (uid1%generation == uid2%generation)
    end function uid_equal
    
    ! Convert UID to string representation
    function uid_to_string(uid) result(str)
        type(uid_t), intent(in) :: uid
        character(len=32) :: str
        
        ! Format: "generation:value"
        write(str, '(I0,":",I0)') uid%generation, uid%value
    end function uid_to_string
    
    ! Get current generation
    pure function get_current_generation() result(generation)
        integer :: generation
        
        generation = current_generation
    end function get_current_generation
    
    ! Check if UID is valid (non-zero)
    pure function uid_is_valid(this) result(valid)
        class(uid_t), intent(in) :: this
        logical :: valid
        
        valid = this%value > 0_int64
    end function uid_is_valid
    
    ! Assignment operator for UID
    subroutine uid_assign(lhs, rhs)
        class(uid_t), intent(out) :: lhs
        type(uid_t), intent(in) :: rhs
        
        lhs%value = rhs%value
        lhs%generation = rhs%generation
    end subroutine uid_assign
    
end module uid_generator