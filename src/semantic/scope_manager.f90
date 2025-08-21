module scope_manager
    ! Hierarchical scope management for semantic analysis
    use type_system_hm
    implicit none
    private

    public :: scope_t, scope_stack_t
    public :: create_scope, create_scope_stack

    ! Scope types
    integer, parameter, public :: SCOPE_GLOBAL = 1
    integer, parameter, public :: SCOPE_MODULE = 2
    integer, parameter, public :: SCOPE_FUNCTION = 3
    integer, parameter, public :: SCOPE_SUBROUTINE = 4
    integer, parameter, public :: SCOPE_BLOCK = 5     ! if/do/etc blocks
    integer, parameter, public :: SCOPE_INTERFACE = 6

    ! Single scope with its own environment
    ! (no parent pointer - stack handles hierarchy)
    type :: scope_t
        integer :: scope_type = SCOPE_GLOBAL
        character(len=:), allocatable :: name  ! e.g., module name, function name
        type(type_env_t) :: env
        ! No parent pointer - stack handles hierarchy
    contains
        procedure :: lookup => scope_lookup
        procedure :: define => scope_define
        procedure :: deep_copy => scope_deep_copy
        procedure :: assign => scope_assign
        generic :: assignment(=) => assign
        ! Remove lookup_recursive - stack handles traversal
    end type scope_t

    ! Stack of scopes for managing nested scopes (cache-efficient design)
    type :: scope_stack_t
        type(scope_t), allocatable :: scopes(:)  ! Stack of scopes (contiguous memory)
        integer :: depth = 0                     ! Current depth (top of stack)
        integer :: capacity = 0                  ! Array capacity
        ! No current/global pointers - use array indices
    contains
        procedure :: push => stack_push_scope
        procedure :: pop => stack_pop_scope
        procedure :: lookup => stack_lookup
        procedure :: define => stack_define
        procedure :: enter_module => stack_enter_module
        procedure :: enter_function => stack_enter_function
        procedure :: enter_subroutine => stack_enter_subroutine
        procedure :: enter_block => stack_enter_block
        procedure :: enter_interface => stack_enter_interface
        procedure :: leave_scope => stack_leave_scope
        procedure :: get_current_scope_type => stack_get_current_scope_type
        procedure :: deep_copy => scope_stack_deep_copy
        procedure :: assign => scope_stack_assign
        generic :: assignment(=) => assign
        ! Remove finalize - automatic cleanup with allocatable
    end type scope_stack_t

contains

    ! Create a new scope (no parent pointer - stack handles hierarchy)
    function create_scope(scope_type, name) result(scope)
        integer, intent(in) :: scope_type
        character(len=*), intent(in), optional :: name
        type(scope_t) :: scope

        scope%scope_type = scope_type

        if (present(name)) then
            scope%name = name
        else
            scope%name = ""
        end if

        ! No parent pointer - stack handles hierarchy

        ! Initialize empty environment
        scope%env%count = 0
        scope%env%capacity = 10  ! Start with some capacity
        allocate (character(len=256) :: scope%env%names(scope%env%capacity))
        allocate (scope%env%schemes(scope%env%capacity))

    end function create_scope

    ! Create a new scope stack with global scope
    function create_scope_stack() result(stack)
        type(scope_stack_t) :: stack

        ! Initialize capacity and create global scope
        stack%capacity = 10
        allocate (stack%scopes(stack%capacity))
        stack%depth = 1
        stack%scopes(1) = create_scope(SCOPE_GLOBAL, "global")

    end function create_scope_stack

    ! Scope lookup (local only)
    subroutine scope_lookup(this, name, scheme)
        class(scope_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable, intent(out) :: scheme

        ! intent(out) automatically deallocates scheme on entry

        ! Safety check: ensure env is properly initialized
        if (this%env%count < 0 .or. this%env%capacity < 0) then
            return
        end if

        ! Additional safety check for uninitialized env
        if (.not. allocated(this%env%names) .or. .not. allocated(this%env%schemes)) then
            return
        end if

        ! Direct implementation to avoid type-bound procedure issues
        block
            integer :: j
            do j = 1, this%env%count
                if (this%env%names(j) == name) then
                    ! Allocate and initialize to ensure clean state
                    allocate(scheme)
                    call scheme%init()
                    ! Now safe to use assignment operator
                    scheme = this%env%schemes(j)
                    return
                end if
            end do
        end block

    end subroutine scope_lookup

    ! Scope define (add to local scope)
    subroutine scope_define(this, name, scheme)
        class(scope_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        ! Direct implementation to avoid type-bound procedure issues
        block
            character(len=:), allocatable :: temp_names(:)
            type(poly_type_t), allocatable :: temp_schemes(:)
            integer :: new_capacity, j

            ! Initialize or grow arrays if needed
            if (this%env%capacity == 0) then
                this%env%capacity = 10
                allocate (character(len=256) :: this%env%names(this%env%capacity))
                allocate (this%env%schemes(this%env%capacity))
            else if (this%env%count >= this%env%capacity) then
                new_capacity = this%env%capacity*2
                allocate (character(len=256) :: temp_names(new_capacity))
                allocate (temp_schemes(new_capacity))
                do j = 1, this%env%count
                    temp_names(j) = this%env%names(j)
                    temp_schemes(j) = this%env%schemes(j)
                end do
                ! Use move_alloc for O(1) performance instead of O(n) copying
                call move_alloc(temp_names, this%env%names)
                call move_alloc(temp_schemes, this%env%schemes)
                this%env%capacity = new_capacity
            end if

            ! Add new binding
            this%env%count = this%env%count + 1
            this%env%names(this%env%count) = name
            ! Deep copy the scheme to ensure proper storage
            ! First ensure the target is in a clean state
            if (allocated(this%env%schemes(this%env%count)%forall)) then
                deallocate(this%env%schemes(this%env%count)%forall)
            end if
            this%env%schemes(this%env%count) = scheme
        end block

    end subroutine scope_define

    ! Recursive lookup removed - stack handles traversal in stack_lookup

    ! Stack: push a new scope using safe array extension
    subroutine stack_push_scope(this, new_scope)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t), intent(in) :: new_scope
        type(scope_t), allocatable :: temp_scopes(:)
        integer :: new_capacity, j

        ! Grow array if needed (following CLAUDE.md safe array extension)
        if (this%depth >= this%capacity) then
            new_capacity = this%capacity*2
            if (new_capacity == 0) new_capacity = 10
            allocate (temp_scopes(new_capacity))
            if (this%depth > 0) then
                ! Deep copy each scope to preserve type-bound procedures
                block
                    integer :: i, j
                    do i = 1, this%depth
                        temp_scopes(i)%scope_type = this%scopes(i)%scope_type
                        if (allocated(this%scopes(i)%name)) then
                            temp_scopes(i)%name = this%scopes(i)%name
                        end if
                        ! Deep copy env to avoid shallow copy issues
                        temp_scopes(i)%env%count = this%scopes(i)%env%count
                        temp_scopes(i)%env%capacity = this%scopes(i)%env%capacity
                        if (allocated(this%scopes(i)%env%names)) then
  allocate (character(len=256) :: temp_scopes(i)%env%names(this%scopes(i)%env%capacity))
                            do j = 1, this%scopes(i)%env%count
                               temp_scopes(i)%env%names(j) = this%scopes(i)%env%names(j)
                            end do
                        end if
                        if (allocated(this%scopes(i)%env%schemes)) then
                      allocate (temp_scopes(i)%env%schemes(this%scopes(i)%env%capacity))
                            temp_scopes(i)%env%schemes(1:this%scopes(i)%env%count) = &
                                this%scopes(i)%env%schemes(1:this%scopes(i)%env%count)
                        end if
                    end do
                end block
            end if
            ! Use move_alloc for O(1) performance instead of O(n) copying
            call move_alloc(temp_scopes, this%scopes)
            this%capacity = new_capacity
        end if

        ! Push new scope onto stack
        this%depth = this%depth + 1
        ! Deep copy scope to ensure type-bound procedures are preserved
        this%scopes(this%depth)%scope_type = new_scope%scope_type
        if (allocated(new_scope%name)) then
            this%scopes(this%depth)%name = new_scope%name
        end if
        ! Deep copy env to avoid shallow copy issues
        this%scopes(this%depth)%env%count = new_scope%env%count
        this%scopes(this%depth)%env%capacity = new_scope%env%capacity
        if (allocated(new_scope%env%names) .and. new_scope%env%capacity > 0) then
            if (allocated(this%scopes(this%depth)%env%names)) then
                deallocate (this%scopes(this%depth)%env%names)
            end if
            allocate(character(len=256) :: &
                     this%scopes(this%depth)%env%names(new_scope%env%capacity))
            do j = 1, new_scope%env%count
                this%scopes(this%depth)%env%names(j) = new_scope%env%names(j)
            end do
        end if
        if (allocated(new_scope%env%schemes) .and. new_scope%env%capacity > 0) then
            if (allocated(this%scopes(this%depth)%env%schemes)) then
                deallocate (this%scopes(this%depth)%env%schemes)
            end if
            allocate (this%scopes(this%depth)%env%schemes(new_scope%env%capacity))
            if (new_scope%env%count > 0) then
                this%scopes(this%depth)%env%schemes(1:new_scope%env%count) = &
                    new_scope%env%schemes(1:new_scope%env%count)
            end if
        end if

    end subroutine stack_push_scope

    ! Stack: pop current scope (simple decrement)
    subroutine stack_pop_scope(this)
        class(scope_stack_t), intent(inout) :: this

        if (this%depth > 1) then
            this%depth = this%depth - 1
        else
            error stop "Cannot pop global scope"
        end if

    end subroutine stack_pop_scope

    ! Stack: lookup with hierarchical search (walk down the stack)
    subroutine stack_lookup(this, name, scheme)
        class(scope_stack_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable, intent(out) :: scheme
        integer :: i

        ! intent(out) automatically deallocates scheme on entry

        ! Walk down the stack from current scope to global scope
        do i = this%depth, 1, -1
            ! Use direct scope_lookup to avoid type-bound procedure issues with arrays
            call scope_lookup(this%scopes(i), name, scheme)
            if (allocated(scheme)) then
                return
            end if
        end do

    end subroutine stack_lookup

    ! Stack: define in current scope (top of stack)
    subroutine stack_define(this, name, scheme)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        if (this%depth > 0) then
            ! Use direct scope_define to avoid type-bound procedure issues with arrays
            call scope_define(this%scopes(this%depth), name, scheme)
        else
            error stop "No current scope"
        end if

    end subroutine stack_define

    ! Enter module scope
    subroutine stack_enter_module(this, module_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: module_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_MODULE, module_name)
        call this%push(new_scope)

    end subroutine stack_enter_module

    ! Enter function scope
    subroutine stack_enter_function(this, function_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: function_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_FUNCTION, function_name)
        call this%push(new_scope)

    end subroutine stack_enter_function

    ! Enter subroutine scope
    subroutine stack_enter_subroutine(this, subroutine_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: subroutine_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_SUBROUTINE, subroutine_name)
        call this%push(new_scope)

    end subroutine stack_enter_subroutine

    ! Enter block scope (if/do/etc)
    subroutine stack_enter_block(this)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_BLOCK, "")
        call this%push(new_scope)

    end subroutine stack_enter_block

    ! Enter interface scope
    subroutine stack_enter_interface(this, interface_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in), optional :: interface_name
        type(scope_t) :: new_scope

        if (present(interface_name)) then
            new_scope = create_scope(SCOPE_INTERFACE, interface_name)
        else
            new_scope = create_scope(SCOPE_INTERFACE, "")
        end if
        call this%push(new_scope)

    end subroutine stack_enter_interface

    ! Leave current scope
    subroutine stack_leave_scope(this)
        class(scope_stack_t), intent(inout) :: this

        call this%pop()

    end subroutine stack_leave_scope

    ! Get current scope type
    function stack_get_current_scope_type(this) result(scope_type)
        class(scope_stack_t), intent(in) :: this
        integer :: scope_type

        if (this%depth > 0) then
            scope_type = this%scopes(this%depth)%scope_type
        else
            scope_type = SCOPE_GLOBAL
        end if

    end function stack_get_current_scope_type

    ! Deep copy a scope
    function scope_deep_copy(this) result(copy)
        class(scope_t), intent(in) :: this
        type(scope_t) :: copy

        copy%scope_type = this%scope_type
        if (allocated(this%name)) then
            copy%name = this%name
        end if
        copy%env = this%env  ! Uses type_env_t assignment (deep copy)
    end function scope_deep_copy

    ! Assignment operator for scope_t (deep copy)
    subroutine scope_assign(lhs, rhs)
        class(scope_t), intent(out) :: lhs
        type(scope_t), intent(in) :: rhs

        lhs%scope_type = rhs%scope_type
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        end if
        lhs%env = rhs%env  ! Uses type_env_t assignment (deep copy)
    end subroutine scope_assign

    ! Deep copy a scope stack
    function scope_stack_deep_copy(this) result(copy)
        class(scope_stack_t), intent(in) :: this
        type(scope_stack_t) :: copy
        integer :: i

        copy%depth = this%depth
        copy%capacity = this%capacity

        if (allocated(this%scopes)) then
            allocate (copy%scopes(size(this%scopes)))
            do i = 1, size(this%scopes)
                copy%scopes(i) = this%scopes(i)  ! Uses scope_t assignment (deep copy)
            end do
        end if
    end function scope_stack_deep_copy

    ! Assignment operator for scope_stack_t (deep copy)
    subroutine scope_stack_assign(lhs, rhs)
        class(scope_stack_t), intent(out) :: lhs
        type(scope_stack_t), intent(in) :: rhs
        integer :: i

        lhs%depth = rhs%depth
        lhs%capacity = rhs%capacity

        if (allocated(rhs%scopes)) then
            allocate (lhs%scopes(size(rhs%scopes)))
            do i = 1, size(rhs%scopes)
                lhs%scopes(i) = rhs%scopes(i)  ! Uses scope_t assignment (deep copy)
            end do
        end if
    end subroutine scope_stack_assign

    ! Finalization removed - automatic cleanup with allocatable arrays

end module scope_manager
