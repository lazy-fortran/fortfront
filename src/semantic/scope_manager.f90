module scope_manager
    ! Hierarchical scope management for semantic analysis
    use, intrinsic :: iso_fortran_env, only: error_unit
    use type_system_unified
    use identifier_table, only: identifier_table_t, identifier_table_init, &
        identifier_table_intern, identifier_table_find, &
        identifier_id_kind
    use error_handling, only: &
        success_result, ERROR_MEMORY
    implicit none
    private

    public :: scope_t, scope_stack_t
    public :: create_scope, create_scope_stack

    ! Scope types
    integer, parameter, public :: SCOPE_GLOBAL = 1
    integer, parameter, public :: SCOPE_MODULE = 2
    integer, parameter, public :: SCOPE_FUNCTION = 3
        integer, parameter, public :: SCOPE_SUBROUTINE = 4
            integer, parameter, public :: SCOPE_BLOCK = 5 ! if/do/etc blocks
            integer, parameter, public :: SCOPE_INTERFACE = 6

            ! Single scope with its own environment
            ! (no parent pointer - stack handles hierarchy)
            type :: scope_t
                integer :: scope_type = SCOPE_GLOBAL
                character(len=:), allocatable :: name ! e.g., module name, function name
                type(type_env_t) :: env
                type(identifier_table_t), pointer :: identifiers => null()
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
                type(scope_t), allocatable :: scopes(:) ! Stack of scopes (contiguous memory)
                integer :: depth = 0 ! Current depth (top of stack)
                integer :: capacity = 0 ! Array capacity
                type(identifier_table_t), pointer :: identifier_storage => null()
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
                    subroutine create_scope(scope, scope_type, name, identifiers)
                        type(scope_t), intent(out) :: scope
                        integer, intent(in) :: scope_type
                        character(len=*), intent(in), optional :: name
                        type(identifier_table_t), target, intent(inout), optional :: identifiers

                        scope%scope_type = scope_type

                        if (present(name)) then
                            scope%name = name
                        else
                            scope%name = ""
                        end if

                        if (present(identifiers)) then
                            scope%identifiers => identifiers
                            scope%env%identifiers => identifiers
                        else
                            nullify (scope%identifiers)
                            nullify (scope%env%identifiers)
                        end if

                        ! Initialize empty environment (allocate on heap to avoid large stack usage)
                        scope%env%count = 0
                        scope%env%capacity = 64
                        if (allocated(scope%env%name_ids)) deallocate (scope%env%name_ids)
                        if (allocated(scope%env%schemes)) deallocate (scope%env%schemes)
                        allocate (scope%env%name_ids(scope%env%capacity))
                        allocate (scope%env%schemes(scope%env%capacity))

                    end subroutine create_scope

                    ! Create a new scope stack with global scope
                    ! Intent(out) avoids large return copies
                    subroutine create_scope_stack(stack)
                        type(scope_stack_t), intent(out) :: stack

                        ! Initialize capacity and create global scope
                        stack%capacity = 10
                        allocate (stack%scopes(stack%capacity))
                        stack%depth = 1
                        if (.not. associated(stack%identifier_storage)) then
                            allocate (stack%identifier_storage)
                        end if
                        call identifier_table_init(stack%identifier_storage)
                        call create_scope(stack%scopes(1), SCOPE_GLOBAL, "global", &
                            stack%identifier_storage)

                    end subroutine create_scope_stack

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

                        if (.not. associated(this%identifiers)) then
                            return
                        end if

                        ! Fixed arrays are always allocated, check count instead
                        if (this%env%count == 0) then
                            return
                        end if

                        ! Direct implementation to avoid type-bound procedure issues
                        block
                            integer :: j
                            integer(identifier_id_kind) :: name_id

                            name_id = identifier_table_find(this%identifiers, name)
                            if (name_id <= 0) return

                            do j = 1, this%env%count
                                if (this%env%name_ids(j) == name_id) then
                                    ! Allocate and use assignment operator for deep copy
                                    allocate (scheme)
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
                        integer(identifier_id_kind) :: name_id
                        integer :: j

                        if (.not. associated(this%identifiers)) then
                            write (error_unit, '(A)') &
                                'ERROR [scope_manager]: scope missing identifier table; skipping define'
                            return
                        end if

                        ! Robust define with local allocation guards
                        if (.not. allocated(this%env%name_ids) .or. &
                            .not. allocated(this%env%schemes)) then
                            if (this%env%capacity <= 0) this%env%capacity = 64
                            allocate (this%env%name_ids(this%env%capacity))
                            allocate (this%env%schemes(this%env%capacity))
                            this%env%count = 0
                        else if (size(this%env%name_ids) == 0 .or. size(this%env%schemes) == 0) then
                            if (this%env%capacity <= 0) this%env%capacity = 64
                            deallocate (this%env%name_ids, this%env%schemes)
                            allocate (this%env%name_ids(this%env%capacity))
                            allocate (this%env%schemes(this%env%capacity))
                            this%env%count = 0
                        end if

                        if (this%env%count >= this%env%capacity) then
                            block
                                integer :: new_capacity, j
                                integer(identifier_id_kind), allocatable :: new_ids(:)
                                type(poly_type_t), allocatable :: new_schemes(:)
                                new_capacity = max(64, this%env%capacity * 2)
                                allocate (new_ids(new_capacity))
                                allocate (new_schemes(new_capacity))
                                if (this%env%count > 0) then
                                    do j = 1, this%env%count
                                        new_ids(j) = this%env%name_ids(j)
                                        new_schemes(j) = this%env%schemes(j)
                                    end do
                                end if
                                call move_alloc(new_ids, this%env%name_ids)
                                call move_alloc(new_schemes, this%env%schemes)
                                this%env%capacity = new_capacity
                            end block
                        end if

                        name_id = identifier_table_intern(this%identifiers, name)

                        do j = 1, this%env%count
                            if (this%env%name_ids(j) == name_id) then
                                this%env%schemes(j) = scheme
                                return
                            end if
                        end do

                        this%env%count = this%env%count + 1
                        this%env%name_ids(this%env%count) = name_id
                        this%env%schemes(this%env%count) = scheme

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
                            new_capacity = this%capacity * 2
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
                                        ! Deep copy env via assignment (allocates and copies entries)
                                        temp_scopes(i)%env = this%scopes(i)%env
                                        call temp_scopes(i)%env%ensure_capacity(max(64, &
                                            temp_scopes(i)%env%count))
                                        if (associated(this%scopes(i)%identifiers)) then
                                            temp_scopes(i)%identifiers => this%identifier_storage
                                            temp_scopes(i)%env%identifiers => this%identifier_storage
                                        else
                                            nullify (temp_scopes(i)%identifiers)
                                            nullify (temp_scopes(i)%env%identifiers)
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
                        this%scopes(this%depth)%identifiers => this%identifier_storage
                        ! Deep copy env via assignment (allocates and copies used elements)
                        this%scopes(this%depth)%env = new_scope%env
                        this%scopes(this%depth)%env%identifiers => this%identifier_storage
                        call this%scopes(this%depth)%env%ensure_capacity(max(64, &
                            this%scopes(this%depth)%env%count))

                    end subroutine stack_push_scope

                    ! Stack: pop current scope (simple decrement)
                    subroutine stack_pop_scope(this)
                        class(scope_stack_t), intent(inout) :: this

                        if (this%depth > 1) then
                            this%depth = this%depth - 1
                        else
                            write (error_unit, '(A)') &
                                'ERROR [scope_manager]: Cannot pop global scope - ignoring pop request'
                            ! Don't modify depth - keep the global scope intact
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
                            ! Ensure environment arrays exist for current scope
                            call this%scopes(this%depth)%env%ensure_capacity(max(64, &
                                this%scopes(this%depth)%env%count + 1))
                            ! Use direct scope_define to avoid type-bound procedure issues with arrays
                            call scope_define(this%scopes(this%depth), name, scheme)
                        else
                            write (error_unit, '(A)') &
                                'ERROR [scope_manager]: No current scope for define; ignoring'
                            ! Don't perform the definition if there's no current scope
                        end if

                    end subroutine stack_define

                    ! Enter module scope
                    subroutine stack_enter_module(this, module_name)
                        class(scope_stack_t), intent(inout) :: this
                        character(len=*), intent(in) :: module_name
                        type(scope_t) :: new_scope

                        call create_scope(new_scope, SCOPE_MODULE, module_name, this%identifier_storage)
                        call this%push(new_scope)

                    end subroutine stack_enter_module

                    ! Enter function scope
                    subroutine stack_enter_function(this, function_name)
                        class(scope_stack_t), intent(inout) :: this
                        character(len=*), intent(in) :: function_name
                        type(scope_t) :: new_scope

                        call create_scope(new_scope, SCOPE_FUNCTION, function_name, &
                            this%identifier_storage)
                        call this%push(new_scope)

                    end subroutine stack_enter_function

                    ! Enter subroutine scope
                    subroutine stack_enter_subroutine(this, subroutine_name)
                        class(scope_stack_t), intent(inout) :: this
                        character(len=*), intent(in) :: subroutine_name
                        type(scope_t) :: new_scope

                        call create_scope(new_scope, SCOPE_SUBROUTINE, subroutine_name, &
                            this%identifier_storage)
                        call this%push(new_scope)

                    end subroutine stack_enter_subroutine

                    ! Enter block scope (if/do/etc)
                    subroutine stack_enter_block(this)
                        class(scope_stack_t), intent(inout) :: this
                        type(scope_t) :: new_scope

                        call create_scope(new_scope, SCOPE_BLOCK, "", this%identifier_storage)
                        call this%push(new_scope)

                    end subroutine stack_enter_block

                    ! Enter interface scope
                    subroutine stack_enter_interface(this, interface_name)
                        class(scope_stack_t), intent(inout) :: this
                        character(len=*), intent(in), optional :: interface_name
                        type(scope_t) :: new_scope

                        if (present(interface_name)) then
                            call create_scope(new_scope, SCOPE_INTERFACE, interface_name, &
                                this%identifier_storage)
                        else
                            call create_scope(new_scope, SCOPE_INTERFACE, "", this%identifier_storage)
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
                        copy%env = this%env ! Uses type_env_t assignment (deep copy)
                        if (associated(this%identifiers)) then
                            copy%identifiers => this%identifiers
                            copy%env%identifiers => this%identifiers
                        else
                            nullify (copy%identifiers)
                            nullify (copy%env%identifiers)
                        end if
                    end function scope_deep_copy

                    ! Assignment operator for scope_t (deep copy)
                    subroutine scope_assign(lhs, rhs)
                        class(scope_t), intent(out) :: lhs
                        type(scope_t), intent(in) :: rhs

                        lhs%scope_type = rhs%scope_type
                        if (allocated(rhs%name)) then
                            lhs%name = rhs%name
                        end if
                        lhs%env = rhs%env ! Uses type_env_t assignment (deep copy)
                        if (associated(rhs%identifiers)) then
                            lhs%identifiers => rhs%identifiers
                            lhs%env%identifiers => rhs%identifiers
                        else
                            nullify (lhs%identifiers)
                            nullify (lhs%env%identifiers)
                        end if
                    end subroutine scope_assign

                    ! Deep copy a scope stack
                    function scope_stack_deep_copy(this) result(copy)
                        class(scope_stack_t), intent(in) :: this
                        type(scope_stack_t) :: copy
                        integer :: i

                        copy%depth = this%depth
                        copy%capacity = this%capacity
                        if (associated(this%identifier_storage)) then
                            if (.not. associated(copy%identifier_storage)) then
                                allocate (copy%identifier_storage)
                            end if
                            copy%identifier_storage = this%identifier_storage
                        else
                            if (associated(copy%identifier_storage)) then
                                deallocate (copy%identifier_storage)
                            end if
                            nullify (copy%identifier_storage)
                        end if

                        if (allocated(this%scopes)) then
                            allocate (copy%scopes(size(this%scopes)))
                            do i = 1, size(this%scopes)
                                copy%scopes(i) = this%scopes(i) ! Uses scope_t assignment (deep copy)
                                if (associated(copy%identifier_storage)) then
                                    copy%scopes(i)%identifiers => copy%identifier_storage
                                    copy%scopes(i)%env%identifiers => copy%identifier_storage
                                else
                                    nullify (copy%scopes(i)%identifiers)
                                    nullify (copy%scopes(i)%env%identifiers)
                                end if
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
                        if (associated(rhs%identifier_storage)) then
                            if (.not. associated(lhs%identifier_storage)) then
                                allocate (lhs%identifier_storage)
                            end if
                            lhs%identifier_storage = rhs%identifier_storage
                        else
                            if (associated(lhs%identifier_storage)) then
                                deallocate (lhs%identifier_storage)
                            end if
                            nullify (lhs%identifier_storage)
                        end if

                        if (allocated(rhs%scopes)) then
                            allocate (lhs%scopes(size(rhs%scopes)))
                            do i = 1, size(rhs%scopes)
                                lhs%scopes(i) = rhs%scopes(i) ! Uses scope_t assignment (deep copy)
                                if (associated(lhs%identifier_storage)) then
                                    lhs%scopes(i)%identifiers => lhs%identifier_storage
                                    lhs%scopes(i)%env%identifiers => lhs%identifier_storage
                                else
                                    nullify (lhs%scopes(i)%identifiers)
                                    nullify (lhs%scopes(i)%env%identifiers)
                                end if
                            end do
                        end if
                    end subroutine scope_stack_assign

                    ! Finalization removed - automatic cleanup with allocatable arrays

                end module scope_manager
