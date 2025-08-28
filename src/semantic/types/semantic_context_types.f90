module semantic_context_types
    ! Define concrete context types to replace class(*) in semantic analysis
    use ast_core, only: ast_arena_t
    implicit none
    private

    ! Base context type for all semantic contexts
    type, abstract, public :: semantic_context_base_t
        integer :: context_id = 0
        character(:), allocatable :: context_name
    contains
        procedure(get_context_name_interface), deferred :: get_context_name
        procedure(clone_context_interface), deferred :: clone_context
    end type semantic_context_base_t

    ! Standard semantic context for most analysis
    type, extends(semantic_context_base_t), public :: standard_context_t
        type(ast_arena_t), pointer :: arena => null()
        integer :: current_node_index = 0
        logical :: type_checking_enabled = .true.
        logical :: scope_checking_enabled = .true.
    contains
        procedure :: get_context_name => standard_get_context_name
        procedure :: clone_context => standard_clone_context
        procedure :: assign => standard_context_assign
        generic :: assignment(=) => assign
    end type standard_context_t

    ! Specialized context for symbol analysis
    type, extends(semantic_context_base_t), public :: symbol_context_t
        character(:), allocatable :: current_scope_name
        logical :: in_procedure = .false.
        logical :: in_module = .false.
    contains
        procedure :: get_context_name => symbol_get_context_name
        procedure :: clone_context => symbol_clone_context
        procedure :: assign => symbol_context_assign
        generic :: assignment(=) => assign
    end type symbol_context_t

    ! Specialized context for type analysis
    type, extends(semantic_context_base_t), public :: type_context_t  
        integer :: current_type_level = 0
        logical :: in_derived_type = .false.
        logical :: in_interface_block = .false.
    contains
        procedure :: get_context_name => type_get_context_name
        procedure :: clone_context => type_clone_context
        procedure :: assign => type_context_assign
        generic :: assignment(=) => assign
    end type type_context_t

    ! Abstract interfaces
    abstract interface
        function get_context_name_interface(this) result(name)
            import :: semantic_context_base_t
            class(semantic_context_base_t), intent(in) :: this
            character(:), allocatable :: name
        end function get_context_name_interface

        function clone_context_interface(this) result(cloned)
            import :: semantic_context_base_t
            class(semantic_context_base_t), intent(in) :: this
            class(semantic_context_base_t), allocatable :: cloned
        end function clone_context_interface
    end interface

    ! Types are already declared public above

contains

    ! Standard context implementations
    function standard_get_context_name(this) result(name)
        class(standard_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "standard_context"
    end function standard_get_context_name

    function standard_clone_context(this) result(cloned)
        class(standard_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(standard_context_t) :: temp_context
        
        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%arena => this%arena
        temp_context%current_node_index = this%current_node_index
        temp_context%type_checking_enabled = this%type_checking_enabled
        temp_context%scope_checking_enabled = this%scope_checking_enabled
        
        allocate(cloned, source=temp_context)
    end function standard_clone_context

    subroutine standard_context_assign(lhs, rhs)
        class(standard_context_t), intent(out) :: lhs
        type(standard_context_t), intent(in) :: rhs
        
        lhs%context_id = rhs%context_id
        lhs%context_name = rhs%context_name
        lhs%arena => rhs%arena
        lhs%current_node_index = rhs%current_node_index
        lhs%type_checking_enabled = rhs%type_checking_enabled
        lhs%scope_checking_enabled = rhs%scope_checking_enabled
    end subroutine standard_context_assign

    ! Symbol context implementations
    function symbol_get_context_name(this) result(name)
        class(symbol_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "symbol_context"
    end function symbol_get_context_name

    function symbol_clone_context(this) result(cloned)
        class(symbol_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(symbol_context_t) :: temp_context
        
        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%current_scope_name = this%current_scope_name
        temp_context%in_procedure = this%in_procedure
        temp_context%in_module = this%in_module
        
        allocate(cloned, source=temp_context)
    end function symbol_clone_context

    subroutine symbol_context_assign(lhs, rhs)
        class(symbol_context_t), intent(out) :: lhs
        type(symbol_context_t), intent(in) :: rhs
        
        lhs%context_id = rhs%context_id
        lhs%context_name = rhs%context_name
        lhs%current_scope_name = rhs%current_scope_name
        lhs%in_procedure = rhs%in_procedure
        lhs%in_module = rhs%in_module
    end subroutine symbol_context_assign

    ! Type context implementations
    function type_get_context_name(this) result(name)
        class(type_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "type_context"
    end function type_get_context_name

    function type_clone_context(this) result(cloned)
        class(type_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(type_context_t) :: temp_context
        
        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%current_type_level = this%current_type_level
        temp_context%in_derived_type = this%in_derived_type
        temp_context%in_interface_block = this%in_interface_block
        
        allocate(cloned, source=temp_context)
    end function type_clone_context

    subroutine type_context_assign(lhs, rhs)
        class(type_context_t), intent(out) :: lhs
        type(type_context_t), intent(in) :: rhs
        
        lhs%context_id = rhs%context_id
        lhs%context_name = rhs%context_name
        lhs%current_type_level = rhs%current_type_level
        lhs%in_derived_type = rhs%in_derived_type
        lhs%in_interface_block = rhs%in_interface_block
    end subroutine type_context_assign

end module semantic_context_types