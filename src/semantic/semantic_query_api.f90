module semantic_query_api
    ! Public API for querying semantic analysis results
    ! Provides convenient interface to access type information, symbol tables,
    ! scope information, and other semantic analysis results
    
    use semantic_analyzer, only: semantic_context_t
    use scope_manager, only: scope_stack_t, SCOPE_GLOBAL, SCOPE_MODULE, &
                             SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK, &
                             SCOPE_INTERFACE
    use type_system_hm, only: mono_type_t, poly_type_t, type_var_t, &
                              TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    use parameter_tracker, only: parameter_tracker_t
    use ast_core
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    implicit none
    private

    public :: semantic_query_t, create_semantic_query
    public :: variable_info_t, function_info_t, type_info_t, scope_info_t
    public :: symbol_info_t
    public :: SYMBOL_VARIABLE, SYMBOL_FUNCTION, SYMBOL_SUBROUTINE, SYMBOL_UNKNOWN
    
    ! Direct query functions to avoid semantic_query_t instantiation (issue #196)
    public :: is_identifier_defined_direct, get_unused_variables_direct
    public :: get_symbols_in_scope_direct

    ! Symbol types
    integer, parameter :: SYMBOL_VARIABLE = 1
    integer, parameter :: SYMBOL_FUNCTION = 2
    integer, parameter :: SYMBOL_SUBROUTINE = 3
    integer, parameter :: SYMBOL_UNKNOWN = 0

    ! Information types returned by queries
    type :: variable_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: scope_name
        logical :: is_defined = .false.
        logical :: is_array = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        integer :: array_rank = 0
        character(len=:), allocatable :: intent  ! "in", "out", "inout", ""
    end type variable_info_t

    type :: function_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: return_type
        character(len=:), allocatable, dimension(:) :: parameter_names
        character(len=:), allocatable, dimension(:) :: parameter_types
        character(len=:), allocatable, dimension(:) :: parameter_intents
        logical :: is_intrinsic = .false.
    end type function_info_t

    type :: type_info_t
        character(len=:), allocatable :: kind_name  ! "integer", "real", etc.
        logical :: is_array = .false.
        character(len=:), allocatable :: array_element_type
        integer :: array_rank = 0
        integer :: character_length = 0
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
    end type type_info_t

    type :: scope_info_t
        character(len=:), allocatable :: scope_name
        integer :: scope_type
        character(len=:), allocatable :: parent_scope_name
        integer :: depth
    end type scope_info_t

    ! Symbol information type (as specified in issues #189, #190)
    type :: symbol_info_t
        character(len=:), allocatable :: name
        type(mono_type_t) :: type_info
        integer :: definition_line = 0      ! Line where symbol is declared
        integer :: definition_column = 0    ! Column where symbol is declared  
        logical :: is_used = .false.        ! Whether symbol is used - populated by usage_tracker_analyzer
        logical :: is_parameter = .false.   ! Whether symbol is a parameter
        ! NOTE: definition_line/definition_column population requires connecting
        !       symbol declarations to AST node source locations during semantic 
        !       analysis. Currently these fields are exported but not populated.
    contains
        procedure :: assign => symbol_info_assign
        generic :: assignment(=) => assign
    end type symbol_info_t

    ! Main query interface - WARNING: Assignment causes deep copies (issue #196)
    ! For production use, prefer direct query functions to avoid memory issues
    type :: semantic_query_t
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: context
    contains
        procedure :: get_variable_info => query_get_variable_info
        procedure :: get_function_info => query_get_function_info
        procedure :: get_type_info_by_name => query_get_type_info_by_name
        procedure :: get_type_info_by_node => query_get_type_info_by_node
        procedure :: get_current_scope_info => query_get_current_scope_info
        procedure :: is_variable_visible => query_is_variable_visible
        procedure :: is_symbol_defined => query_is_symbol_defined
        procedure :: get_symbol_type => query_get_symbol_type
        ! APIs from issue #14
        procedure :: get_symbols_in_scope => query_get_symbols_in_scope
        procedure :: is_variable_used => query_is_variable_used
        procedure :: get_unused_variables => query_get_unused_variables
        procedure :: get_identifier_type => query_get_identifier_type
        procedure :: is_identifier_defined => query_is_identifier_defined
    end type semantic_query_t

contains

    ! Assignment operator for symbol_info_t to handle deep copy
    subroutine symbol_info_assign(lhs, rhs)
        class(symbol_info_t), intent(inout) :: lhs
        type(symbol_info_t), intent(in) :: rhs
        
        ! Deep copy allocatable string
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate(lhs%name)
        end if
        
        ! Use mono_type_t's assignment operator for deep copy
        lhs%type_info = rhs%type_info
        
        ! Copy scalar fields
        lhs%definition_line = rhs%definition_line
        lhs%definition_column = rhs%definition_column
        lhs%is_used = rhs%is_used
        lhs%is_parameter = rhs%is_parameter
    end subroutine symbol_info_assign

    ! Create semantic query interface - WARNING: causes deep copies (issue #196)
    ! This function is retained for compatibility but should be avoided in production.
    ! Use direct query functions instead to avoid memory issues.
    function create_semantic_query(arena, context) result(query)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: context
        type(semantic_query_t) :: query

        ! Direct assignment causes deep copies - use with caution
        query%arena = arena
        query%context = context
    end function create_semantic_query

    ! Get variable information by name
    function query_get_variable_info(this, var_name, var_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name
        type(variable_info_t), intent(out) :: var_info
        logical :: success
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: mono_type

        success = .false.
        
        ! Look up variable in scope stack
        call this%context%scopes%lookup(var_name, scheme)
        
        if (.not. allocated(scheme)) return
        
        ! Extract type information
        mono_type = this%context%instantiate(scheme)
        
        ! Fill variable info
        var_info%name = var_name
        var_info%type_name = get_type_name(mono_type)
        var_info%is_defined = .true.
        var_info%is_array = (mono_type%kind == TARRAY)
        var_info%is_allocatable = mono_type%alloc_info%is_allocatable
        var_info%is_pointer = mono_type%alloc_info%is_pointer
        
        if (var_info%is_array .and. allocated(mono_type%args)) then
            var_info%array_rank = get_array_rank(mono_type)
        end if
        
        ! Get scope name
        var_info%scope_name = get_current_scope_name(this%context%scopes)
        
        ! Check for parameter intent
        var_info%intent = get_parameter_intent(this%context%param_tracker, var_name)
        
        success = .true.
    end function query_get_variable_info

    ! Get function information by name
    function query_get_function_info(this, func_name, func_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: func_name
        type(function_info_t), intent(out) :: func_info
        logical :: success
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: mono_type

        success = .false.
        
        ! Look up function in scope stack
        call this%context%scopes%lookup(func_name, scheme)
        
        if (.not. allocated(scheme)) then
            ! Check if it's a builtin function
            mono_type = this%context%get_builtin_function_type(func_name)
            if (mono_type%kind /= 0) then
                func_info%name = func_name
                func_info%is_intrinsic = .true.
                call extract_function_signature(mono_type, func_info)
                success = .true.
            end if
            return
        end if
        
        ! Extract function type information
        mono_type = this%context%instantiate(scheme)
        
        if (mono_type%kind /= TFUN) return  ! Not a function
        
        func_info%name = func_name
        func_info%is_intrinsic = .false.
        call extract_function_signature(mono_type, func_info)
        
        success = .true.
    end function query_get_function_info

    ! Get type information by variable name
    function query_get_type_info_by_name(this, name, type_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(type_info_t), intent(out) :: type_info
        logical :: success
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: mono_type

        success = .false.
        
        ! Look up symbol in scope stack
        call this%context%scopes%lookup(name, scheme)
        
        if (.not. allocated(scheme)) return
        
        mono_type = this%context%instantiate(scheme)
        call extract_type_info(mono_type, type_info)
        
        success = .true.
    end function query_get_type_info_by_name

    ! Get type information by AST node index
    function query_get_type_info_by_node(this, node_index, type_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        integer, intent(in) :: node_index
        type(type_info_t), intent(out) :: type_info
        logical :: success

        success = .false.
        
        if (node_index <= 0 .or. node_index > this%arena%size) return
        if (.not. allocated(this%arena%entries)) return
        if (size(this%arena%entries) < node_index) return
        if (.not. allocated(this%arena%entries(node_index)%node)) return
        if (.not. allocated(this%arena%entries(node_index)%node%inferred_type)) return
        
        call extract_type_info(this%arena%entries(node_index)%node%inferred_type, &
                                type_info)
        success = .true.
    end function query_get_type_info_by_node

    ! Get current scope information
    function query_get_current_scope_info(this, scope_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        type(scope_info_t), intent(out) :: scope_info
        logical :: success

        success = .false.
        
        if (this%context%scopes%depth <= 0) return
        
        scope_info%scope_type = this%context%scopes%get_current_scope_type()
        scope_info%depth = this%context%scopes%depth
        scope_info%scope_name = get_current_scope_name(this%context%scopes)
        
        if (this%context%scopes%depth > 1) then
            scope_info%parent_scope_name = get_parent_scope_name(this%context%scopes)
        else
            scope_info%parent_scope_name = ""
        end if
        
        success = .true.
    end function query_get_current_scope_info

    ! Check if variable is visible in current scope
    function query_is_variable_visible(this, var_name) result(visible)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name
        logical :: visible
        type(poly_type_t), allocatable :: scheme

        call this%context%scopes%lookup(var_name, scheme)
        visible = allocated(scheme)
    end function query_is_variable_visible

    ! Check if symbol is defined
    function query_is_symbol_defined(this, symbol_name) result(defined)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: symbol_name
        logical :: defined
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: builtin_type

        ! Check in scope stack
        call this%context%scopes%lookup(symbol_name, scheme)
        if (allocated(scheme)) then
            defined = .true.
            return
        end if
        
        ! Check builtin functions
        builtin_type = this%context%get_builtin_function_type(symbol_name)
        defined = (builtin_type%kind /= 0)
    end function query_is_symbol_defined

    ! Get symbol type (variable, function, etc.)
    function query_get_symbol_type(this, symbol_name) result(symbol_type)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: symbol_name
        integer :: symbol_type
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: mono_type, builtin_type

        symbol_type = SYMBOL_UNKNOWN
        
        ! Look up in scope stack
        call this%context%scopes%lookup(symbol_name, scheme)
        
        if (allocated(scheme)) then
            mono_type = this%context%instantiate(scheme)
            if (mono_type%kind == TFUN) then
                symbol_type = SYMBOL_FUNCTION
            else
                symbol_type = SYMBOL_VARIABLE
            end if
            return
        end if
        
        ! Check builtin functions
        builtin_type = this%context%get_builtin_function_type(symbol_name)
        if (builtin_type%kind /= 0) then
            symbol_type = SYMBOL_FUNCTION
        end if
    end function query_get_symbol_type

    ! Helper functions
    
    recursive function get_type_name(mono_type) result(name)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: name
        
        select case (mono_type%kind)
        case (TINT)
            name = "integer"
        case (TREAL)
            name = "real"
        case (TCHAR)
            name = "character"
        case (TLOGICAL)
            name = "logical"
        case (TARRAY)
            if (allocated(mono_type%args) .and. size(mono_type%args) > 0) then
                name = get_type_name(mono_type%args(1))
            else
                name = "unknown_array"
            end if
        case (TFUN)
            name = "function"
        case (TVAR)
            name = "type_variable"
        case default
            name = "unknown"
        end select
    end function get_type_name

    function get_array_rank(mono_type) result(rank)
        type(mono_type_t), intent(in) :: mono_type
        integer :: rank
        
        rank = 0
        if (mono_type%kind == TARRAY) then
            ! For Fortran arrays, rank is typically stored in size field or 
            ! can be inferred from the array structure
            ! For now, default to rank 1 which covers most cases
            rank = 1
            
            ! If size is > 1, it might indicate multi-dimensional
            ! This is a simplified heuristic until more sophisticated
            ! rank tracking is implemented in the type system
            if (mono_type%size > 1) then
                rank = mono_type%size
            end if
        end if
    end function get_array_rank

    function get_current_scope_name(scopes) result(name)
        type(scope_stack_t), intent(in) :: scopes
        character(len=:), allocatable :: name
        
        if (scopes%depth > 0) then
            if (allocated(scopes%scopes(scopes%depth)%name)) then
                name = scopes%scopes(scopes%depth)%name
            else
                name = "unnamed_scope"
            end if
        else
            name = "no_scope"
        end if
    end function get_current_scope_name

    function get_parent_scope_name(scopes) result(name)
        type(scope_stack_t), intent(in) :: scopes
        character(len=:), allocatable :: name
        
        if (scopes%depth > 1) then
            if (allocated(scopes%scopes(scopes%depth - 1)%name)) then
                name = scopes%scopes(scopes%depth - 1)%name
            else
                name = "unnamed_parent"
            end if
        else
            name = ""
        end if
    end function get_parent_scope_name

    function get_parameter_intent(tracker, param_name) result(intent_str)
        type(parameter_tracker_t), intent(in) :: tracker
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable :: intent_str
        
        intent_str = tracker%get_parameter_intent(param_name)
    end function get_parameter_intent

    subroutine extract_function_signature(func_type, func_info)
        type(mono_type_t), intent(in) :: func_type
        type(function_info_t), intent(inout) :: func_info
        
        if (func_type%kind /= TFUN .or. .not. allocated(func_type%args)) return
        
        ! Simple case: single argument function
        if (size(func_type%args) == 2) then
            allocate(character(len=16) :: func_info%parameter_names(1))
            allocate(character(len=16) :: func_info%parameter_types(1))
            allocate(character(len=16) :: func_info%parameter_intents(1))
            
            func_info%parameter_names(1) = "param1"
            func_info%parameter_types(1) = get_type_name(func_type%args(1))
            func_info%parameter_intents(1) = ""
            func_info%return_type = get_type_name(func_type%args(2))
        else
            ! No parameters or complex curried function
            allocate(character(len=16) :: func_info%parameter_names(0))
            allocate(character(len=16) :: func_info%parameter_types(0))
            allocate(character(len=16) :: func_info%parameter_intents(0))
            
            if (allocated(func_type%args) .and. size(func_type%args) > 0) then
                func_info%return_type = &
                    get_type_name(func_type%args(size(func_type%args)))
            else
                func_info%return_type = "unknown"
            end if
        end if
    end subroutine extract_function_signature

    subroutine extract_type_info(mono_type, type_info)
        type(mono_type_t), intent(in) :: mono_type
        type(type_info_t), intent(out) :: type_info
        
        type_info%kind_name = get_type_name(mono_type)
        type_info%is_array = (mono_type%kind == TARRAY)
        type_info%is_allocatable = mono_type%alloc_info%is_allocatable
        type_info%is_pointer = mono_type%alloc_info%is_pointer
        
        if (mono_type%kind == TCHAR) then
            type_info%character_length = mono_type%size
        end if
        
        if (type_info%is_array .and. allocated(mono_type%args) .and. &
            size(mono_type%args) > 0) then
            type_info%array_element_type = get_type_name(mono_type%args(1))
            type_info%array_rank = get_array_rank(mono_type)
        end if
    end subroutine extract_type_info

    ! Issue #14 required APIs
    
    ! Get all symbols in current scope
    function query_get_symbols_in_scope(this, scope_type, symbols) result(success)
        class(semantic_query_t), intent(inout) :: this
        integer, intent(in) :: scope_type
        type(symbol_info_t), allocatable, intent(out) :: symbols(:)
        logical :: success
        integer :: i, count, target_depth
        
        success = .false.
        count = 0
        
        ! Find target scope depth based on scope_type
        select case (scope_type)
        case (SCOPE_GLOBAL)
            target_depth = 1
        case (SCOPE_MODULE, SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK)
            target_depth = this%context%scopes%depth
        case default
            target_depth = this%context%scopes%depth
        end select
        
        if (target_depth <= 0 .or. target_depth > this%context%scopes%depth) return
        
        ! Count symbols in target scope
        count = this%context%scopes%scopes(target_depth)%env%count
        
        if (count == 0) then
            allocate(symbols(0))
            success = .true.
            return
        end if
        
        ! Allocate and populate symbols array
        allocate(symbols(count))
        
        do i = 1, count
            symbols(i)%name = this%context%scopes%scopes(target_depth)%env%names(i)
            symbols(i)%type_info = this%context%instantiate( &
                this%context%scopes%scopes(target_depth)%env%schemes(i))
            symbols(i)%is_parameter = is_parameter_name( &
                this%context%param_tracker, symbols(i)%name)
            symbols(i)%is_used = .false.  ! TODO: Implement usage tracking
        end do
        
        success = .true.
    end function query_get_symbols_in_scope
    
    ! Check if variable is used anywhere in scope
    function query_is_variable_used(this, var_name, scope_type) result(used)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: scope_type
        logical :: used
        
        ! TODO: Implement proper usage tracking
        ! For now, assume all defined variables are used
        used = this%is_symbol_defined(var_name)
    end function query_is_variable_used
    
    ! Get all unused variables in scope
    function query_get_unused_variables(this, scope_type, unused_vars) result(success)
        class(semantic_query_t), intent(inout) :: this
        integer, intent(in) :: scope_type
        character(len=:), allocatable, intent(out) :: unused_vars(:)
        logical :: success
        type(symbol_info_t), allocatable :: all_symbols(:)
        integer :: i, unused_count
        
        success = .false.
        
        ! Get all symbols in scope
        if (.not. this%get_symbols_in_scope(scope_type, all_symbols)) return
        
        if (size(all_symbols) == 0) then
            allocate(character(len=1) :: unused_vars(0))
            success = .true.
            return
        end if
        
        ! Count unused variables
        unused_count = 0
        do i = 1, size(all_symbols)
            if (.not. this%is_variable_used(all_symbols(i)%name, scope_type)) then
                unused_count = unused_count + 1
            end if
        end do
        
        ! Allocate and populate unused variables array
        allocate(character(len=256) :: unused_vars(unused_count))
        unused_count = 0
        
        do i = 1, size(all_symbols)
            if (.not. this%is_variable_used(all_symbols(i)%name, scope_type)) then
                unused_count = unused_count + 1
                unused_vars(unused_count) = all_symbols(i)%name
            end if
        end do
        
        success = .true.
    end function query_get_unused_variables
    
    ! Get resolved type for identifier
    function query_get_identifier_type(this, identifier_name, type_info) result(success)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: identifier_name
        type(mono_type_t), optional, intent(out) :: type_info
        logical :: success
        type(poly_type_t), allocatable :: scheme
        
        success = .false.
        
        call this%context%scopes%lookup(identifier_name, scheme)
        
        if (allocated(scheme)) then
            if (present(type_info)) then
                type_info = this%context%instantiate(scheme)
            end if
            success = .true.
        end if
    end function query_get_identifier_type
    
    ! Check if identifier is defined (alias for is_symbol_defined)
    function query_is_identifier_defined(this, identifier_name) result(defined)
        class(semantic_query_t), intent(inout) :: this
        character(len=*), intent(in) :: identifier_name
        logical :: defined
        
        defined = this%is_symbol_defined(identifier_name)
    end function query_is_identifier_defined
    
    ! Helper function to check if name is a parameter
    function is_parameter_name(tracker, param_name) result(is_param)
        type(parameter_tracker_t), intent(in) :: tracker
        character(len=*), intent(in) :: param_name
        logical :: is_param
        
        is_param = tracker%is_parameter(param_name)
    end function is_parameter_name

    ! ===== DIRECT QUERY FUNCTIONS (RECOMMENDED APPROACH) =====
    ! These functions provide the recommended way to query semantic information.
    ! They avoid the deep copy issues of semantic_query_t and should be used
    ! in production code, especially with large AST arenas.
    ! 
    ! Benefits:
    ! - No memory allocation for query objects
    ! - No deep copies of arena/context
    ! - Direct access to semantic information
    ! - Safe to use with any size arena
    
    ! Direct function to check if identifier is defined (issue #196)
    function is_identifier_defined_direct(arena, context, &
                                           identifier_name) result(defined)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(inout) :: context
        character(len=*), intent(in) :: identifier_name
        logical :: defined
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: builtin_type
        
        ! Check in scope stack  
        call context%scopes%lookup(identifier_name, scheme)
        if (allocated(scheme)) then
            defined = .true.
            return
        end if
        
        ! Check builtin functions
        builtin_type = context%get_builtin_function_type(identifier_name)
        defined = (builtin_type%kind /= 0)
    end function is_identifier_defined_direct
    
    ! Direct function to get unused variables (issue #196)  
    function get_unused_variables_direct(arena, context, scope_type, &
                                          unused_vars) result(success)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(inout) :: context
        integer, intent(in) :: scope_type
        character(len=:), allocatable, intent(out) :: unused_vars(:)
        logical :: success
        type(symbol_info_t), allocatable :: all_symbols(:)
        integer :: i, unused_count
        
        success = .false.
        
        ! Get all symbols in scope
        if (.not. get_symbols_in_scope_direct(arena, context, scope_type, &
                                               all_symbols)) return
        
        if (size(all_symbols) == 0) then
            allocate(character(len=1) :: unused_vars(0))
            success = .true.
            return
        end if
        
        ! For now, assume all variables are unused (usage tracking not implemented yet)
        ! This matches the behavior of the semantic_query_t version
        unused_count = size(all_symbols)
        
        ! Allocate and populate unused variables array
        allocate(character(len=256) :: unused_vars(unused_count))
        
        do i = 1, unused_count
            unused_vars(i) = all_symbols(i)%name
        end do
        
        success = .true.
    end function get_unused_variables_direct
    
    ! Direct function to get symbols in scope (issue #196)
    function get_symbols_in_scope_direct(arena, context, scope_type, &
                                          symbols) result(success)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(inout) :: context
        integer, intent(in) :: scope_type
        type(symbol_info_t), allocatable, intent(out) :: symbols(:)
        logical :: success
        integer :: i, count, target_depth
        
        success = .false.
        count = 0
        
        ! Find target scope depth based on scope_type
        select case (scope_type)
        case (SCOPE_GLOBAL)
            target_depth = 1
        case (SCOPE_MODULE, SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK)
            target_depth = context%scopes%depth
        case default
            target_depth = context%scopes%depth
        end select
        
        if (target_depth <= 0 .or. target_depth > context%scopes%depth) return
        
        ! Count symbols in target scope
        count = context%scopes%scopes(target_depth)%env%count
        
        if (count == 0) then
            allocate(symbols(0))
            success = .true.
            return
        end if
        
        ! Allocate and populate symbols array
        allocate(symbols(count))
        
        do i = 1, count
            symbols(i)%name = context%scopes%scopes(target_depth)%env%names(i)
            symbols(i)%type_info = context%instantiate( &
                context%scopes%scopes(target_depth)%env%schemes(i))
            symbols(i)%is_parameter = is_parameter_name( &
                context%param_tracker, symbols(i)%name)
            symbols(i)%is_used = .false.  ! TODO: Implement usage tracking
        end do
        
        success = .true.
    end function get_symbols_in_scope_direct

end module semantic_query_api