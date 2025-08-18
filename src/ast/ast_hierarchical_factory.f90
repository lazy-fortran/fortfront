module ast_hierarchical_factory
    use ast_core
    use ast_node_registry, only: ast_node_registry_t, create_node_registry, registry_stats_t
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none
    private

    ! Hierarchical AST factory with distributed node management
    type, public :: hierarchical_factory_t
        type(ast_node_registry_t) :: registry    ! Node registry with ref counting
        integer :: root_node_id = 0             ! Root of current AST
        
        ! Performance optimization: node pools by type
        integer, allocatable :: identifier_pool(:)
        integer, allocatable :: literal_pool(:)
        integer, allocatable :: expression_pool(:)
        integer :: pool_sizes(3) = 0  ! Track pool usage
        
        ! Statistics tracking
        integer :: nodes_created = 0
        integer :: nodes_reused = 0
        integer :: gc_cycles = 0
    contains
        procedure :: create_program_node
        procedure :: create_assignment_node
        procedure :: create_binary_op_node
        procedure :: create_call_node
        procedure :: create_identifier_node
        procedure :: create_literal_node
        procedure :: create_array_literal_node
        procedure :: create_declaration_node
        procedure :: create_function_def_node
        procedure :: create_subroutine_def_node
        
        ! Node management
        procedure :: get_node_reference
        procedure :: release_node_reference
        procedure :: set_root_node
        procedure :: get_root_node
        
        ! Pool management for performance
        procedure :: get_pooled_node
        procedure :: return_to_pool
        procedure :: clear_pools
        
        ! Memory management
        procedure :: collect_garbage
        procedure :: get_memory_stats
        procedure :: optimize_memory
        
        ! Cleanup
        procedure :: clear_factory
        final :: cleanup_factory
    end type hierarchical_factory_t

    ! Factory statistics
    type, public :: factory_stats_t
        type(registry_stats_t) :: registry_stats
        integer :: nodes_created = 0
        integer :: nodes_reused = 0
        integer :: gc_cycles = 0
        integer :: pool_hits = 0
        real :: memory_efficiency = 0.0
    end type factory_stats_t

    public :: create_hierarchical_factory

contains

    ! Create new hierarchical factory
    function create_hierarchical_factory(initial_capacity) result(factory)
        integer, intent(in), optional :: initial_capacity
        type(hierarchical_factory_t) :: factory
        integer :: cap

        cap = 128  ! Reasonable default for hierarchical design
        if (present(initial_capacity)) then
            cap = max(initial_capacity, 32)
        end if

        factory%registry = create_node_registry(cap)
        
        ! Initialize node pools for performance
        allocate(factory%identifier_pool(16))
        allocate(factory%literal_pool(16))
        allocate(factory%expression_pool(16))
        factory%pool_sizes = 0
        
        factory%nodes_created = 0
        factory%nodes_reused = 0
        factory%gc_cycles = 0
    end function create_hierarchical_factory

    ! Create program node with hierarchical management
    function create_program_node(this, name, body_node_ids, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_node_ids(:)
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(program_node) :: prog

        prog = create_program(name, body_node_ids, line, column)
        
        ! Register with reference counting
        node_id = this%registry%register_node(prog, is_root=.true.)
        this%nodes_created = this%nodes_created + 1
        
        ! Track as potential root
        if (this%root_node_id == 0) then
            this%root_node_id = node_id
        end if
    end function create_program_node

    ! Create assignment node with hierarchical parent-child relationships
    function create_assignment_node(this, target_id, value_id, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: target_id, value_id
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(assignment_node) :: assign

        ! Validate child references exist
        block
            class(ast_node), allocatable :: target_node, value_node
            target_node = this%registry%get_node(target_id)
            value_node = this%registry%get_node(value_id)
            
            if (.not. allocated(target_node) .or. .not. allocated(value_node)) then
                ! Create error placeholder for invalid references
                block
                    type(literal_node) :: error_node
                    error_node = create_literal("!ERROR: Invalid child reference", LITERAL_STRING)
                    node_id = this%registry%register_node(error_node)
                    return
                end block
            end if
        end block

        assign = create_assignment(target_id, value_id, line, column)
        
        ! Register and add references to children
        node_id = this%registry%register_node(assign)
        call this%registry%add_reference(target_id)
        call this%registry%add_reference(value_id)
        
        this%nodes_created = this%nodes_created + 1
    end function create_assignment_node

    ! Create binary operation with proper reference management
    function create_binary_op_node(this, left_id, right_id, operator, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: left_id, right_id
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(binary_op_node) :: binop

        ! Validate operand references
        block
            class(ast_node), allocatable :: left_node, right_node
            left_node = this%registry%get_node(left_id)
            right_node = this%registry%get_node(right_id)
            
            if (.not. allocated(left_node) .or. .not. allocated(right_node)) then
                ! Create error placeholder
                block
                    type(literal_node) :: error_node
                    error_node = create_literal("!ERROR: Invalid operand reference", LITERAL_STRING)
                    node_id = this%registry%register_node(error_node)
                    return
                end block
            end if
        end block

        binop = create_binary_op(left_id, right_id, operator, line, column)
        
        ! Register with proper reference counting
        node_id = this%registry%register_node(binop)
        call this%registry%add_reference(left_id)
        call this%registry%add_reference(right_id)
        
        this%nodes_created = this%nodes_created + 1
    end function create_binary_op_node

    ! Create function call with argument validation
    function create_call_node(this, name, arg_ids, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: arg_ids(:)
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(call_or_subscript_node) :: call_node
        integer :: i

        ! Validate all argument references if provided
        if (present(arg_ids)) then
            do i = 1, size(arg_ids)
                block
                    class(ast_node), allocatable :: arg_node
                    arg_node = this%registry%get_node(arg_ids(i))
                    if (.not. allocated(arg_node)) then
                        ! Create error placeholder for invalid argument
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid argument reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        call_node = create_call_or_subscript(name, arg_ids, line, column)
        
        ! Register and add references to arguments
        node_id = this%registry%register_node(call_node)
        if (present(arg_ids)) then
            do i = 1, size(arg_ids)
                call this%registry%add_reference(arg_ids(i))
            end do
        end if
        
        this%nodes_created = this%nodes_created + 1
    end function create_call_node

    ! Create identifier with potential pooling
    function create_identifier_node(this, name, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(identifier_node) :: id

        ! Try to reuse from pool first
        node_id = this%get_pooled_node(1)  ! 1 = identifier pool
        if (node_id > 0) then
            this%nodes_reused = this%nodes_reused + 1
            return
        end if

        id = create_identifier(name, line, column)
        node_id = this%registry%register_node(id)
        this%nodes_created = this%nodes_created + 1
    end function create_identifier_node

    ! Create literal with potential pooling
    function create_literal_node(this, value, kind, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(literal_node) :: lit

        ! Try to reuse from pool first
        node_id = this%get_pooled_node(2)  ! 2 = literal pool
        if (node_id > 0) then
            this%nodes_reused = this%nodes_reused + 1
            return
        end if

        lit = create_literal(value, kind, line, column)
        node_id = this%registry%register_node(lit)
        this%nodes_created = this%nodes_created + 1
    end function create_literal_node

    ! Create array literal with proper element reference management
    function create_array_literal_node(this, element_ids, line, column, syntax_style) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: element_ids(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: syntax_style
        integer :: node_id
        type(array_literal_node) :: array_lit
        integer :: i

        ! Validate all element references
        do i = 1, size(element_ids)
            block
                class(ast_node), allocatable :: element_node
                element_node = this%registry%get_node(element_ids(i))
                if (.not. allocated(element_node)) then
                    ! Create error placeholder
                    block
                        type(literal_node) :: error_node
                        error_node = create_literal("!ERROR: Invalid element reference", LITERAL_STRING)
                        node_id = this%registry%register_node(error_node)
                        return
                    end block
                end if
            end block
        end do

        array_lit = create_array_literal(element_ids, line, column, syntax_style)
        
        ! Register and add references to all elements
        node_id = this%registry%register_node(array_lit)
        do i = 1, size(element_ids)
            call this%registry%add_reference(element_ids(i))
        end do
        
        this%nodes_created = this%nodes_created + 1
    end function create_array_literal_node

    ! Create declaration node with type validation
    function create_declaration_node(this, type_name, var_name, kind_value, &
         dimension_ids, initializer_id, is_allocatable, is_pointer, is_target, &
         intent_value, is_optional, is_parameter, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: type_name, var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_ids(:)
        integer, intent(in), optional :: initializer_id
        logical, intent(in), optional :: is_allocatable, is_pointer, is_target
        character(len=*), intent(in), optional :: intent_value
        logical, intent(in), optional :: is_optional, is_parameter
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(declaration_node) :: decl
        integer :: i

        ! Validate optional references
        if (present(initializer_id)) then
            if (initializer_id > 0) then
                block
                    class(ast_node), allocatable :: init_node
                    init_node = this%registry%get_node(initializer_id)
                    if (.not. allocated(init_node)) then
                        ! Create error placeholder
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid initializer reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end if
        end if

        ! Validate dimension references
        if (present(dimension_ids)) then
            do i = 1, size(dimension_ids)
                block
                    class(ast_node), allocatable :: dim_node
                    dim_node = this%registry%get_node(dimension_ids(i))
                    if (.not. allocated(dim_node)) then
                        ! Create error placeholder
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid dimension reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        ! Create declaration using factory function (implementation would be in ast_factory_core)
        decl%type_name = type_name
        decl%var_name = var_name
        
        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        end if
        
        if (present(initializer_id) .and. initializer_id > 0) then
            decl%initializer_index = initializer_id
            decl%has_initializer = .true.
        end if
        
        if (present(dimension_ids)) then
            decl%is_array = .true.
            allocate(decl%dimension_indices, source=dimension_ids)
        end if
        
        ! Set other attributes
        if (present(is_allocatable)) decl%is_allocatable = is_allocatable
        if (present(is_pointer)) decl%is_pointer = is_pointer
        if (present(is_target)) decl%is_target = is_target
        if (present(is_optional)) decl%is_optional = is_optional
        if (present(is_parameter)) decl%is_parameter = is_parameter
        
        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        end if
        
        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        ! Register with proper reference counting
        node_id = this%registry%register_node(decl)
        
        if (present(initializer_id) .and. initializer_id > 0) then
            call this%registry%add_reference(initializer_id)
        end if
        
        if (present(dimension_ids)) then
            do i = 1, size(dimension_ids)
                call this%registry%add_reference(dimension_ids(i))
            end do
        end if
        
        this%nodes_created = this%nodes_created + 1
    end function create_declaration_node

    ! Create function definition with proper body management
    function create_function_def_node(this, name, param_ids, return_type, body_ids, &
                                      line, column, result_variable) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_ids(:), body_ids(:)
        character(len=*), intent(in), optional :: return_type, result_variable
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(function_def_node) :: func_def
        integer :: i

        ! Validate parameter and body references
        if (present(param_ids)) then
            do i = 1, size(param_ids)
                block
                    class(ast_node), allocatable :: param_node
                    param_node = this%registry%get_node(param_ids(i))
                    if (.not. allocated(param_node)) then
                        ! Create error placeholder
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid parameter reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        if (present(body_ids)) then
            do i = 1, size(body_ids)
                block
                    class(ast_node), allocatable :: body_node
                    body_node = this%registry%get_node(body_ids(i))
                    if (.not. allocated(body_node)) then
                        ! Create error placeholder
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid body reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        func_def = create_function_def(name, param_ids, return_type, body_ids, &
                                       line, column, result_variable)
        
        ! Register with proper reference counting
        node_id = this%registry%register_node(func_def)
        
        if (present(param_ids)) then
            do i = 1, size(param_ids)
                call this%registry%add_reference(param_ids(i))
            end do
        end if
        
        if (present(body_ids)) then
            do i = 1, size(body_ids)
                call this%registry%add_reference(body_ids(i))
            end do
        end if
        
        this%nodes_created = this%nodes_created + 1
    end function create_function_def_node

    ! Create subroutine definition with proper body management
    function create_subroutine_def_node(this, name, param_ids, body_ids, line, column) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_ids(:), body_ids(:)
        integer, intent(in), optional :: line, column
        integer :: node_id
        type(subroutine_def_node) :: sub_def
        integer :: i

        ! Validate references (similar to function_def)
        if (present(param_ids)) then
            do i = 1, size(param_ids)
                block
                    class(ast_node), allocatable :: param_node
                    param_node = this%registry%get_node(param_ids(i))
                    if (.not. allocated(param_node)) then
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid parameter reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        if (present(body_ids)) then
            do i = 1, size(body_ids)
                block
                    class(ast_node), allocatable :: body_node
                    body_node = this%registry%get_node(body_ids(i))
                    if (.not. allocated(body_node)) then
                        block
                            type(literal_node) :: error_node
                            error_node = create_literal("!ERROR: Invalid body reference", LITERAL_STRING)
                            node_id = this%registry%register_node(error_node)
                            return
                        end block
                    end if
                end block
            end do
        end if

        sub_def = create_subroutine_def(name, param_ids, body_ids, line, column)
        
        ! Register with proper reference counting
        node_id = this%registry%register_node(sub_def)
        
        if (present(param_ids)) then
            do i = 1, size(param_ids)
                call this%registry%add_reference(param_ids(i))
            end do
        end if
        
        if (present(body_ids)) then
            do i = 1, size(body_ids)
                call this%registry%add_reference(body_ids(i))
            end do
        end if
        
        this%nodes_created = this%nodes_created + 1
    end function create_subroutine_def_node

    ! Node reference management
    function get_node_reference(this, node_id) result(node)
        class(hierarchical_factory_t), intent(in) :: this
        integer, intent(in) :: node_id
        class(ast_node), allocatable :: node
        
        node = this%registry%get_node(node_id)
    end function get_node_reference

    subroutine release_node_reference(this, node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: node_id
        
        call this%registry%remove_reference(node_id)
    end subroutine release_node_reference

    subroutine set_root_node(this, node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: node_id
        
        this%root_node_id = node_id
        call this%registry%add_reference(node_id)
    end subroutine set_root_node

    function get_root_node(this) result(node)
        class(hierarchical_factory_t), intent(in) :: this
        class(ast_node), allocatable :: node
        
        if (this%root_node_id > 0) then
            node = this%registry%get_node(this%root_node_id)
        end if
    end function get_root_node

    ! Pool management for performance optimization
    function get_pooled_node(this, pool_type) result(node_id)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: pool_type  ! 1=identifier, 2=literal, 3=expression
        integer :: node_id
        
        node_id = 0  ! No pooling in this simplified implementation
        ! In a full implementation, would check pools and reuse nodes
    end function get_pooled_node

    subroutine return_to_pool(this, node_id, pool_type)
        class(hierarchical_factory_t), intent(inout) :: this
        integer, intent(in) :: node_id, pool_type
        
        ! In a full implementation, would return unused nodes to pools
        ! For now, just release the reference
        call this%registry%remove_reference(node_id)
    end subroutine return_to_pool

    subroutine clear_pools(this)
        class(hierarchical_factory_t), intent(inout) :: this
        
        this%pool_sizes = 0
        ! In a full implementation, would clear all pools
    end subroutine clear_pools

    ! Memory management
    subroutine collect_garbage(this)
        class(hierarchical_factory_t), intent(inout) :: this
        
        call this%registry%collect_garbage()
        this%gc_cycles = this%gc_cycles + 1
    end subroutine collect_garbage

    function get_memory_stats(this) result(stats)
        class(hierarchical_factory_t), intent(in) :: this
        type(factory_stats_t) :: stats
        
        stats%registry_stats = this%registry%get_stats()
        stats%nodes_created = this%nodes_created
        stats%nodes_reused = this%nodes_reused
        stats%gc_cycles = this%gc_cycles
        stats%pool_hits = 0  ! Would track pool reuse
        
        if (this%nodes_created > 0) then
            stats%memory_efficiency = real(this%nodes_reused) / real(this%nodes_created)
        else
            stats%memory_efficiency = 0.0
        end if
    end function get_memory_stats

    subroutine optimize_memory(this)
        class(hierarchical_factory_t), intent(inout) :: this
        
        ! Trigger garbage collection
        call this%collect_garbage()
        
        ! Clear pools to free memory
        call this%clear_pools()
    end subroutine optimize_memory

    ! Cleanup
    subroutine clear_factory(this)
        class(hierarchical_factory_t), intent(inout) :: this
        
        call this%registry%clear_registry()
        call this%clear_pools()
        
        this%root_node_id = 0
        this%nodes_created = 0
        this%nodes_reused = 0
        this%gc_cycles = 0
    end subroutine clear_factory

    subroutine cleanup_factory(this)
        type(hierarchical_factory_t), intent(inout) :: this
        
        call this%clear_factory()
        
        if (allocated(this%identifier_pool)) deallocate(this%identifier_pool)
        if (allocated(this%literal_pool)) deallocate(this%literal_pool)
        if (allocated(this%expression_pool)) deallocate(this%expression_pool)
    end subroutine cleanup_factory

end module ast_hierarchical_factory