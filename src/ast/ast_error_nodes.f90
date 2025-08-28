module ast_error_nodes
    use json_module
    use ast_base, only: ast_node, ast_visitor_base_t
    implicit none
    private
    
    public :: error_node_t, create_error_node
    
    ! Error node for unknown AST node types - safer than source= allocation  
    type, extends(ast_node), public :: error_node_t
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: original_data
        integer :: error_code = -1
    contains
        procedure :: to_json => error_node_to_json
        procedure :: accept => error_node_accept
        procedure :: cleanup => error_node_cleanup
    end type
    
contains

    ! Factory function for creating error nodes
    function create_error_node(message, original_data, line, column) result(node)
        character(*), intent(in) :: message
        character(*), intent(in), optional :: original_data
        integer, intent(in), optional :: line, column
        type(error_node_t) :: node
        
        node%error_message = message
        if (present(original_data)) then
            node%original_data = original_data
        else
            node%original_data = "unknown"
        end if
        node%error_code = -1
        
        ! Set location in inherited fields
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function

    ! JSON representation for error nodes (implements to_json_interface)
    subroutine error_node_to_json(this, json, parent)
        class(error_node_t), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        
        call json%add(parent, 'type', 'error_node')
        call json%add(parent, 'error', this%error_message)
        call json%add(parent, 'data', this%original_data)
        call json%add(parent, 'error_code', this%error_code)
        call json%add(parent, 'line', this%line)
        call json%add(parent, 'column', this%column)
    end subroutine

    ! Visitor pattern support for error nodes (implements visit_interface)
    subroutine error_node_accept(this, visitor)
        class(error_node_t), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Error nodes don't participate in normal AST visitation
        ! They represent failure states that should be handled separately
    end subroutine

    ! Cleanup for error nodes
    subroutine error_node_cleanup(this)
        class(error_node_t), intent(inout) :: this
        if (allocated(this%error_message)) deallocate(this%error_message)
        if (allocated(this%original_data)) deallocate(this%original_data)
        this%error_code = 0
    end subroutine

end module ast_error_nodes