module ast_error_nodes
    use ast_base, only: ast_node
    implicit none
    private
    
    public :: error_node_t, create_error_node
    
    ! Error node for unknown AST node types - safer than source= allocation  
    type, extends(ast_node), public :: error_node_t
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: original_data
        integer :: error_code = -1
        integer :: line = 0
        integer :: column = 0
    contains
        procedure :: to_json => error_node_to_json
        procedure :: accept_visitor => error_node_accept_visitor
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
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function

    ! JSON representation for error nodes
    function error_node_to_json(this) result(json_str)
        class(error_node_t), intent(in) :: this
        character(len=:), allocatable :: json_str
        character(len=32) :: line_str, col_str
        
        write(line_str, '(I0)') this%line
        write(col_str, '(I0)') this%column
        
        json_str = '{"type": "error_node"' // &
                   ', "error": "' // this%error_message // '"' // &
                   ', "data": "' // this%original_data // '"' // &
                   ', "line": ' // trim(line_str) // &
                   ', "column": ' // trim(col_str) // '}'
    end function

    ! Visitor pattern support for error nodes
    subroutine error_node_accept_visitor(this, visitor)
        class(error_node_t), intent(in) :: this
        class(*), intent(inout) :: visitor
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