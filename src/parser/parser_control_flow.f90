module parser_control_flow_module
    ! Wrapper module that re-exports control flow parsing functionality
    ! from specialized modules to maintain API compatibility
    
    ! Re-export IF constructs
    use parser_if_constructs_module, only: &
        parse_if, parse_if_condition, parse_if_body, parse_elseif_block
    
    ! Re-export DO constructs  
    use parser_do_constructs_module, only: &
        parse_do_loop, parse_do_while, parse_do_while_from_do
    
    ! Re-export SELECT constructs
    use parser_select_constructs_module, only: &
        parse_select_case
    
    ! Re-export WHERE/ASSOCIATE constructs
    use parser_array_constructs_module, only: &
        parse_where_construct, parse_associate
    
    ! Re-export basic statement parsing
    use parser_basic_statement_module, only: &
        parse_basic_statement_multi, parse_statement_body
    
    implicit none
    private

    ! Re-export all public interfaces to maintain compatibility
    public :: parse_if, parse_do_loop, parse_do_while, parse_select_case
    public :: parse_if_condition, parse_if_body, parse_elseif_block
    public :: parse_do_while_from_do, parse_basic_statement_multi, parse_statement_body
    public :: parse_where_construct, parse_associate

end module parser_control_flow_module