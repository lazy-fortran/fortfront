module ast_json
    ! This module re-exports the JSON functionality from ast_core
    ! All JSON serialization is now handled directly by the AST nodes in ast_core
    use ast_core
    use json_module
    implicit none
    
    ! Re-export the ast_to_json_string function for convenience
    public :: ast_to_json_string

end module ast_json