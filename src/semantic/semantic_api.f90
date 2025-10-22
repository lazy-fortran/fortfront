module semantic_api
    ! Public semantic analysis API for library consumers
    ! Provides type inference and semantic validation
    use semantic_analyzer, only: &
        semantic_context_t, &
        create_semantic_context, &
        analyze_program, &
        has_semantic_errors
    use type_system_unified, only: &
        mono_type_t, &
        poly_type_t, &
        type_env_t, &
        type_var_t, &
        substitution_t, &
        create_mono_type, &
        create_poly_type, &
        create_type_var, &
        TVAR, &
        TINT, &
        TREAL, &
        TCHAR, &
        TLOGICAL, &
        TFUN, &
        TARRAY, &
        TCOMPLEX, &
        TDOUBLE, &
        TDERIVED
    use scope_manager, only: &
        scope_stack_t, &
        create_scope_stack, &
        push_scope, &
        pop_scope
    use error_handling, only: &
        error_collection_t, &
        result_t

    implicit none
    private

    ! Main semantic context type
    public :: semantic_context_t

    ! Context management
    public :: create_semantic_context
    public :: analyze_program
    public :: has_semantic_errors

    ! Type system types
    public :: mono_type_t
    public :: poly_type_t
    public :: type_env_t
    public :: type_var_t
    public :: substitution_t

    ! Type constructors
    public :: create_mono_type
    public :: create_poly_type
    public :: create_type_var

    ! Type constants
    public :: TVAR
    public :: TINT
    public :: TREAL
    public :: TCHAR
    public :: TLOGICAL
    public :: TFUN
    public :: TARRAY
    public :: TCOMPLEX
    public :: TDOUBLE
    public :: TDERIVED

    ! Scope management
    public :: scope_stack_t
    public :: create_scope_stack
    public :: push_scope
    public :: pop_scope

    ! Error handling
    public :: error_collection_t
    public :: result_t

end module semantic_api
