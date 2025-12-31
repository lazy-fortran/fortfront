submodule(semantic_analyzer) semantic_analyzer_infer_impl
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   allocation_info_t, create_mono_type, &
                                   create_fun_type, create_poly_type, &
                                   create_type_var, TREAL, TVAR, TCOMPLEX, &
                                   TFUN, TARRAY, TLOGICAL, TINT, TCHAR
    use semantic_type_operations, only: get_common_type
    use ast_nodes_misc, only: data_statement_node
    use semantic_explicit_interface_checker, only: &
        validate_explicit_interface_for_function_reference, &
        validate_explicit_interface_for_subroutine_call
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use semantic_type_lookup_wrapper, only: set_semantic_context_for_lookup, &
                                            clear_semantic_context_for_lookup, &
                                            get_type_with_stored_context
    use semantic_where_validation, only: validate_where_construct
    implicit none

! Module-level context for wrapper function
! (not thread-safe, but works around gfortran bug)
    class(semantic_context_t), pointer :: g_current_context => null()
    type(ast_arena_t), pointer :: g_current_arena => null()

    type :: infer_frame_t
        integer :: node_index = 0
        integer :: state = 0
        integer :: aux_index = 0
        logical :: leave_scope = .false.
        logical :: has_cached_type = .false.
        type(mono_type_t) :: cached_type
        type(mono_type_t), allocatable :: param_types(:)
    end type infer_frame_t

contains

    include 'semantic_analyzer_infer_impl_part1.inc'
    include 'semantic_analyzer_infer_impl_part2.inc'
    include 'semantic_analyzer_infer_impl_part3.inc'

end submodule semantic_analyzer_infer_impl
