submodule(semantic_analyzer) semantic_analyzer_infer_impl
use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
    allocation_info_t, create_mono_type, &
    create_fun_type, create_poly_type, &
    create_type_var, TREAL, TVAR, TCOMPLEX, &
    TFUN, TARRAY, TLOGICAL, TINT, TCHAR
use error_handling, only: create_error_result, ERROR_SEMANTIC
use semantic_type_operations, only: get_common_type
use ast_nodes_misc, only: data_statement_node
use semantic_explicit_interface_checker, only: &
    build_explicit_interface_name_cache, &
    validate_explicit_interface_for_function_reference, &
    validate_explicit_interface_for_subroutine_call
use semantic_strict_argument_type_checker, only: &
    validate_strict_argument_types_for_function_reference, &
    validate_strict_argument_types_for_subroutine_call
use semantic_strict_argument_type_checker_validation, only: &
    validate_value_dummy_attributes
use semantic_operating_mode, only: OPERATING_MODE_STRICT
use semantic_input_mode, only: INPUT_MODE_STANDARD
use semantic_identifier_context, only: find_program_owner
use semantic_where_validation, only: validate_where_construct
use semantic_pure_validation, only: validate_pure_procedure, &
    validate_do_concurrent_purity
use semantic_bind_c_validation, only: validate_bind_c_derived_type, &
    validate_bind_c_procedure
use semantic_elemental_validation, only: validate_elemental_procedure
use semantic_abstract_validation, only: validate_abstract_extension
use semantic_implied_do_validation, only: validate_implied_do_array
use string_utils_mod, only: int_to_string
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

integer, parameter :: UNKNOWN_STATIC_SIZE = -1
! Static size helpers return UNKNOWN_STATIC_SIZE when a size cannot be
! determined. Treat any negative value as unknown and compare with < 0;
! any value >= 0 is a known size, where 0 means known empty.

contains

    include 'semantic_analyzer_infer_impl_part1.inc'
    include 'semantic_analyzer_infer_impl_part2.inc'
    include 'semantic_analyzer_infer_impl_part3.inc'

end submodule semantic_analyzer_infer_impl
