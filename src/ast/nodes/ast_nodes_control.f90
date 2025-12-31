! Re-export module for backward compatibility
! This module aggregates all control flow AST nodes from specialized modules
module ast_nodes_control
    ! Import from specialized modules
    use ast_nodes_conditional, only: elseif_wrapper_t, case_wrapper_t, &
                                     if_node, select_case_node, &
                                     case_block_node, case_range_node, &
                                     case_default_node, create_if, &
                                     create_select_case, select_type_node, &
                                     type_guard_block_node, create_select_type, &
                                     select_rank_node, rank_block_node, &
                                     create_select_rank
    use ast_nodes_loops, only: MAX_INDEX_NAME_LENGTH, do_loop_node, &
                               do_while_node, forall_node, forall_triplet_t, &
                               create_do_loop, create_do_while
    use ast_nodes_array, only: where_node, where_stmt_node, elsewhere_clause_t
    use ast_nodes_transfer, only: cycle_node, exit_node, stop_node, &
                                  return_node, entry_node, goto_node, &
                                  error_stop_node, continue_node, pause_node, &
                                  nullify_node, create_continue
    use ast_nodes_associate, only: association_t, associate_node, &
                                   create_associate, block_construct_node, &
                                   create_block_construct
    implicit none

    ! Re-export constants
    public :: MAX_INDEX_NAME_LENGTH

    ! Re-export utility types
    public :: elseif_wrapper_t, case_wrapper_t, association_t
    public :: forall_triplet_t, elsewhere_clause_t

    ! Re-export all control flow node types
    public :: if_node, do_loop_node, do_while_node, forall_node
    public :: select_case_node, case_block_node, case_range_node, case_default_node
    public :: select_type_node, type_guard_block_node
    public :: select_rank_node, rank_block_node
    public :: where_node, where_stmt_node
    public :: cycle_node, exit_node, stop_node, return_node, entry_node, goto_node
    public :: error_stop_node, continue_node, associate_node, pause_node
    public :: nullify_node, block_construct_node

    ! Re-export factory functions
    public :: create_do_loop, create_do_while, create_if, create_select_case
    public :: create_select_type, create_select_rank, create_associate, create_continue
    public :: create_block_construct

end module ast_nodes_control
