module ast_factory_control
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_factory_core, only: validate_arena, validate_node_index
    use ast_nodes_control, only: MAX_INDEX_NAME_LENGTH, if_node, select_case_node, &
                                 case_block_node, case_range_node, case_default_node, &
                                 select_type_node, type_guard_block_node, &
                                 select_rank_node, rank_block_node, &
                                 where_node, elsewhere_clause_t, associate_node, &
                                 block_construct_node
    use ast_nodes_loops, only: do_loop_node, do_while_node, forall_node
    use uid_generator, only: generate_uid
    use error_handling, only: result_t, success_result, create_error_result
    implicit none
    private

    ! Public control flow node creation functions
    public :: push_if, push_do_loop, push_do_while, push_forall, push_select_case
    public :: push_associate, push_block_construct
    public :: push_case_block, push_case_range, push_case_default, &
              push_select_case_with_default
    public :: push_select_type, push_select_type_with_default, push_type_guard_block
    public :: push_select_rank, push_select_rank_with_default, push_rank_block
    public :: push_where, push_where_construct, push_where_construct_with_elsewhere

contains

    include 'ast_factory_control_part1.inc'
    include 'ast_factory_control_part2.inc'

end module ast_factory_control
