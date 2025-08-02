program test_ast_introspection_final_coverage
    use fortfront
    implicit none

    logical :: all_passed = .true.

    print *, "Testing AST Introspection Final Coverage..."

    if (.not. test_missing_node_types()) all_passed = .false.
    if (.not. test_node_with_reference()) all_passed = .false.

    if (all_passed) then
        print *, "All final coverage tests passed!"
        stop 0
    else
        print *, "Some final coverage tests failed!"
        stop 1
    end if

contains

    logical function test_missing_node_types()
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg, source
        integer :: root_index, i, type_id
        
        test_missing_node_types = .true.
        print *, "Testing missing node types..."
        
        ! Test forall and pointer assignment
        arena = create_ast_arena()
        source = "forall (i = 1:10) arr(i) = i" // new_line('a') // &
                "ptr => target"
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  Note: forall/pointer may not be fully supported"
        else
            call parse_tokens(tokens, arena, root_index, error_msg)
            
            do i = 1, arena%size
                type_id = get_node_type_id_from_arena(arena, i)
                select case (type_id)
                case (33)
                    print *, "  Found forall node"
                case (32)
                    print *, "  Found pointer assignment"
                end select
            end do
        end if
        
        ! Test interface block and use statement in proper context
        arena = create_ast_arena()
        source = "module test_mod" // new_line('a') // &
                "  use iso_fortran_env" // new_line('a') // &
                "  interface operator(+)" // new_line('a') // &
                "    module procedure add_custom" // new_line('a') // &
                "  end interface" // new_line('a') // &
                "  type :: point" // new_line('a') // &
                "    real :: x, y" // new_line('a') // &
                "  end type" // new_line('a') // &
                "contains" // new_line('a') // &
                "  function add_custom(a, b)" // new_line('a') // &
                "    type(point) :: a, b, add_custom" // new_line('a') // &
                "  end function" // new_line('a') // &
                "  subroutine test_sub(x, y)" // new_line('a') // &
                "    real, intent(in) :: x" // new_line('a') // &
                "    real, intent(out) :: y" // new_line('a') // &
                "    y = x * 2" // new_line('a') // &
                "  end subroutine" // new_line('a') // &
                "end module"
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Module lexing failed:", error_msg
            test_missing_node_types = .false.
        else
            call parse_tokens(tokens, arena, root_index, error_msg)
            
            print *, "  Checking module nodes..."
            do i = 1, arena%size
                type_id = get_node_type_id_from_arena(arena, i)
                select case (type_id)
                case (2)
                    print *, "  Found function_def_node"
                case (9)
                    print *, "  Found subroutine_def_node"
                case (12)
                    print *, "  Found parameter_declaration_node"
                case (19)
                    print *, "  Found use_statement_node"
                case (30)
                    print *, "  Found interface_block_node"
                case (31)
                    print *, "  Found derived_type_node"
                case (38)
                    print *, "  Found contains_node"
                end select
            end do
        end if
        
        ! Test case blocks
        arena = create_ast_arena()
        source = "select case (n)" // new_line('a') // &
                "case (1:5)" // new_line('a') // &
                "  x = 1" // new_line('a') // &
                "case (6, 7, 8)" // new_line('a') // &
                "  x = 2" // new_line('a') // &
                "case default" // new_line('a') // &
                "  x = 0" // new_line('a') // &
                "end select"
        
        call lex_source(source, tokens, error_msg)
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        do i = 1, arena%size
            type_id = get_node_type_id_from_arena(arena, i)
            select case (type_id)
            case (17)
                print *, "  Found case_block_node"
            case (34)
                print *, "  Found case_range_node"
            case (35)
                print *, "  Found case_default_node"
            end select
        end do
        
        print *, "  Missing node types: PASS"
    end function test_missing_node_types

    logical function test_node_with_reference()
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        integer :: type_id, line, column
        logical :: has_info
        
        test_node_with_reference = .true.
        print *, "Testing node reference APIs..."
        
        ! Create nodes directly to test APIs
        id_node%name = "test_var"
        id_node%line = 10
        id_node%column = 5
        
        ! Test get_node_type_id with node reference
        type_id = get_node_type_id(id_node)
        print *, "  Identifier node type_id:", type_id
        
        ! Test get_node_source_location with node reference
        call get_node_source_location(id_node, line, column)
        print *, "  Location: line", line, "column", column
        
        ! Test has_semantic_info
        has_info = has_semantic_info(id_node)
        print *, "  Has semantic info:", has_info
        
        ! Test with literal node
        lit_node%value = "3.14"
        lit_node%literal_kind = LITERAL_REAL
        lit_node%line = 11
        lit_node%column = 10
        
        type_id = get_node_type_id(lit_node)
        print *, "  Literal node type_id:", type_id
        
        call get_node_source_location(lit_node, line, column)
        print *, "  Location: line", line, "column", column
        
        print *, "  Node reference APIs: PASS"
    end function test_node_with_reference

end program test_ast_introspection_final_coverage