program test_derived_type_component_scope
    use lexer_api, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: derived_type_node, declaration_node
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    integer :: i
    integer :: derived_index
    integer :: comp_index
    integer, allocatable :: component_list(:)
    logical :: found_x
    logical :: found_y
    logical :: found_p
    logical :: test_passed

    source = "program attr_type_test" // new_line('a') // &
        "    implicit none" // new_line('a') // new_line('a') // &
        "    type :: point_t" // new_line('a') // &
        "        real :: x" // new_line('a') // &
        "        real :: y" // new_line('a') // &
        "    end type point_t" // new_line('a') // new_line('a') // &
        "    type(point_t) :: p" // new_line('a') // new_line('a') // &
        "    p%x = 1.0" // new_line('a') // &
        "    p%y = 2.0" // new_line('a') // new_line('a') // &
        "    print *, p%x, p%y" // new_line('a') // &
        "end program attr_type_test"

    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: lexer error:", trim(error_msg)
        stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: parser error:", trim(error_msg)
        stop 1
    end if

    derived_index = 0
    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (node => arena%entries(i)%node)
            type is (derived_type_node)
            if (allocated(node%name)) then
                if (trim(node%name) == "point_t") then
                    derived_index = i
                    exit
                end if
            end if
        end select
    end do

    if (derived_index <= 0) then
        print *, "FAIL: derived type node not found in arena"
        stop 1
    end if

    found_x = .false.
    found_y = .false.
    found_p = .false.
    test_passed = .true.

    if (allocated(component_list)) deallocate (component_list)

    select type (dtype => arena%entries(derived_index)%node)
        type is (derived_type_node)
        if (allocated(dtype%component_indices)) then
            component_list = dtype%component_indices
        else
            allocate (component_list(0))
        end if

        if (size(component_list) /= 2) then
            print *, "FAIL: expected two components inside derived type, got", &
                size(component_list)
            test_passed = .false.
        end if

        do i = 1, size(component_list)
            comp_index = component_list(i)
            if (comp_index <= 0 .or. comp_index > arena%size) then
                print *, "FAIL: component index out of bounds:", comp_index
                test_passed = .false.
                cycle
            end if

            if (.not. allocated(arena%entries(comp_index)%node)) then
                print *, "FAIL: component node missing at index", comp_index
                test_passed = .false.
                cycle
            end if

            select type (decl => arena%entries(comp_index)%node)
                type is (declaration_node)
                if (.not. allocated(decl%var_name)) then
                    print *, "FAIL: component missing variable name"
                    test_passed = .false.
                else
                    select case (trim(decl%var_name))
                    case ("x")
                        found_x = .true.
                        if (trim(decl%type_name) /= "real") then
                            print *, "FAIL: component x type mismatch:", &
                                trim(decl%type_name)
                            test_passed = .false.
                        end if
                    case ("y")
                        found_y = .true.
                        if (trim(decl%type_name) /= "real") then
                            print *, "FAIL: component y type mismatch:", &
                                trim(decl%type_name)
                            test_passed = .false.
                        end if
                    case default
                        print *, "FAIL: unexpected declaration inside type:", &
                            trim(decl%var_name)
                        test_passed = .false.
                    end select
                end if
            class default
                print *, "FAIL: non-declaration node found in components"
                test_passed = .false.
            end select
        end do
    class default
        print *, "FAIL: derived index does not reference derived_type_node"
        stop 1
    end select

    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (decl => arena%entries(i)%node)
            type is (declaration_node)
            if (.not. allocated(decl%var_name)) cycle
            if (trim(decl%var_name) == "p") then
                found_p = .true.
                if (allocated(component_list)) then
                    if (any(component_list == i)) then
                        print *, "FAIL: top-level variable captured as component"
                        test_passed = .false.
                    end if
                end if
            end if
        end select
    end do

    if (.not. found_x) then
        print *, "FAIL: component x not registered in derived type"
        test_passed = .false.
    end if

    if (.not. found_y) then
        print *, "FAIL: component y not registered in derived type"
        test_passed = .false.
    end if

    if (.not. found_p) then
        print *, "FAIL: top-level declaration for p not found"
        test_passed = .false.
    end if

    if (test_passed) then
        print *, "PASS: derived type components scoped correctly"
        stop 0
    else
        stop 1
    end if
end program test_derived_type_component_scope
