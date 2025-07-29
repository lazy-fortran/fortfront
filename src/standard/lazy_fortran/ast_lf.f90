module ast_lazy_fortran
    use ast_core
    use json_module
    implicit none
    private

    ! Extended program node with implicit program support
    type, extends(program_node), public :: lf_program_node
        logical :: implicit = .false.      ! true if auto-wrapped
        logical :: auto_contains = .false. ! true if contains was auto-inserted
    contains
        procedure :: accept => lf_program_accept
        procedure :: to_json => lf_program_to_json
    end type lf_program_node

    ! Type-inferred variable node (unique to Simple Fortran)
    type, extends(ast_node), public :: inferred_var_node
        character(len=:), allocatable :: name
        class(ast_node), allocatable :: initial_value
        ! Type will be inferred during semantic analysis
    contains
        procedure :: accept => inferred_var_accept
        procedure :: to_json => inferred_var_to_json
    end type inferred_var_node

    ! List comprehension node (future Python-like feature)
    type, extends(ast_node), public :: list_comp_node
        class(ast_node), allocatable :: expr
        class(ast_node), allocatable :: target
        class(ast_node), allocatable :: iter
        class(ast_node), allocatable :: condition  ! optional
    contains
        procedure :: accept => list_comp_accept
        procedure :: to_json => list_comp_to_json
    end type list_comp_node

    ! F-string node (future Python-like feature)
    type, extends(ast_node), public :: fstring_node
        character(len=:), allocatable :: template
        type(ast_node_wrapper), allocatable :: expressions(:)
    contains
        procedure :: accept => fstring_accept
        procedure :: to_json => fstring_to_json
    end type fstring_node

    ! Note: lf_assignment_node removed - type inference now in core assignment_node

    ! Public interface for creating Simple Fortran nodes
    public :: create_lf_program, create_inferred_var, create_list_comp
    public :: create_fstring
    ! Note: create_lf_assignment removed - use core create_assignment

contains

    ! Factory functions for Simple Fortran AST nodes

    function create_lf_program(name, body, implicit, auto_contains, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: body(:)
        logical, intent(in), optional :: implicit, auto_contains
        integer, intent(in), optional :: line, column
        type(lf_program_node) :: node
        integer :: i

        node%name = name
        ! Arena-based body allocation disabled - use standard AST nodes instead
        if (present(implicit)) node%implicit = implicit
        if (present(auto_contains)) node%auto_contains = auto_contains
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_lf_program

    function create_inferred_var(name, initial_value, line, column) result(node)
        character(len=*), intent(in) :: name
        class(ast_node), intent(in) :: initial_value
        integer, intent(in), optional :: line, column
        type(inferred_var_node) :: node

        node%name = name
        allocate (node%initial_value, source=initial_value)
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_inferred_var

    function create_list_comp(expr, target, iter, condition, line, column) result(node)
        class(ast_node), intent(in) :: expr
        class(ast_node), intent(in) :: target
        class(ast_node), intent(in) :: iter
        class(ast_node), intent(in), optional :: condition
        integer, intent(in), optional :: line, column
        type(list_comp_node) :: node

        allocate (node%expr, source=expr)
        allocate (node%target, source=target)
        allocate (node%iter, source=iter)
        if (present(condition)) allocate (node%condition, source=condition)
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_list_comp

    function create_fstring(template, expressions, line, column) result(node)
        character(len=*), intent(in) :: template
        class(ast_node), intent(in) :: expressions(:)
        integer, intent(in), optional :: line, column
        type(fstring_node) :: node
        integer :: i
        type(ast_node_wrapper) :: temp_wrapper

        node%template = template
        if (size(expressions) > 0) then
            allocate (node%expressions(0))
            do i = 1, size(expressions)
                allocate (temp_wrapper%node, source=expressions(i))
                node%expressions = [node%expressions, temp_wrapper]
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_fstring

    ! create_lf_assignment removed - use core create_assignment which now has type inference

    ! Visitor pattern implementations for Simple Fortran nodes

    subroutine lf_program_accept(this, visitor)
        class(lf_program_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implement visitor pattern for lazy Fortran program nodes
        ! This would be extended to handle specific visitor types
        ! For now, basic implementation
    end subroutine lf_program_accept

    subroutine inferred_var_accept(this, visitor)
        class(inferred_var_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implement visitor pattern for type-inferred variable nodes
        ! This would be extended to handle specific visitor types
        ! For now, basic implementation
    end subroutine inferred_var_accept

    subroutine list_comp_accept(this, visitor)
        class(list_comp_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implement visitor pattern for list comprehension nodes
        ! This would be extended to handle specific visitor types
        ! For now, basic implementation
    end subroutine list_comp_accept

    subroutine fstring_accept(this, visitor)
        class(fstring_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Implement visitor pattern for F-string nodes
        ! This would be extended to handle specific visitor types
        ! For now, basic implementation
    end subroutine fstring_accept

    ! lf_assignment_accept removed - core assignment_accept handles type inference

    ! JSON serialization implementations for Simple Fortran nodes

    subroutine lf_program_to_json(this, json, parent)
        class(lf_program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'lf_program')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'implicit', this%implicit)
        call json%add(obj, 'auto_contains', this%auto_contains)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)

        ! Arena-based JSON serialization disabled - empty body for now

        call json%add(parent, obj)
    end subroutine lf_program_to_json

    subroutine inferred_var_to_json(this, json, parent)
        class(inferred_var_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'inferred_var')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call this%initial_value%to_json(json, obj)

        call json%add(parent, obj)
    end subroutine inferred_var_to_json

    subroutine list_comp_to_json(this, json, parent)
        class(list_comp_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'list_comp')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call this%expr%to_json(json, obj)
        call this%target%to_json(json, obj)
        call this%iter%to_json(json, obj)

        if (allocated(this%condition)) then
            call this%condition%to_json(json, obj)
        end if

        call json%add(parent, obj)
    end subroutine list_comp_to_json

    subroutine fstring_to_json(this, json, parent)
        class(fstring_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, expr_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'fstring')
        call json%add(obj, 'template', this%template)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(expr_array, 'expressions')
        call json%add(obj, expr_array)

        do i = 1, size(this%expressions)
            call this%expressions(i)%node%to_json(json, expr_array)
        end do

        call json%add(parent, obj)
    end subroutine fstring_to_json

    ! lf_assignment_to_json removed - core assignment_to_json now handles type inference

end module ast_lazy_fortran
