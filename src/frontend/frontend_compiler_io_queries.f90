module frontend_compiler_io_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: io_specifier_t, print_statement_node, &
        write_statement_node, read_statement_node, format_statement_node, &
        open_statement_node, close_statement_node, inquire_statement_node, &
        backspace_statement_node, rewind_statement_node, endfile_statement_node
    implicit none
    private

    integer, parameter, public :: IO_STATEMENT_PRINT = 1
    integer, parameter, public :: IO_STATEMENT_WRITE = 2
    integer, parameter, public :: IO_STATEMENT_READ = 3
    integer, parameter, public :: IO_STATEMENT_FORMAT = 4
    integer, parameter, public :: IO_STATEMENT_OPEN = 5
    integer, parameter, public :: IO_STATEMENT_CLOSE = 6
    integer, parameter, public :: IO_STATEMENT_INQUIRE = 7
    integer, parameter, public :: IO_STATEMENT_BACKSPACE = 8
    integer, parameter, public :: IO_STATEMENT_REWIND = 9
    integer, parameter, public :: IO_STATEMENT_ENDFILE = 10

    type, public :: io_specifier_query_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: value
        logical :: has_value_node = .false.
        integer :: value_node_index = 0
    end type io_specifier_query_t

    type, public :: io_statement_query_t
        logical :: found = .false.
        integer :: statement_kind = 0
        integer :: line = 0
        integer :: column = 0
        logical :: has_statement_label = .false.
        character(len=:), allocatable :: statement_label
        type(io_specifier_query_t), allocatable :: specifiers(:)
        integer, allocatable :: item_node_indices(:)
        logical :: has_unit_spec = .false.
        character(len=:), allocatable :: unit_spec
        logical :: has_unit_node = .false.
        integer :: unit_node_index = 0
        logical :: has_format_spec = .false.
        character(len=:), allocatable :: format_spec
        logical :: has_file_spec = .false.
        character(len=:), allocatable :: file_spec
        logical :: has_status_spec = .false.
        character(len=:), allocatable :: status_spec
        logical :: has_access_spec = .false.
        character(len=:), allocatable :: access_spec
        logical :: has_form_spec = .false.
        character(len=:), allocatable :: form_spec
        logical :: has_recl_spec = .false.
        character(len=:), allocatable :: recl_spec
        logical :: has_blank_spec = .false.
        character(len=:), allocatable :: blank_spec
        logical :: has_position_spec = .false.
        character(len=:), allocatable :: position_spec
        logical :: has_action_spec = .false.
        character(len=:), allocatable :: action_spec
        logical :: has_delim_spec = .false.
        character(len=:), allocatable :: delim_spec
        logical :: has_pad_spec = .false.
        character(len=:), allocatable :: pad_spec
        logical :: has_namelist_group = .false.
        character(len=:), allocatable :: namelist_group
        character(len=:), allocatable :: specifier_list
        logical :: has_iostat_node = .false.
        integer :: iostat_node_index = 0
        logical :: has_err_label = .false.
        integer :: err_label_node_index = 0
        logical :: has_end_label = .false.
        integer :: end_label_node_index = 0
        logical :: has_format_node = .false.
        integer :: format_node_index = 0
        logical :: is_formatted = .false.
    end type io_statement_query_t

    public :: query_io_statement

contains

    function query_io_statement(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(io_statement_query_t) :: query

        call initialize_io_query(query)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (print_statement_node)
            call fill_print_query(node, query)
            type is (write_statement_node)
            call fill_write_query(node, query)
            type is (read_statement_node)
            call fill_read_query(node, query)
            type is (format_statement_node)
            call fill_format_query(node, query)
            type is (open_statement_node)
            call fill_open_query(node, query)
            type is (close_statement_node)
            call fill_close_query(node, query)
            type is (inquire_statement_node)
            call fill_inquire_query(node, query)
            type is (backspace_statement_node)
            call fill_position_query(node%unit_spec, node%specifiers, &
                IO_STATEMENT_BACKSPACE, query)
            type is (rewind_statement_node)
            call fill_position_query(node%unit_spec, node%specifiers, &
                IO_STATEMENT_REWIND, query)
            type is (endfile_statement_node)
            call fill_position_query(node%unit_spec, node%specifiers, &
                IO_STATEMENT_ENDFILE, query)
        end select
        if (.not. query%found) return
        query%line = arena%entries(node_index)%node%line
        query%column = arena%entries(node_index)%node%column
        call copy_statement_label(arena, node_index, query)
    end function query_io_statement

    subroutine initialize_io_query(query)
        type(io_statement_query_t), intent(out) :: query

        query%statement_label = ''
        query%unit_spec = ''
        query%format_spec = ''
        query%file_spec = ''
        query%status_spec = ''
        query%access_spec = ''
        query%form_spec = ''
        query%recl_spec = ''
        query%blank_spec = ''
        query%position_spec = ''
        query%action_spec = ''
        query%delim_spec = ''
        query%pad_spec = ''
        query%namelist_group = ''
        query%specifier_list = ''
        allocate (query%specifiers(0))
        allocate (query%item_node_indices(0))
    end subroutine initialize_io_query

    subroutine copy_statement_label(arena, node_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(io_statement_query_t), intent(inout) :: query

        if (.not. allocated(arena%entries(node_index)%node%stmt_label)) return
        query%statement_label = arena%entries(node_index)%node%stmt_label
        query%has_statement_label = len(query%statement_label) > 0
    end subroutine copy_statement_label

    subroutine fill_print_query(node, query)
        type(print_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_PRINT
        if (allocated(node%expression_indices)) then
            query%item_node_indices = node%expression_indices
        end if
        call copy_text(node%format_spec, query%format_spec, &
            query%has_format_spec)
    end subroutine fill_print_query

    subroutine fill_write_query(node, query)
        type(write_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_WRITE
        if (allocated(node%arg_indices)) query%item_node_indices = node%arg_indices
        call copy_common_transfer_io(node%unit_spec, node%format_spec, &
            node%namelist_group, node%specifiers, node%format_expr_index, query)
        query%is_formatted = node%is_formatted
    end subroutine fill_write_query

    subroutine fill_read_query(node, query)
        type(read_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_READ
        if (allocated(node%var_indices)) query%item_node_indices = node%var_indices
        call copy_common_transfer_io(node%unit_spec, node%format_spec, &
            node%namelist_group, node%specifiers, node%format_expr_index, query)
        query%is_formatted = node%is_formatted
    end subroutine fill_read_query

    subroutine fill_format_query(node, query)
        type(format_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_FORMAT
        call copy_text(node%format_spec, query%format_spec, &
            query%has_format_spec)
    end subroutine fill_format_query

    subroutine fill_open_query(node, query)
        type(open_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_OPEN
        call copy_io_specifiers(node%specifiers, query)
        call copy_index(node%iostat_var_index, query%iostat_node_index, &
            query%has_iostat_node)
        call copy_index(node%err_label_index, query%err_label_node_index, &
            query%has_err_label)
    end subroutine fill_open_query

    subroutine fill_close_query(node, query)
        type(close_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_CLOSE
        call copy_io_specifiers(node%specifiers, query)
        call copy_index(node%iostat_var_index, query%iostat_node_index, &
            query%has_iostat_node)
        call copy_index(node%err_label_index, query%err_label_node_index, &
            query%has_err_label)
    end subroutine fill_close_query

    subroutine fill_inquire_query(node, query)
        type(inquire_statement_node), intent(in) :: node
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = IO_STATEMENT_INQUIRE
        call copy_text_value(node%spec_list, query%specifier_list)
        call copy_io_specifiers(node%specifiers, query)
        call copy_index(node%iostat_var_index, query%iostat_node_index, &
            query%has_iostat_node)
        call copy_index(node%err_label_index, query%err_label_node_index, &
            query%has_err_label)
    end subroutine fill_inquire_query

    subroutine fill_position_query(raw_spec, specifiers, statement_kind, query)
        character(len=:), allocatable, intent(in) :: raw_spec
        type(io_specifier_t), allocatable, intent(in) :: specifiers(:)
        integer, intent(in) :: statement_kind
        type(io_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = statement_kind
        call copy_text_value(raw_spec, query%specifier_list)
        call copy_io_specifiers(specifiers, query)
    end subroutine fill_position_query

    subroutine copy_common_transfer_io(unit_spec, format_spec, namelist_group, &
            specifiers, format_node_index, query)
        character(len=:), allocatable, intent(in) :: unit_spec, format_spec
        character(len=:), allocatable, intent(in) :: namelist_group
        type(io_specifier_t), allocatable, intent(in) :: specifiers(:)
        integer, intent(in) :: format_node_index
        type(io_statement_query_t), intent(inout) :: query

        call copy_text(unit_spec, query%unit_spec, query%has_unit_spec)
        call copy_text(format_spec, query%format_spec, query%has_format_spec)
        call copy_text(namelist_group, query%namelist_group, &
            query%has_namelist_group)
        call copy_io_specifiers(specifiers, query)
        call copy_index(format_node_index, query%format_node_index, &
            query%has_format_node)
    end subroutine copy_common_transfer_io

    subroutine copy_io_specifiers(source, query)
        type(io_specifier_t), allocatable, intent(in) :: source(:)
        type(io_statement_query_t), intent(inout) :: query
        integer :: i

        if (.not. allocated(source)) return
        deallocate (query%specifiers)
        allocate (query%specifiers(size(source)))
        do i = 1, size(source)
            call copy_one_specifier(source(i), query%specifiers(i))
            call apply_named_specifier(source(i), query)
        end do
        call set_control_indices(source, query)
    end subroutine copy_io_specifiers

    subroutine copy_one_specifier(source, target)
        type(io_specifier_t), intent(in) :: source
        type(io_specifier_query_t), intent(out) :: target

        target%name = ''
        target%value = ''
        if (allocated(source%name)) target%name = source%name
        if (allocated(source%value)) target%value = source%value
        call copy_index(source%value_node_index, target%value_node_index, &
            target%has_value_node)
    end subroutine copy_one_specifier

    subroutine apply_named_specifier(specifier, query)
        type(io_specifier_t), intent(in) :: specifier
        type(io_statement_query_t), intent(inout) :: query

        if (.not. allocated(specifier%name)) return
        select case (specifier%name)
        case ('unit')
            call apply_unit_specifier(specifier, query)
        case ('fmt', 'format')
            call apply_text_specifier(specifier, query%format_spec, &
                query%has_format_spec)
        case ('file')
            call apply_text_specifier(specifier, query%file_spec, &
                query%has_file_spec)
        case ('status')
            call apply_text_specifier(specifier, query%status_spec, &
                query%has_status_spec)
        case ('access')
            call apply_text_specifier(specifier, query%access_spec, &
                query%has_access_spec)
        case ('form')
            call apply_text_specifier(specifier, query%form_spec, &
                query%has_form_spec)
        case ('recl')
            call apply_text_specifier(specifier, query%recl_spec, &
                query%has_recl_spec)
        case ('blank')
            call apply_text_specifier(specifier, query%blank_spec, &
                query%has_blank_spec)
        case ('position')
            call apply_text_specifier(specifier, query%position_spec, &
                query%has_position_spec)
        case ('action')
            call apply_text_specifier(specifier, query%action_spec, &
                query%has_action_spec)
        case ('delim')
            call apply_text_specifier(specifier, query%delim_spec, &
                query%has_delim_spec)
        case ('pad')
            call apply_text_specifier(specifier, query%pad_spec, &
                query%has_pad_spec)
        end select
    end subroutine apply_named_specifier

    subroutine apply_unit_specifier(specifier, query)
        type(io_specifier_t), intent(in) :: specifier
        type(io_statement_query_t), intent(inout) :: query

        call apply_text_specifier(specifier, query%unit_spec, &
            query%has_unit_spec)
        call copy_index(specifier%value_node_index, query%unit_node_index, &
            query%has_unit_node)
    end subroutine apply_unit_specifier

    subroutine apply_text_specifier(specifier, value, present_flag)
        type(io_specifier_t), intent(in) :: specifier
        character(len=:), allocatable, intent(inout) :: value
        logical, intent(inout) :: present_flag

        if (.not. allocated(specifier%value)) return
        value = specifier%value
        present_flag = .true.
    end subroutine apply_text_specifier

    subroutine set_control_indices(specifiers, query)
        type(io_specifier_t), intent(in) :: specifiers(:)
        type(io_statement_query_t), intent(inout) :: query
        integer :: i

        do i = 1, size(specifiers)
            if (.not. allocated(specifiers(i)%name)) cycle
            select case (specifiers(i)%name)
            case ('iostat')
                call copy_index(specifiers(i)%value_node_index, &
                    query%iostat_node_index, query%has_iostat_node)
            case ('err')
                call copy_index(specifiers(i)%value_node_index, &
                    query%err_label_node_index, query%has_err_label)
            case ('end')
                call copy_index(specifiers(i)%value_node_index, &
                    query%end_label_node_index, query%has_end_label)
            end select
        end do
    end subroutine set_control_indices

    subroutine copy_text(source, target, present_flag)
        character(len=:), allocatable, intent(in) :: source
        character(len=:), allocatable, intent(inout) :: target
        logical, intent(inout) :: present_flag

        if (.not. allocated(source)) return
        target = source
        present_flag = .true.
    end subroutine copy_text

    subroutine copy_text_value(source, target)
        character(len=:), allocatable, intent(in) :: source
        character(len=:), allocatable, intent(inout) :: target

        if (allocated(source)) target = source
    end subroutine copy_text_value

    subroutine copy_index(source, target, present_flag)
        integer, intent(in) :: source
        integer, intent(inout) :: target
        logical, intent(inout) :: present_flag

        if (source <= 0) return
        target = source
        present_flag = .true.
    end subroutine copy_index

end module frontend_compiler_io_queries
