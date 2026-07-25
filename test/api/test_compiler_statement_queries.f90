program test_compiler_statement_queries
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: ast_arena_t, compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, io_statement_query_t, control_statement_query_t, &
        branch_statement_query_t, query_io_statement, query_control_statement, &
        query_branch_statement, IO_STATEMENT_PRINT, IO_STATEMENT_WRITE, &
        IO_STATEMENT_READ, IO_STATEMENT_FORMAT, IO_STATEMENT_OPEN, &
        IO_STATEMENT_CLOSE, IO_STATEMENT_INQUIRE, IO_STATEMENT_BACKSPACE, &
        IO_STATEMENT_REWIND, IO_STATEMENT_ENDFILE, CONTROL_ASSOCIATE, &
        CONTROL_BLOCK, CONTROL_SELECT_TYPE, CONTROL_TYPE_GUARD, &
        CONTROL_SELECT_RANK, CONTROL_RANK_BLOCK, CONTROL_WHERE, &
        CONTROL_WHERE_STATEMENT, BRANCH_GOTO, BRANCH_PAUSE, BRANCH_CONTINUE
    implicit none

    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(statement_source(), result, options)
    if (.not. result%success()) call fail('frontend rejected statement source')

    call test_io_queries(result%arena)
    call test_control_queries(result%arena)
    call test_branch_queries(result%arena)
    call test_absent_queries(result%arena, result%root_index)
    call test_absent_queries(result%arena, 0)
    call test_labeled_wrong_kind_queries(result%arena)
    print *, 'PASS: compiler statement queries'

contains

    function statement_source() result(source)
        character(len=:), allocatable :: source

        source = 'program statements'//new_line('a')// &
            'class(*), allocatable :: object'//new_line('a')// &
            'integer :: values(3), choice, ios'//new_line('a')// &
            'logical :: mask(3), opened'//new_line('a')// &
            "open(unit=10,file='data.txt',status='old',access='sequential',"// &
            "form='formatted',recl=80,blank='null',position='rewind',"// &
            "action='read',delim='quote',pad='yes',iostat=ios,err=900)"// &
            new_line('a')// &
            "close(unit=10,status='keep',iostat=ios,err=900)"//new_line('a')// &
            'close(unit=11)'//new_line('a')// &
            'rewind(unit=10,iostat=ios,err=900)'//new_line('a')// &
            'backspace(unit=10,iostat=ios,err=900)'//new_line('a')// &
            'endfile(unit=10,iostat=ios,err=900)'//new_line('a')// &
            'inquire(unit=10,opened=opened,iostat=ios,err=900)'//new_line('a')// &
            "print '(I0)', choice"//new_line('a')// &
            "write(unit=10,fmt='(I0)',iostat=ios,err=900) choice"// &
            new_line('a')// &
            "read(unit=10,fmt='(I0)',iostat=ios,end=800,err=900) choice"// &
            new_line('a')// &
            '700 format(I0)'//new_line('a')// &
            'associate(alias => choice)'//new_line('a')// &
            'values(1) = alias'//new_line('a')//'end associate'//new_line('a')// &
            'block'//new_line('a')//'integer :: local'//new_line('a')// &
            'local = choice'//new_line('a')//'end block'//new_line('a')// &
            'allocate(object, source=choice)'//new_line('a')// &
            'select type (object)'//new_line('a')//'type is (integer)'// &
            new_line('a')//'values(1) = object'//new_line('a')// &
            'class default'//new_line('a')//'values(1) = 0'//new_line('a')// &
            'end select'//new_line('a')//'select rank(values)'//new_line('a')// &
            'rank(1)'//new_line('a')//'values = 1'//new_line('a')// &
            'rank default'//new_line('a')//'values = 0'//new_line('a')// &
            'end select'//new_line('a')//'where(mask)'//new_line('a')// &
            'values = 1'//new_line('a')//'elsewhere(.not. mask)'//new_line('a')// &
            'values = 2'//new_line('a')//'elsewhere'//new_line('a')// &
            'values = 3'//new_line('a')//'end where'//new_line('a')// &
            'where(mask) values = 3'//new_line('a')//'goto 100'//new_line('a')// &
            'goto (100,200), choice'//new_line('a')//'pause 1'//new_line('a')// &
            "pause 'hold'"//new_line('a')//'pause'//new_line('a')// &
            '100 continue'//new_line('a')//'200 continue'//new_line('a')// &
            '800 continue'//new_line('a')//'900 continue'//new_line('a')// &
            'contains'//new_line('a')//'subroutine inner'//new_line('a')// &
            'goto 100'//new_line('a')//'100 continue'//new_line('a')// &
            'end subroutine inner'//new_line('a')// &
            'end program statements'
    end function statement_source

    subroutine test_io_queries(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: counts(10)
        integer :: i
        type(io_statement_query_t) :: query

        counts = 0
        do i = 1, arena%size
            query = query_io_statement(arena, i)
            if (.not. query%found) cycle
            counts(query%statement_kind) = counts(query%statement_kind) + 1
            call check_io_query(query)
        end do
        call require_equal(counts(IO_STATEMENT_PRINT), 1, 'PRINT count')
        call require_equal(counts(IO_STATEMENT_WRITE), 1, 'WRITE count')
        call require_equal(counts(IO_STATEMENT_READ), 1, 'READ count')
        call require_equal(counts(IO_STATEMENT_FORMAT), 1, 'FORMAT count')
        call require_equal(counts(IO_STATEMENT_OPEN), 1, 'OPEN count')
        call require_equal(counts(IO_STATEMENT_CLOSE), 2, 'CLOSE count')
        call require_equal(counts(IO_STATEMENT_INQUIRE), 1, 'INQUIRE count')
        call require_equal(counts(IO_STATEMENT_BACKSPACE), 1, 'BACKSPACE count')
        call require_equal(counts(IO_STATEMENT_REWIND), 1, 'REWIND count')
        call require_equal(counts(IO_STATEMENT_ENDFILE), 1, 'ENDFILE count')
    end subroutine test_io_queries

    subroutine check_io_query(query)
        type(io_statement_query_t), intent(in) :: query

        if (.not. allocated(query%item_node_indices)) then
            call fail('I/O item indices are not initialized')
        end if
        select case (query%statement_kind)
        case (IO_STATEMENT_OPEN)
            call check_open_query(query)
        case (IO_STATEMENT_CLOSE)
            if (query%has_status_spec) then
                call require_string(query%unit_spec, '10', 'CLOSE unit')
                call require_string(query%status_spec, "'keep'", 'CLOSE status')
                call require_present_indices(query, .false.)
            else
                call require_string(query%unit_spec, '11', 'minimal CLOSE unit')
                call require_absent_indices(query)
            end if
            call require_unit_node(query)
        case (IO_STATEMENT_INQUIRE)
            call require_specifier(query, 'opened', 'opened')
            call require_unit_node(query)
            call require_present_indices(query, .false.)
        case (IO_STATEMENT_BACKSPACE, IO_STATEMENT_REWIND, IO_STATEMENT_ENDFILE)
            call require_string(query%unit_spec, '10', 'positioning unit')
            call require_unit_node(query)
            call require_present_indices(query, .false.)
        case (IO_STATEMENT_WRITE)
            call require_string(query%unit_spec, '10', 'WRITE unit')
            call require_unit_node(query)
            call require_equal(size(query%item_node_indices), 1, 'WRITE items')
            call require_present_indices(query, .false.)
        case (IO_STATEMENT_READ)
            call require_string(query%unit_spec, '10', 'READ unit')
            call require_unit_node(query)
            call require_equal(size(query%item_node_indices), 1, 'READ items')
            call require_present_indices(query, .true.)
        case (IO_STATEMENT_PRINT)
            call require_equal(size(query%item_node_indices), 1, 'PRINT items')
            if (.not. query%has_format_spec) call fail('PRINT format absent')
        case (IO_STATEMENT_FORMAT)
            if (.not. query%has_format_spec) call fail('FORMAT spec absent')
        end select
    end subroutine check_io_query

    subroutine check_open_query(query)
        type(io_statement_query_t), intent(in) :: query

        call require_string(query%unit_spec, '10', 'OPEN unit')
        call require_string(query%file_spec, "'data.txt'", 'OPEN file')
        call require_string(query%status_spec, "'old'", 'OPEN status')
        call require_string(query%access_spec, "'sequential'", 'OPEN access')
        call require_string(query%form_spec, "'formatted'", 'OPEN form')
        call require_string(query%recl_spec, '80', 'OPEN recl')
        call require_string(query%blank_spec, "'null'", 'OPEN blank')
        call require_string(query%position_spec, "'rewind'", 'OPEN position')
        call require_string(query%action_spec, "'read'", 'OPEN action')
        call require_string(query%delim_spec, "'quote'", 'OPEN delim')
        call require_string(query%pad_spec, "'yes'", 'OPEN pad')
        call require_unit_node(query)
        call require_present_indices(query, .false.)
    end subroutine check_open_query

    subroutine require_unit_node(query)
        type(io_statement_query_t), intent(in) :: query

        if (.not. query%has_unit_node) call fail('unit node absent')
        if (query%unit_node_index <= 0) call fail('invalid unit node index')
    end subroutine require_unit_node

    subroutine require_present_indices(query, expect_end)
        type(io_statement_query_t), intent(in) :: query
        logical, intent(in) :: expect_end

        if (.not. query%has_iostat_node) call fail('IOSTAT index absent')
        if (query%iostat_node_index <= 0) call fail('invalid IOSTAT index')
        if (.not. query%has_err_label) call fail('ERR label absent')
        if (query%err_label_node_index <= 0) call fail('invalid ERR label index')
        if (query%has_end_label .neqv. expect_end) call fail('END presence mismatch')
        if (expect_end) then
            if (query%end_label_node_index <= 0) call fail('invalid END label index')
        end if
    end subroutine require_present_indices

    subroutine require_absent_indices(query)
        type(io_statement_query_t), intent(in) :: query

        if (query%has_iostat_node .or. query%iostat_node_index /= 0) then
            call fail('absent IOSTAT is ambiguous')
        end if
        if (query%has_err_label .or. query%err_label_node_index /= 0) then
            call fail('absent ERR is ambiguous')
        end if
        if (query%has_end_label .or. query%end_label_node_index /= 0) then
            call fail('absent END is ambiguous')
        end if
    end subroutine require_absent_indices

    subroutine require_specifier(query, name, value)
        type(io_statement_query_t), intent(in) :: query
        character(len=*), intent(in) :: name, value
        integer :: i

        do i = 1, size(query%specifiers)
            if (query%specifiers(i)%name /= name) cycle
            call require_string(query%specifiers(i)%value, value, &
                'INQUIRE '//name)
            if (.not. query%specifiers(i)%has_value_node) then
                call fail('INQUIRE '//name//' node absent')
            end if
            return
        end do
        call fail('INQUIRE specifier absent: '//name)
    end subroutine require_specifier

    subroutine test_control_queries(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: counts(8)
        integer :: i
        type(control_statement_query_t) :: query

        counts = 0
        do i = 1, arena%size
            query = query_control_statement(arena, i)
            if (.not. query%found) cycle
            counts(query%statement_kind) = counts(query%statement_kind) + 1
            call check_control_query(query)
        end do
        call require_equal(counts(CONTROL_ASSOCIATE), 1, 'ASSOCIATE count')
        call require_equal(counts(CONTROL_BLOCK), 1, 'BLOCK count')
        call require_equal(counts(CONTROL_SELECT_TYPE), 1, 'SELECT TYPE count')
        call require_equal(counts(CONTROL_TYPE_GUARD), 2, 'TYPE guard count')
        call require_equal(counts(CONTROL_SELECT_RANK), 1, 'SELECT RANK count')
        call require_equal(counts(CONTROL_RANK_BLOCK), 2, 'RANK block count')
        call require_equal(counts(CONTROL_WHERE), 1, 'WHERE construct count')
        call require_equal(counts(CONTROL_WHERE_STATEMENT), 1, &
            'WHERE statement count')
    end subroutine test_control_queries

    subroutine check_control_query(query)
        type(control_statement_query_t), intent(in) :: query

        if (.not. allocated(query%body_node_indices)) then
            call fail('control body is not initialized')
        end if
        select case (query%statement_kind)
        case (CONTROL_ASSOCIATE)
            call require_equal(size(query%associations), 1, 'association count')
            call require_string(query%associations(1)%name, 'alias', &
                'association name')
            if (query%associations(1)%expression_node_index <= 0) then
                call fail('association expression absent')
            end if
            call require_nonempty(query%body_node_indices, 'ASSOCIATE body')
        case (CONTROL_BLOCK)
            call require_nonempty(query%body_node_indices, 'BLOCK body')
        case (CONTROL_SELECT_TYPE, CONTROL_SELECT_RANK)
            if (.not. query%has_selector) call fail('selector absent')
            if (query%selector_node_index <= 0) call fail('invalid selector')
            call require_nonempty(query%child_node_indices, 'selector arms')
            if (.not. query%has_default) call fail('default arm absent')
        case (CONTROL_TYPE_GUARD)
            call check_type_guard_query(query)
        case (CONTROL_RANK_BLOCK)
            call check_rank_block_query(query)
        case (CONTROL_WHERE)
            if (.not. query%has_mask) call fail('WHERE mask absent')
            call require_nonempty(query%body_node_indices, 'WHERE body')
            call require_equal(size(query%elsewhere_clauses), 2, &
                'ELSEWHERE count')
            if (.not. query%elsewhere_clauses(1)%has_mask) then
                call fail('masked ELSEWHERE mask absent')
            end if
            if (query%elsewhere_clauses(1)%mask_node_index <= 0) then
                call fail('invalid ELSEWHERE mask index')
            end if
            call require_nonempty(query%elsewhere_clauses(1)%body_node_indices, &
                'masked ELSEWHERE body')
            if (query%elsewhere_clauses(2)%has_mask) then
                call fail('final ELSEWHERE unexpectedly has a mask')
            end if
            call require_nonempty(query%elsewhere_clauses(2)%body_node_indices, &
                'final ELSEWHERE body')
        case (CONTROL_WHERE_STATEMENT)
            if (.not. query%has_mask) call fail('WHERE statement mask absent')
            if (.not. query%has_assignment) call fail('WHERE assignment absent')
        end select
    end subroutine check_control_query

    subroutine check_type_guard_query(query)
        type(control_statement_query_t), intent(in) :: query

        if (query%is_default) then
            call require_string(query%guard_type, 'class_default', &
                'default guard type')
            if (query%has_type_name) call fail('default type name present')
        else
            call require_string(query%guard_type, 'type_is', 'guard type')
            if (.not. query%has_type_name) call fail('type name absent')
        end if
        call require_nonempty(query%body_node_indices, 'TYPE guard body')
    end subroutine check_type_guard_query

    subroutine check_rank_block_query(query)
        type(control_statement_query_t), intent(in) :: query

        if (query%is_default) then
            if (query%has_rank) call fail('default rank has a value')
        else
            if (.not. query%has_rank) call fail('rank value absent')
            call require_equal(query%rank_value, 1, 'rank value')
        end if
        call require_nonempty(query%body_node_indices, 'RANK body')
    end subroutine check_rank_block_query

    subroutine test_branch_queries(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: counts(3)
        integer :: direct_targets(2)
        integer :: i, direct_count
        type(branch_statement_query_t) :: query

        counts = 0
        direct_count = 0
        direct_targets = 0
        do i = 1, arena%size
            query = query_branch_statement(arena, i)
            if (.not. query%found) cycle
            counts(query%statement_kind) = counts(query%statement_kind) + 1
            if (query%statement_kind == BRANCH_GOTO) then
                if (.not. query%is_computed) then
                    direct_count = direct_count + 1
                    direct_targets(direct_count) = query%targets(1)%node_index
                end if
            end if
            call check_branch_query(query)
        end do
        call require_equal(counts(BRANCH_GOTO), 3, 'GOTO count')
        call require_equal(counts(BRANCH_PAUSE), 3, 'PAUSE count')
        call require_equal(counts(BRANCH_CONTINUE), 5, 'CONTINUE count')
        call require_equal(direct_count, 2, 'direct GOTO count')
        if (direct_targets(1) == direct_targets(2)) then
            call fail('GOTO targets crossed scoping-unit boundaries')
        end if
    end subroutine test_branch_queries

    subroutine check_branch_query(query)
        type(branch_statement_query_t), intent(in) :: query

        select case (query%statement_kind)
        case (BRANCH_GOTO)
            if (query%is_computed) then
                if (.not. query%has_selector) call fail('GOTO selector absent')
                call require_string(query%target_labels, '100, 200', &
                    'computed GOTO labels')
                call require_equal(size(query%targets), 2, &
                    'computed GOTO targets')
            else
                call require_string(query%target_label, '100', 'GOTO label')
                call require_equal(size(query%targets), 1, 'GOTO targets')
            end if
            call require_resolved_targets(query)
        case (BRANCH_PAUSE)
            call check_pause_query(query)
        case (BRANCH_CONTINUE)
            if (.not. query%has_statement_label) then
                call fail('CONTINUE statement label absent')
            end if
        end select
    end subroutine check_branch_query

    subroutine check_pause_query(query)
        type(branch_statement_query_t), intent(in) :: query

        if (query%has_code) then
            if (query%code_node_index <= 0) call fail('invalid PAUSE code')
        else if (query%has_message) then
            call require_string(query%message, "'hold'", 'PAUSE message')
        else
            if (query%code_node_index /= 0) call fail('absent PAUSE code ambiguous')
            call require_string(query%message, '', 'absent PAUSE message')
        end if
    end subroutine check_pause_query

    subroutine require_resolved_targets(query)
        type(branch_statement_query_t), intent(in) :: query
        integer :: i

        do i = 1, size(query%targets)
            if (.not. query%targets(i)%has_node) then
                call fail('GOTO target node absent: '//query%targets(i)%label)
            end if
            if (query%targets(i)%node_index <= 0) then
                call fail('invalid GOTO target node')
            end if
        end do
    end subroutine require_resolved_targets

    subroutine test_absent_queries(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(io_statement_query_t) :: io
        type(control_statement_query_t) :: control
        type(branch_statement_query_t) :: branch

        io = query_io_statement(arena, node_index)
        control = query_control_statement(arena, node_index)
        branch = query_branch_statement(arena, node_index)
        if (io%found) call fail('wrong-kind I/O query found a node')
        if (control%found) call fail('wrong-kind control query found a node')
        if (branch%found) call fail('wrong-kind branch query found a node')
        if (io%has_iostat_node .or. io%iostat_node_index /= 0) then
            call fail('absent IOSTAT is ambiguous')
        end if
        if (.not. allocated(io%item_node_indices)) then
            call fail('absent I/O item indices are not initialized')
        end if
        if (.not. allocated(control%body_node_indices)) then
            call fail('absent control body is not initialized')
        end if
    end subroutine test_absent_queries

    subroutine test_labeled_wrong_kind_queries(arena)
        type(ast_arena_t), intent(in) :: arena
        type(io_statement_query_t) :: io
        type(control_statement_query_t) :: control
        type(branch_statement_query_t) :: branch
        integer :: i

        do i = 1, arena%size
            io = query_io_statement(arena, i)
            if (io%found) then
                if (io%statement_kind == IO_STATEMENT_FORMAT) then
                    control = query_control_statement(arena, i)
                    branch = query_branch_statement(arena, i)
                    call require_empty_wrong_kind(control, branch)
                end if
            end if
            branch = query_branch_statement(arena, i)
            if (.not. branch%found) cycle
            if (branch%statement_kind /= BRANCH_CONTINUE) cycle
            io = query_io_statement(arena, i)
            if (io%found .or. io%has_statement_label) then
                call fail('wrong-kind I/O query retained statement metadata')
            end if
            return
        end do
        call fail('labeled CONTINUE fixture absent')
    end subroutine test_labeled_wrong_kind_queries

    subroutine require_empty_wrong_kind(control, branch)
        type(control_statement_query_t), intent(in) :: control
        type(branch_statement_query_t), intent(in) :: branch

        if (control%found .or. control%line /= 0 .or. control%column /= 0) then
            call fail('wrong-kind control query retained metadata')
        end if
        if (branch%found .or. branch%has_statement_label) then
            call fail('wrong-kind branch query retained statement metadata')
        end if
        if (branch%line /= 0 .or. branch%column /= 0) then
            call fail('wrong-kind branch query retained source location')
        end if
    end subroutine require_empty_wrong_kind

    subroutine require_nonempty(values, label)
        integer, intent(in) :: values(:)
        character(len=*), intent(in) :: label

        if (size(values) == 0) call fail(label//' is empty')
    end subroutine require_nonempty

    subroutine require_equal(actual, expected, label)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: label

        if (actual /= expected) then
            write (error_unit, '(A,I0,A,I0)') trim(label)//': got ', actual, &
                ', expected ', expected
            error stop 1
        end if
    end subroutine require_equal

    subroutine require_string(actual, expected, label)
        character(len=*), intent(in) :: actual, expected, label

        if (actual /= expected) then
            call fail(trim(label)//': got "'//actual//'", expected "'// &
                expected//'"')
        end if
    end subroutine require_string

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//message
        error stop 1
    end subroutine fail

end program test_compiler_statement_queries
