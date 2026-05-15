program test_frontend_compiler_api
    use fortfront, only: compiler_frontend_result_t, &
                         compiler_frontend_options_t, &
                         compile_frontend_from_string, &
                         compile_frontend_from_file, get_node_type_at
    use ast_nodes_control, only: if_node
    use ast_nodes_io, only: print_statement_node
    implicit none

    logical :: all_passed

    print *, '=== fortfront Compiler Frontend API Tests ==='
    print *

    all_passed = .true.
    if (.not. test_compile_string()) all_passed = .false.
    if (.not. test_block_if_structure()) all_passed = .false.
    if (.not. test_file_diagnostics()) all_passed = .false.
    if (.not. test_compile_file()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All compiler frontend API tests passed!'
        stop 0
    else
        print *, 'Compiler frontend API tests failed!'
        stop 1
    end if

contains

    logical function test_compile_string()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: source = &
                                       'program sample'//new_line('a')// &
                                       '  implicit none'//new_line('a')// &
                                       '  integer :: x'//new_line('a')// &
                                       '  x = 3'//new_line('a')// &
                                       'end program sample'

        test_compile_string = .true.
        print *, 'Testing compile_frontend_from_string...'

        call compile_frontend_from_string(source, result)

        if (.not. result%success()) then
            print *, '  FAIL: unexpected diagnostic -> ', &
                trim(result%diagnostic_text)
            test_compile_string = .false.
            return
        end if

        if (result%root_index <= 0) then
            print *, '  FAIL: root index not set'
            test_compile_string = .false.
            return
        end if

        if (result%arena%size <= 0) then
            print *, '  FAIL: arena is empty'
            test_compile_string = .false.
            return
        end if

        if (.not. allocated(result%tokens)) then
            print *, '  FAIL: tokens were not retained'
            test_compile_string = .false.
            return
        end if

        if (get_node_type_at(result%arena, result%root_index) /= 'program') then
            print *, '  FAIL: root node is not a program'
            test_compile_string = .false.
            return
        end if

        print *, '  PASS: compile_frontend_from_string'
    end function test_compile_string

    logical function test_block_if_structure()
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        character(len=*), parameter :: source = &
                                       'program branch_sample'//new_line('a')// &
                                       '  integer :: x'//new_line('a')// &
                                       '  x = 5'//new_line('a')// &
                                       '  if (x == 5) then'//new_line('a')// &
                                       '    print *, 1'//new_line('a')// &
                                       '  else'//new_line('a')// &
                                       '    print *, 0'//new_line('a')// &
                                       '  end if'//new_line('a')// &
                                       'end program branch_sample'
        integer :: i
        integer :: if_count
        integer :: then_index
        integer :: else_index
        logical :: branch_shape_ok

        test_block_if_structure = .true.
        print *, 'Testing block IF compiler API structure...'

        options%run_semantics = .false.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%parse_ok) then
            print *, '  FAIL: block IF source did not parse -> ', &
                trim(result%diagnostic_text)
            test_block_if_structure = .false.
            return
        end if

        if_count = 0
        branch_shape_ok = .false.
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
            type is (if_node)
                if_count = if_count + 1
                branch_shape_ok = validate_block_if_node(result, i, node)
                if (branch_shape_ok) then
                    then_index = node%then_body_indices(1)
                    else_index = node%else_body_indices(1)
                end if
            end select
        end do

        if (if_count /= 1) then
            print *, '  FAIL: expected one IF node, got ', if_count
            test_block_if_structure = .false.
            return
        end if

        if (.not. branch_shape_ok) then
            print *, '  FAIL: block IF branches are not exposed consistently'
            test_block_if_structure = .false.
            return
        end if

        if (.not. is_print_node(result, then_index)) then
            print *, '  FAIL: THEN branch is not a print statement'
            test_block_if_structure = .false.
            return
        end if

        if (.not. is_print_node(result, else_index)) then
            print *, '  FAIL: ELSE branch is not a print statement'
            test_block_if_structure = .false.
            return
        end if

        print *, '  PASS: block IF structure exposed'
    end function test_block_if_structure

    logical function validate_block_if_node(result, if_index, node)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: if_index
        type(if_node), intent(in) :: node
        integer :: then_index
        integer :: else_index

        validate_block_if_node = .false.
        if (node%condition_index <= 0) return
        if (.not. allocated(node%then_body_indices)) return
        if (.not. allocated(node%else_body_indices)) return
        if (size(node%then_body_indices) /= 1) return
        if (size(node%else_body_indices) /= 1) return

        then_index = node%then_body_indices(1)
        else_index = node%else_body_indices(1)
        if (then_index <= 0 .or. else_index <= 0) return
        if (then_index > result%arena%size .or. else_index > result%arena%size) &
            return

        if (result%arena%entries(then_index)%parent_index /= if_index) return
        if (result%arena%entries(else_index)%parent_index /= if_index) return
        validate_block_if_node = .true.
    end function validate_block_if_node

    logical function is_print_node(result, node_index)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: node_index

        is_print_node = .false.
        if (node_index <= 0 .or. node_index > result%arena%size) return
        if (.not. result%arena%has_node_at(node_index)) return

        select type (node => result%arena%entries(node_index)%node)
        type is (print_statement_node)
            is_print_node = .true.
        end select
    end function is_print_node

    logical function test_file_diagnostics()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: file_path = &
                                       'compiler_api_missing_sample.f90'

        test_file_diagnostics = .true.
        print *, 'Testing file diagnostics retention...'

        call cleanup_temp_file(file_path)
        call compile_frontend_from_file(file_path, result)

        if (result%success()) then
            print *, '  FAIL: missing file failure was not reported'
            test_file_diagnostics = .false.
            return
        end if

        if (result%parse_ok) then
            print *, '  FAIL: parse_ok set on missing file'
            test_file_diagnostics = .false.
            return
        end if

        if (len_trim(result%diagnostic_text) == 0) then
            print *, '  FAIL: diagnostic text is empty'
            test_file_diagnostics = .false.
            return
        end if

        if (trim(result%source_path) /= file_path) then
            print *, '  FAIL: source path not retained for missing file'
            test_file_diagnostics = .false.
            return
        end if

        print *, '  PASS: file diagnostics retained'
    end function test_file_diagnostics

    logical function test_compile_file()
        type(compiler_frontend_result_t) :: result
        character(len=*), parameter :: file_path = 'compiler_api_sample.f90'
        integer :: unit
        integer :: io_stat

        test_compile_file = .true.
        print *, 'Testing compile_frontend_from_file...'

        open (newunit=unit, file=file_path, status='replace', action='write', &
              iostat=io_stat)
        if (io_stat /= 0) then
            print *, '  FAIL: unable to create sample file'
            test_compile_file = .false.
            return
        end if
        write (unit, '(A)') 'program file_sample'
        write (unit, '(A)') '  implicit none'
        write (unit, '(A)') '  integer :: x'
        write (unit, '(A)') '  x = 7'
        write (unit, '(A)') 'end program file_sample'
        close (unit)

        call compile_frontend_from_file(file_path, result)

        if (.not. result%success()) then
            print *, '  FAIL: unexpected diagnostic -> ', &
                trim(result%diagnostic_text)
            call cleanup_temp_file(file_path)
            test_compile_file = .false.
            return
        end if

        if (trim(result%source_path) /= file_path) then
            print *, '  FAIL: source path not retained'
            call cleanup_temp_file(file_path)
            test_compile_file = .false.
            return
        end if

        print *, '  PASS: compile_frontend_from_file'
        call cleanup_temp_file(file_path)
    end function test_compile_file

    subroutine cleanup_temp_file(path)
        character(len=*), intent(in) :: path
        integer :: unit
        integer :: stat
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) return

        open (newunit=unit, file=path, status='old', action='write', iostat=stat)
        if (stat == 0) then
            close (unit, status='delete')
        end if
    end subroutine cleanup_temp_file

end program test_frontend_compiler_api
