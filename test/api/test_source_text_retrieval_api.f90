program test_source_text_retrieval_api
    use fortfront, only: ast_arena_t, tooling_load_ast_from_string, &
                         tooling_parse_options_t, has_source_text, &
                         get_source_text, get_source_line, get_source_range
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Source Text Retrieval API Tests ==='

    if (.not. test_crlf_roundtrip()) all_passed = .false.
    if (.not. test_line_and_range_queries()) all_passed = .false.

    if (all_passed) then
        print *, 'All source text retrieval API tests passed!'
        stop 0
    end if

    print *, 'Some source text retrieval API tests failed!'
    stop 1

contains

    logical function test_crlf_roundtrip()
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source
        character(len=:), allocatable :: stored
        character(len=*), parameter :: crlf = char(13) // char(10)
        logical :: found

        test_crlf_roundtrip = .true.
        print *, 'Testing CRLF normalization...'

        source = 'x = 1' // crlf // 'y = 2' // crlf

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          tooling_parse_options_t())
        if (len(error_msg) > 0) then
            print *, '  FAIL: Unexpected parse error: ', trim(error_msg)
            test_crlf_roundtrip = .false.
            return
        end if

        if (.not. has_source_text(arena)) then
            print *, '  FAIL: Expected source text to be stored in arena'
            test_crlf_roundtrip = .false.
            return
        end if

        call get_source_text(arena, stored, found)
        if (.not. found) then
            print *, '  FAIL: Expected get_source_text to succeed'
            test_crlf_roundtrip = .false.
            return
        end if

        if (stored /= 'x = 1'//new_line('A')//'y = 2'//new_line('A')) then
            print *, '  FAIL: Stored source did not match expected normalization'
            test_crlf_roundtrip = .false.
            return
        end if

        print *, '  PASS: CRLF normalization'
    end function test_crlf_roundtrip

    logical function test_line_and_range_queries()
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source
        character(len=:), allocatable :: line
        character(len=:), allocatable :: text
        logical :: found

        test_line_and_range_queries = .true.
        print *, 'Testing line and range queries...'

        source = 'abc' // new_line('A') // 'def' // new_line('A')

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          tooling_parse_options_t())
        if (len(error_msg) > 0) then
            print *, '  FAIL: Unexpected parse error: ', trim(error_msg)
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_line(arena, 1, line, found)
        if (.not. found .or. line /= 'abc') then
            print *, '  FAIL: Line 1 mismatch'
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_line(arena, 2, line, found)
        if (.not. found .or. line /= 'def') then
            print *, '  FAIL: Line 2 mismatch'
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_line(arena, 3, line, found)
        if (.not. found .or. len(line) /= 0) then
            print *, '  FAIL: Expected trailing empty line'
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_range(arena, 1, 2, 1, 3, text, found)
        if (.not. found .or. text /= 'bc') then
            print *, '  FAIL: Range mismatch on line 1'
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_range(arena, 1, 0, 1, 1, text, found)
        if (found) then
            print *, '  FAIL: Expected invalid column to be rejected'
            test_line_and_range_queries = .false.
            return
        end if

        call get_source_line(arena, 0, line, found)
        if (found) then
            print *, '  FAIL: Expected invalid line number to be rejected'
            test_line_and_range_queries = .false.
            return
        end if

        print *, '  PASS: Line and range queries'
    end function test_line_and_range_queries

end program test_source_text_retrieval_api
