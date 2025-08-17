program test_source_reconstruction_quality
    use source_reconstruction_analyzer, only: source_location_t, &
                                              source_context_t, &
                                              strategy_dispatcher_t, &
                                              reconstruction_quality_t, &
                                              source_reconstruction_analyzer_t
    use ast_core, only: ast_arena_t, create_ast_arena, &
                        create_identifier, identifier_node
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Source Reconstruction Quality Assessment Tests ==="
    print *, ""

    ! Test 1: Quality assessment initialization
    call test_start("Quality assessment initialization")
    block
        type(reconstruction_quality_t) :: quality
        
        call quality%initialize()
        
        if (quality%total_nodes == 0 .and. &
            quality%exact_matches == 0 .and. &
            quality%generated_fallbacks == 0 .and. &
            quality%failed_reconstructions == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: all counts = 0"
            print *, "  Got: total=", quality%total_nodes, &
                     " exact=", quality%exact_matches, &
                     " generated=", quality%generated_fallbacks, &
                     " failed=", quality%failed_reconstructions
        end if
    end block

    ! Test 2: Record exact matches
    call test_start("Record exact matches")
    block
        type(reconstruction_quality_t) :: quality
        
        call quality%initialize()
        call quality%record_exact_match()
        call quality%record_exact_match()
        call quality%record_exact_match()
        
        if (quality%total_nodes == 3 .and. &
            quality%exact_matches == 3 .and. &
            quality%generated_fallbacks == 0 .and. &
            quality%failed_reconstructions == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: total=3, exact=3, others=0"
            print *, "  Got: total=", quality%total_nodes, &
                     " exact=", quality%exact_matches, &
                     " generated=", quality%generated_fallbacks, &
                     " failed=", quality%failed_reconstructions
        end if
    end block

    ! Test 3: Record generated fallbacks
    call test_start("Record generated fallbacks")
    block
        type(reconstruction_quality_t) :: quality
        
        call quality%initialize()
        call quality%record_generated_fallback()
        call quality%record_generated_fallback()
        
        if (quality%total_nodes == 2 .and. &
            quality%exact_matches == 0 .and. &
            quality%generated_fallbacks == 2 .and. &
            quality%failed_reconstructions == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: total=2, generated=2, others=0"
            print *, "  Got: total=", quality%total_nodes, &
                     " exact=", quality%exact_matches, &
                     " generated=", quality%generated_fallbacks, &
                     " failed=", quality%failed_reconstructions
        end if
    end block

    ! Test 4: Record failed reconstructions
    call test_start("Record failed reconstructions")
    block
        type(reconstruction_quality_t) :: quality
        
        call quality%initialize()
        call quality%record_failed_reconstruction()
        
        if (quality%total_nodes == 1 .and. &
            quality%exact_matches == 0 .and. &
            quality%generated_fallbacks == 0 .and. &
            quality%failed_reconstructions == 1) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: total=1, failed=1, others=0"
            print *, "  Got: total=", quality%total_nodes, &
                     " exact=", quality%exact_matches, &
                     " generated=", quality%generated_fallbacks, &
                     " failed=", quality%failed_reconstructions
        end if
    end block

    ! Test 5: Calculate accuracy (perfect score)
    call test_start("Calculate accuracy (perfect score)")
    block
        type(reconstruction_quality_t) :: quality
        real :: accuracy
        
        call quality%initialize()
        call quality%record_exact_match()
        call quality%record_exact_match()
        call quality%record_exact_match()
        
        accuracy = quality%get_accuracy()
        
        if (abs(accuracy - 1.0) < 1e-6) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: accuracy = 1.0"
            print *, "  Got: accuracy =", accuracy
        end if
    end block

    ! Test 6: Calculate accuracy (mixed results)
    call test_start("Calculate accuracy (mixed results)")
    block
        type(reconstruction_quality_t) :: quality
        real :: accuracy
        
        call quality%initialize()
        call quality%record_exact_match()
        call quality%record_exact_match()
        call quality%record_generated_fallback()
        call quality%record_failed_reconstruction()
        
        accuracy = quality%get_accuracy()
        
        if (abs(accuracy - 0.5) < 1e-6) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: accuracy = 0.5 (2 exact out of 4 total)"
            print *, "  Got: accuracy =", accuracy
        end if
    end block

    ! Test 7: Calculate accuracy (empty case)
    call test_start("Calculate accuracy (empty case)")
    block
        type(reconstruction_quality_t) :: quality
        real :: accuracy
        
        call quality%initialize()
        
        accuracy = quality%get_accuracy()
        
        if (abs(accuracy - 0.0) < 1e-6) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: accuracy = 0.0 (no nodes)"
            print *, "  Got: accuracy =", accuracy
        end if
    end block

    ! Test 8: Error handling - invalid node index
    call test_start("Error handling - invalid node index")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        character(:), allocatable :: result
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        ! Try to reconstruct with invalid index (arena is empty)
        result = dispatcher%reconstruct_node(context, arena, 999)
        
        if (result == "<invalid_index>") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '<invalid_index>'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 9: Error handling - unallocated node
    call test_start("Error handling - unallocated node")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        character(:), allocatable :: result
        
        call dispatcher%initialize_default_strategies()
        arena = create_ast_arena()
        
        ! Manually expand arena size without adding nodes
        if (.not. allocated(arena%entries)) then
            allocate(arena%entries(10))
        end if
        arena%size = 1
        arena%capacity = 10
        
        ! Try to reconstruct with unallocated node
        result = dispatcher%reconstruct_node(context, arena, 1)
        
        if (result == "<unallocated_node>") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '<unallocated_node>'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 10: Error handling - empty source context
    call test_start("Error handling - empty source context")
    block
        type(source_context_t) :: context
        type(source_location_t) :: location
        character(:), allocatable :: result
        
        ! Don't initialize context - should handle gracefully
        location = source_location_t(line=1, column=1, start_char=1, end_char=5)
        
        result = context%extract_range(location)
        
        if (result == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 11: Error handling - invalid line number
    call test_start("Error handling - invalid line number")
    block
        type(source_context_t) :: context
        character(:), allocatable :: result
        character(*), parameter :: test_source = "line1" // new_line('a') // "line2"
        
        call context%initialize_source(test_source)
        
        result = context%get_line_text(0)  ! Invalid line number
        
        if (result == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for invalid line"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 12: Error handling - invalid character range
    call test_start("Error handling - invalid character range")
    block
        type(source_context_t) :: context
        type(source_location_t) :: location
        character(:), allocatable :: result
        character(*), parameter :: test_source = "hello world"
        
        call context%initialize_source(test_source)
        
        ! Invalid range (end before start)
        location = source_location_t(line=1, column=1, start_char=5, end_char=3)
        
        result = context%extract_range(location)
        
        if (result == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for invalid range"
            print *, "  Got: '", result, "'"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All source reconstruction quality tests passed!"
        stop 0
    else
        print *, "Some source reconstruction quality tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_source_reconstruction_quality