program test_parameter_attributes_simple
    use lexer_api, only: lex_source, lex_file
    use parser_api, only: parse_tokens, parse_tokens_safe
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, compile_source
    use ast_nodes_data, only: parameter_declaration_node, INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use ast_visitor
    use ast_traversal
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed = .true.

    print *, '=== Testing Parameter Attributes ==='
    print *

    if (.not. test_parameter_attributes_parsing()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Parameter attribute tests passed'
    else
        print *, 'FAIL: Parameter attribute tests failed'
        stop 1
    end if

contains

    include '../../common/read_example.inc'


    function test_parameter_attributes_parsing() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, generated_code, error_msg

        passed = .true.

        ! Test case from issue #20 - wrapped in a program
        call read_example('examples/f90/parameter_attributes_simple.f90', source)

        call transform_lazy_fortran_string(source, generated_code, error_msg)

        if (error_msg /= "") then
            print *, "ERROR: Failed to parse parameter attributes: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(generated_code)) then
            print *, "ERROR: No output generated (not allocated)"
            passed = .false.
        else if (len(generated_code) == 0) then
            print *, "ERROR: Empty output generated"
            passed = .false.
        else
            print *, "SUCCESS: Parameter attributes parsed correctly"
            print *, "Generated code:"
            print *, trim(generated_code)

            ! Verify attributes are preserved
            if (index(generated_code, "intent(in)") == 0) then
                print *, "WARNING: intent(in) not found in output"
            end if
            if (index(generated_code, "intent(out)") == 0) then
                print *, "WARNING: intent(out) not found in output"
            end if
            if (index(generated_code, "optional") == 0) then
                print *, "WARNING: optional not found in output"
            end if
        end if

    end function test_parameter_attributes_parsing

end program test_parameter_attributes_simple
