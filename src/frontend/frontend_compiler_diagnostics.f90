module frontend_compiler_diagnostics
    use frontend_compiler_api, only: compiler_frontend_result_t
    use fortfront_types, only: source_range_t, DIAGNOSTIC_ERROR, &
        DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO
    use error_reporting, only: PARSER_ERROR_WARNING => ERROR_WARNING, &
        PARSER_ERROR_ERROR => ERROR_ERROR, PARSER_ERROR_FATAL => ERROR_FATAL
    use error_handling, only: ERROR_PARSER, ERROR_SEMANTIC, &
        SEMANTIC_ERROR_WARNING => ERROR_WARNING, &
        SEMANTIC_ERROR_ERROR => ERROR_ERROR, &
        SEMANTIC_ERROR_CRITICAL => ERROR_CRITICAL
    implicit none
    private

    integer, parameter, public :: DIAGNOSTIC_PHASE_PARSER = 1
    integer, parameter, public :: DIAGNOSTIC_PHASE_SEMANTIC = 2
    integer, parameter, public :: DIAGNOSTIC_CODE_PARSER = ERROR_PARSER
    integer, parameter, public :: DIAGNOSTIC_CODE_SEMANTIC = ERROR_SEMANTIC

    type, public :: compiler_diagnostic_t
        integer :: phase = 0
        integer :: code = 0
        integer :: severity = DIAGNOSTIC_INFO
        type(source_range_t) :: span
        character(len=:), allocatable :: message
        character(len=:), allocatable :: category
    end type compiler_diagnostic_t

    public :: get_compiler_diagnostics

contains

    function get_compiler_diagnostics(result) result(diagnostics)
        type(compiler_frontend_result_t), intent(in) :: result
        type(compiler_diagnostic_t), allocatable :: diagnostics(:)
        integer :: count, output_index

        count = result%semantic_ctx%errors%count
        if (allocated(result%parser_errors)) then
            count = count + size(result%parser_errors)
        end if
        allocate (diagnostics(count))
        output_index = 0
        call fill_parser_diagnostics(result, diagnostics, output_index)
        call fill_semantic_diagnostics(result, diagnostics, output_index)
    end function get_compiler_diagnostics

    subroutine fill_parser_diagnostics(result, diagnostics, output_index)
        type(compiler_frontend_result_t), intent(in) :: result
        type(compiler_diagnostic_t), intent(inout) :: diagnostics(:)
        integer, intent(inout) :: output_index
        integer :: i

        if (.not. allocated(result%parser_errors)) return
        do i = 1, size(result%parser_errors)
            output_index = output_index + 1
            diagnostics(output_index)%phase = DIAGNOSTIC_PHASE_PARSER
            diagnostics(output_index)%code = DIAGNOSTIC_CODE_PARSER
            diagnostics(output_index)%severity = parser_severity( &
                result%parser_errors(i)%severity)
            call set_span(diagnostics(output_index)%span, &
                result%parser_errors(i)%context%line, &
                result%parser_errors(i)%context%column, &
                result%parser_errors(i)%context%end_line, &
                result%parser_errors(i)%context%end_column)
            if (allocated(result%parser_errors(i)%message)) then
                diagnostics(output_index)%message = result%parser_errors(i)%message
            else
                diagnostics(output_index)%message = ''
            end if
            diagnostics(output_index)%category = 'parser'
        end do
    end subroutine fill_parser_diagnostics

    subroutine fill_semantic_diagnostics(result, diagnostics, output_index)
        type(compiler_frontend_result_t), intent(in) :: result
        type(compiler_diagnostic_t), intent(inout) :: diagnostics(:)
        integer, intent(inout) :: output_index
        integer :: i

        do i = 1, result%semantic_ctx%errors%count
            output_index = output_index + 1
            call fill_semantic_identity(result, i, diagnostics(output_index))
            call set_span(diagnostics(output_index)%span, &
                result%semantic_ctx%errors%errors(i)%line, &
                result%semantic_ctx%errors%errors(i)%column, &
                result%semantic_ctx%errors%errors(i)%end_line, &
                result%semantic_ctx%errors%errors(i)%end_column)
            if (allocated( &
                result%semantic_ctx%errors%errors(i)%error_message)) then
                diagnostics(output_index)%message = &
                    result%semantic_ctx%errors%errors(i)%error_message
            else
                diagnostics(output_index)%message = ''
            end if
            if (allocated(result%semantic_ctx%errors%errors(i)%component)) then
                diagnostics(output_index)%category = &
                    result%semantic_ctx%errors%errors(i)%component
            else
                diagnostics(output_index)%category = 'semantic'
            end if
        end do
    end subroutine fill_semantic_diagnostics

    subroutine fill_semantic_identity(result, index, diagnostic)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: index
        type(compiler_diagnostic_t), intent(inout) :: diagnostic

        diagnostic%phase = DIAGNOSTIC_PHASE_SEMANTIC
        diagnostic%code = result%semantic_ctx%errors%errors(index)%error_code
        if (diagnostic%code == 0) diagnostic%code = DIAGNOSTIC_CODE_SEMANTIC
        diagnostic%severity = semantic_severity( &
            result%semantic_ctx%errors%errors(index)%severity)
    end subroutine fill_semantic_identity

    subroutine set_span(span, line, column, end_line, end_column)
        type(source_range_t), intent(out) :: span
        integer, intent(in) :: line, column
        integer, intent(in), optional :: end_line, end_column

        span%start%line = max(line, 0)
        span%start%column = max(column, 0)
        span%start%byte_offset = 0
        span%end = span%start
        if (present(end_line)) then
            span%end%line = max(end_line, span%start%line)
        end if
        if (present(end_column)) then
            span%end%column = max(end_column, span%start%column)
        else if (span%start%column > 0) then
            span%end%column = span%start%column + 1
        end if
    end subroutine set_span

    pure integer function parser_severity(severity) result(public_severity)
        integer, intent(in) :: severity

        select case (severity)
        case (PARSER_ERROR_FATAL, PARSER_ERROR_ERROR)
            public_severity = DIAGNOSTIC_ERROR
        case (PARSER_ERROR_WARNING)
            public_severity = DIAGNOSTIC_WARNING
        case default
            public_severity = DIAGNOSTIC_INFO
        end select
    end function parser_severity

    pure integer function semantic_severity(severity) result(public_severity)
        integer, intent(in) :: severity

        select case (severity)
        case (SEMANTIC_ERROR_CRITICAL, SEMANTIC_ERROR_ERROR)
            public_severity = DIAGNOSTIC_ERROR
        case (SEMANTIC_ERROR_WARNING)
            public_severity = DIAGNOSTIC_WARNING
        case default
            public_severity = DIAGNOSTIC_INFO
        end select
    end function semantic_severity

end module frontend_compiler_diagnostics
