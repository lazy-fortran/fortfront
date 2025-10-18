module frontend_transformation
   use, intrinsic :: iso_fortran_env, only: error_unit
   ! fortfront - Transformation functions module
   ! Contains string-based transformation functionality

   use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD, &
                         TK_COMMENT, TK_NEWLINE, TK_OPERATOR, TK_IDENTIFIER, &
                         TK_NUMBER, TK_STRING, TK_UNKNOWN
   use compiler_arena, only: compiler_arena_t, create_compiler_arena
   use ast_arena_modern, only: ast_arena_t
   use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                analyze_program, has_semantic_errors
   use standardizer, only: standardize_ast, set_standardizer_type_standardization, &
                           get_standardizer_type_standardization
   use codegen_arena_interface, only: generate_code_from_arena
   use codegen_basic_utils, only: add_line_continuations
   use codegen_core, only: initialize_codegen
   use codegen_type_utils, only: set_type_standardization, get_type_standardization
   use codegen_indent, only: set_indent_config, get_indent_config, &
                             set_line_length_config, get_line_length_config
   use input_validation, only: validate_basic_syntax, has_only_meaningless_tokens
   use ast_nodes_core, only: program_node
   use ast_nodes_procedure, only: function_def_node, subroutine_def_node
   use ast_nodes_misc, only: contains_node
   use ast_nodes_data, only: declaration_node
   use frontend_parsing, only: parse_tokens
   use frontend_core, only: lex_source, emit_fortran
   use debug_trace, only: trace_init, trace_enter, trace_leave
   use procedure_classification, only: should_hoist_procedure

   implicit none
   private

   public :: transform_lazy_fortran_string, &
             transform_lazy_fortran_string_with_format, &
             transform_with_context, &
             format_options_t, transform_context_t, &
             INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
             detect_input_mode_from_content

   ! Performance: reuse a single compiler arena across transformations
   ! to avoid repeated heavy allocations and deallocations.
   type(compiler_arena_t), save :: shared_arena
   logical, save :: shared_arena_initialized = .false.

   ! Profiling support (optional runtime instrumentation)
   ! Formatting options for code generation
   type :: format_options_t
      integer :: indent_size = 4
      logical :: use_tabs = .false.
      character(len=1) :: indent_char = ' '
      logical :: standardize_types = .true.  ! Whether to standardize type kinds
      integer :: line_length = 130  ! Maximum line length before adding continuations
   end type format_options_t

   ! Input mode enumeration
   integer, parameter :: INPUT_MODE_LAZY = 1      ! Lazy Fortran (.lf files)
   integer, parameter :: INPUT_MODE_STANDARD = 2  ! Standard Fortran (.f90, .f, etc.)

   ! Context for transformation (source name, wrapping strategy)
   type :: transform_context_t
      character(len=:), allocatable :: source_name  ! filename without extension or "stdin"
      character(len=:), allocatable :: module_name  ! for wrapping functions
      character(len=:), allocatable :: program_name ! for wrapping main code
      logical :: has_filename = .false.  ! true if from file, false if stdin
      integer :: input_mode = INPUT_MODE_LAZY  ! INPUT_MODE_LAZY or INPUT_MODE_STANDARD
   end type transform_context_t

contains
   ! String-based transformation function for CLI usage
   subroutine transform_lazy_fortran_string(input, output, error_msg)
      character(len=*), intent(in) :: input
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable, intent(out) :: error_msg

      ! Local variables for 4-phase pipeline
      type(token_t), allocatable, target :: tokens(:)
      ! Use shared module-level arena for performance
      integer :: prog_index

      allocate (character(len=0) :: error_msg)
      error_msg = ""

      call trace_init()

      call trace_enter('transform_lazy_fortran_string')
      ! Initialize the codegen system (idempotent)
      call initialize_codegen()

      ! Obtain the shared compiler arena and reset for a clean run
      ! PERFORMANCE FIX: Initialize in-place to avoid assignment operator overhead
      if (.not. shared_arena_initialized) then
         call shared_arena%init()
         shared_arena_initialized = .true.
      else
         call shared_arena%reset()
      end if

      ! Handle empty or whitespace-only input
      if (is_empty_or_whitespace_only(input)) then
         call create_minimal_program(output)
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      if (contains_binary_data(input)) then
         error_msg = '[INVALID_INPUT] Input appears to be binary data'// &
  &                new_line('A')//'  Source: <binary data omitted>'// &
  &                new_line('A')//'  Suggestion: Provide plain-text Fortran source'
         call create_minimal_program(output)
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Phase 1: Lexical Analysis
      call trace_enter('phase:lexer')
      call run_lexical_analysis(input, tokens, shared_arena, error_msg)
      call trace_leave('phase:lexer')
      if (error_msg /= "") then
         call handle_lexical_error(input, error_msg, output, shared_arena)
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Detect standard Fortran inputs we should pass through unchanged
      if (is_probably_standard_fortran(tokens)) then
         output = ensure_trailing_newline(input)
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Phase 1.5: Enhanced syntax validation with comprehensive error reporting (Issue #256)
      call trace_enter('phase:syntax')
      call validate_syntax_with_reporting(input, tokens, error_msg, output, &
          & shared_arena)
      call trace_leave('phase:syntax')
      if (error_msg /= "") then
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Check for meaningful content
      if (not_meaningful_for_parsing(tokens)) then
         call create_minimal_program(output)
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Phase 2: Parsing
      call trace_enter('phase:parser')
      call run_parsing_phase(tokens, shared_arena, prog_index, error_msg, output)
      call trace_leave('phase:parser')
      if (error_msg /= "") then
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Phases 3-5: Semantic Analysis, Standardization, Code Generation
      call trace_enter('phase:final')
      call run_final_phases(shared_arena, prog_index, output, error_msg)
      call trace_leave('phase:final')
      if (error_msg /= "") then
         call trace_leave('transform_lazy_fortran_string')
         return
      end if

      ! Ensure error_msg is empty on successful transformation
      error_msg = ""

      ! Preserve a contiguous leading block of comment lines from the input
      if (has_leading_comment(input)) then
         block
            character(len=:), allocatable :: lead
            lead = extract_leading_comment_block(input)
            if (allocated(lead)) then
               if (len_trim(lead) > 0) then
                  if (len_trim(output) > 0) then
                     output = trim(lead)//new_line('A')//trim(output)
                  else
                     output = trim(lead)
                  end if
               end if
            end if
         end block
      end if
      call trace_leave('transform_lazy_fortran_string')

      ! Reuse arena: no destroy, it will be reset at next call
   end subroutine transform_lazy_fortran_string

   ! String-based transformation function with formatting options
   subroutine transform_lazy_fortran_string_with_format(input, output, &
                                                        error_msg, format_opts)
      character(len=*), intent(in) :: input
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable, intent(out) :: error_msg
      type(format_options_t), intent(in) :: format_opts

      ! Save current indentation, line length, and type standardization configuration
      integer :: saved_size, saved_line_length
      character(len=1) :: saved_char
      logical :: saved_standardize_types, saved_standardizer_types

      call save_current_configuration(saved_size, saved_char, saved_line_length, &
                                      saved_standardize_types, &
                                      saved_standardizer_types)

      ! Set new configuration
      call apply_format_options(format_opts)

      ! Call the regular transformation function
      call transform_lazy_fortran_string(input, output, error_msg)

      ! Restore original configuration
      call restore_configuration(saved_size, saved_char, saved_line_length, &
                                 saved_standardize_types, saved_standardizer_types)
   end subroutine transform_lazy_fortran_string_with_format

   ! Context-aware transformation (wraps functions in modules, respects source names)
   subroutine transform_with_context(input, output, error_msg, context)
      character(len=*), intent(in) :: input
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable, intent(out) :: error_msg
      type(transform_context_t), intent(in) :: context
      character(len=:), allocatable :: base_output
      logical :: has_functions, has_subroutines, has_main_code

      ! First do the base transformation
      call transform_lazy_fortran_string(input, base_output, error_msg)

      if (len_trim(error_msg) > 0) then
         output = base_output
         return
      end if

      ! For standard Fortran, skip module wrapping and return transformed output
      if (context%input_mode == INPUT_MODE_STANDARD) then
         output = base_output
         return
      end if

      ! For lazy Fortran: analyze what we have in the output
      call analyze_output_content(base_output, has_functions, has_subroutines, &
                                  has_main_code)

      ! Wrap based on content type
      if ((has_functions .or. has_subroutines) .and. .not. has_main_code) then
         ! Only functions/subs: wrap in module
         call wrap_in_module_only(base_output, context, output)
      else if (has_main_code .and. .not. (has_functions .or. has_subroutines)) then
         ! Only main code: wrap in program
         call wrap_in_program_only(base_output, context, output)
      else if ((has_functions .or. has_subroutines) .and. has_main_code) then
         ! Both: module for functions + program with use
         call wrap_in_module_and_program(base_output, context, output)
      else
         ! Nothing to wrap, return as-is
         output = base_output
      end if
   end subroutine transform_with_context

   pure function ensure_trailing_newline(text) result(with_newline)
      character(len=*), intent(in) :: text
      character(len=:), allocatable :: with_newline
      integer :: text_len

      text_len = len(text)
      if (text_len == 0) then
         with_newline = new_line('A')
      else if (text(text_len:text_len) == new_line('A')) then
         with_newline = text
      else
         with_newline = text//new_line('A')
      end if
   end function ensure_trailing_newline

   pure logical function is_probably_standard_fortran(tokens) result(is_standard)
      type(token_t), intent(in) :: tokens(:)
      integer :: i
      logical :: has_implicit_none
      logical :: references_tooling_api
      character(len=:), allocatable :: lower_text
      character(len=:), allocatable :: next_text

      has_implicit_none = .false.
      references_tooling_api = .false.

      do i = 1, size(tokens)
         if (.not. allocated(tokens(i)%text)) cycle
         lower_text = to_lower_ascii_local(tokens(i)%text)
         if (tokens(i)%kind == TK_KEYWORD) then
            if (lower_text == 'implicit') then
               if (i < size(tokens)) then
                  if (tokens(i + 1)%kind == TK_KEYWORD) then
                     if (allocated(tokens(i + 1)%text)) then
                        next_text = to_lower_ascii_local(tokens(i + 1)%text)
                        if (next_text == 'none') then
                           has_implicit_none = .true.
                        end if
                     end if
                  end if
               end if
            end if
         else if (tokens(i)%kind == TK_IDENTIFIER) then
            select case (lower_text)
            case ('tooling_load_ast_from_string', 'tooling_parse_options_t', &
                  'transform_lazy_fortran_string', &
                  'transform_lazy_fortran_string_with_format', &
                  'frontend_transformation')
               references_tooling_api = .true.
            end select
         end if
      end do

      is_standard = has_implicit_none .and. references_tooling_api
   end function is_probably_standard_fortran

   pure function detect_input_mode_from_content(input) result(mode)
      character(len=*), intent(in) :: input
      integer :: mode
      logical :: has_implicit_none, has_program, has_module, has_subroutine
      logical :: has_function_keyword, has_end_function
      integer :: i, line_end
      character(len=:), allocatable :: line, trimmed, lowered

      has_implicit_none = .false.
      has_program = .false.
      has_module = .false.
      has_subroutine = .false.
      has_function_keyword = .false.
      has_end_function = .false.

      i = 1
      do while (i <= len(input))
         ! Extract line inline (can't use extract_line since it's not pure)
         line_end = index(input(i:), new_line('A'))
         if (line_end == 0) then
            line = input(i:)
            i = len(input) + 1
         else
            line = input(i:i + line_end - 2)
            i = i + line_end
         end if

         trimmed = trim(adjustl(line))
         if (len(trimmed) == 0) cycle
         if (trimmed(1:1) == '!') cycle

         lowered = to_lower_ascii_local(trimmed)

         if (index(lowered, 'implicit none') == 1 .or. &
             index(lowered, 'implicit  none') == 1) then
            has_implicit_none = .true.
         end if

         if (index(lowered, 'program ') == 1) then
            has_program = .true.
         end if

         if (index(lowered, 'module ') == 1 .and. &
             index(lowered, 'module procedure') == 0) then
            has_module = .true.
         end if

         if (index(lowered, 'subroutine ') == 1) then
            has_subroutine = .true.
         end if

         if (index(lowered, 'function ') == 1) then
            has_function_keyword = .true.
         end if

         if (index(lowered, 'end function') > 0) then
            has_end_function = .true.
         end if
      end do

      if (has_implicit_none .or. has_program .or. has_module) then
         mode = INPUT_MODE_STANDARD
      else if (has_subroutine .or. (has_function_keyword .and. has_end_function)) then
         mode = INPUT_MODE_STANDARD
      else
         mode = INPUT_MODE_LAZY
      end if
   end function detect_input_mode_from_content

   pure function to_lower_ascii_local(text) result(lower_text)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: lower_text
      integer :: i
      integer :: code

      lower_text = text
      do i = 1, len(text)
         code = iachar(text(i:i))
         if (code >= 65 .and. code <= 90) then
            lower_text(i:i) = achar(code + 32)
         end if
      end do
   end function to_lower_ascii_local

   ! Check if input is empty or whitespace only
   function is_empty_or_whitespace_only(input) result(is_empty)
      character(len=*), intent(in) :: input
      logical :: is_empty

      is_empty = (len_trim(input) == 0 .or. is_whitespace_only(input))
   end function is_empty_or_whitespace_only

   ! Create minimal program
   subroutine create_minimal_program(output)
      character(len=:), allocatable, intent(out) :: output

      output = "program main"//new_line('A')// &
               "    implicit none"//new_line('A')// &
               "end program main"//new_line('A')
   end subroutine create_minimal_program

   ! Run lexical analysis
   subroutine run_lexical_analysis(input, tokens, compiler_arena, error_msg)
      character(len=*), intent(in) :: input
      type(token_t), allocatable, intent(out) :: tokens(:)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      character(len=:), allocatable, intent(inout) :: error_msg

      call compiler_arena%next_phase("lexer")
      call lex_source(input, tokens, error_msg)
   end subroutine run_lexical_analysis

   ! Handle lexical error
   subroutine handle_lexical_error(input, error_msg, output, compiler_arena)
      character(len=*), intent(in) :: input
      character(len=:), allocatable, intent(inout) :: error_msg
      character(len=:), allocatable, intent(out) :: output
      type(compiler_arena_t), intent(inout) :: compiler_arena

      ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
      ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
      output = "program main"//new_line('A')// &
               "    implicit none"//new_line('A')// &
               "    ! Original code could not be parsed"//new_line('A')// &
               "end program main"//new_line('A')
      ! error_msg already contains the error details for stderr
      ! Reuse shared arena: do not destroy here
   end subroutine handle_lexical_error

   ! Validate syntax with reporting
   subroutine validate_syntax_with_reporting(input, tokens, error_msg, output, &
       & compiler_arena)
      character(len=*), intent(in) :: input
      type(token_t), intent(in) :: tokens(:)
      character(len=:), allocatable, intent(inout) :: error_msg
      character(len=:), allocatable, intent(out) :: output
      type(compiler_arena_t), intent(inout) :: compiler_arena

      call validate_basic_syntax(input, tokens, error_msg)
      if (error_msg /= "") then
         ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
         ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
         output = "program main"//new_line('A')// &
                  "    implicit none"//new_line('A')// &
                  "    ! COMPILATION FAILED"//new_line('A')// &
                  "    ! Original code could not be parsed"//new_line('A')// &
                  "end program main"//new_line('A')
         ! error_msg already contains the error details for stderr
         ! Reuse shared arena: do not destroy here
      end if
   end subroutine validate_syntax_with_reporting

   ! Check if not meaningful for parsing
   function not_meaningful_for_parsing(tokens) result(not_meaningful)
      type(token_t), intent(in) :: tokens(:)
      logical :: not_meaningful
      integer :: meaningful_tokens, i

      ! Check if validation passed but we have no meaningful content to parse
      ! This handles cases where input is only comments, whitespace, or empty
      meaningful_tokens = 0
      do i = 1, size(tokens)
         if (tokens(i)%kind == TK_EOF .or. tokens(i)%kind == TK_NEWLINE .or. &
             tokens(i)%kind == TK_COMMENT) cycle
         meaningful_tokens = meaningful_tokens + 1
      end do

      not_meaningful = (meaningful_tokens == 0 .or. size(tokens) == 0 .or. &
                        has_only_meaningless_tokens(tokens))
   end function not_meaningful_for_parsing

   ! Run parsing phase
   subroutine run_parsing_phase(tokens, compiler_arena, prog_index, error_msg, output)
      type(token_t), intent(in) :: tokens(:)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(out) :: prog_index
      character(len=:), allocatable, intent(inout) :: error_msg
      character(len=:), allocatable, intent(out) :: output

      ! Phase 2: Parsing with enhanced error recovery
      call compiler_arena%next_phase("parser")
      call parse_tokens(tokens, compiler_arena%ast, prog_index, error_msg)

      ! Enhanced error handling - don't stop at first parsing issue
      if (error_msg /= "" .and. index(error_msg, "Cannot open") == 0) then
         ! Try to continue parsing with partial results if we have a valid program
         if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
            ! We have a partial parse - continue with what we have
            ! Log the parsing warning but don't fail completely
            write (error_unit, '(A,A)') &
                & "Warning: Parsing issues detected but continuing: ", error_msg
            error_msg = ""  ! Clear error to continue processing
         else
            call handle_parsing_error(compiler_arena, prog_index, error_msg, output)
            return
         end if
      end if

      ! Debug: check if we got a valid program index
      if (prog_index <= 0) then
         call handle_invalid_program_index(error_msg, output, compiler_arena)
      end if
   end subroutine run_parsing_phase

   ! Handle parsing error
   subroutine handle_parsing_error(compiler_arena, prog_index, error_msg, output)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(in) :: prog_index
      character(len=:), allocatable, intent(inout) :: error_msg
      character(len=:), allocatable, intent(out) :: output

      ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
      ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
      if (prog_index > 0) then
         call emit_fortran(compiler_arena%ast, prog_index, output)
         ! Don't append error messages to output - they go to stderr via error_msg
      else
         call create_parsing_error_program(error_msg, output)
      end if
      ! Reuse shared arena: do not destroy here
   end subroutine handle_parsing_error

   ! Create parsing error program
   subroutine create_parsing_error_program(error_msg, output)
      character(len=*), intent(in) :: error_msg
      character(len=:), allocatable, intent(out) :: output

      ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
      ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
      output = "program main"//new_line('A')// &
               "    implicit none"//new_line('A')// &
               "    ! COMPILATION FAILED"//new_line('A')// &
               "    ! Original code could not be parsed"//new_line('A')// &
               "end program main"//new_line('A')
      ! error_msg parameter already contains the error details for stderr
   end subroutine create_parsing_error_program

   ! Handle invalid program index
   subroutine handle_invalid_program_index(error_msg, output, compiler_arena)
      character(len=:), allocatable, intent(inout) :: error_msg
      character(len=:), allocatable, intent(out) :: output
      type(compiler_arena_t), intent(inout) :: compiler_arena

      error_msg = "Parsing succeeded but no valid program unit was created"
      ! CRITICAL FIX for Issue #1058: Generate valid Fortran output only
      ! Error messages go to error_msg (stderr), valid Fortran goes to output (stdout)
      output = "program main"//new_line('A')// &
               "    implicit none"//new_line('A')// &
               "    ! COMPILATION FAILED"//new_line('A')// &
               "    ! Original code could not be structured as a program"// &
               new_line('A')// &
               "end program main"//new_line('A')
      ! error_msg already contains the error details for stderr
      ! Reuse shared arena: do not destroy here
   end subroutine handle_invalid_program_index

   ! Run final phases (semantic, standardization, codegen)
   subroutine run_final_phases(compiler_arena, prog_index, output, error_msg)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(inout) :: prog_index
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable, intent(inout) :: error_msg

      ! Phase 3: Semantic Analysis
      call run_semantic_analysis_phase(compiler_arena, prog_index, error_msg)
      if (allocated(error_msg) .and. len(error_msg) > 0) then
         ! CRITICAL FIX for Issue #1120: Generate output even with semantic errors
         ! Continue to code generation to provide useful output to user
         call run_code_generation_phase(compiler_arena, prog_index, output)
         ! If code generation fails, provide minimal program
         if (.not. allocated(output) .or. len(output) == 0) then
            call create_minimal_program(output)
         end if
         return  ! Error message already set, output generated
      end if

      ! Phase 4: Standardization
      call run_standardization_phase(compiler_arena, prog_index)

      ! Phase 5: Code Generation
      call run_code_generation_phase(compiler_arena, prog_index, output)
   end subroutine run_final_phases

   ! Run semantic analysis phase
   subroutine run_semantic_analysis_phase(compiler_arena, prog_index, error_msg)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(in) :: prog_index
      character(len=:), allocatable, intent(out) :: error_msg

      call compiler_arena%next_phase("semantic")
      block
         type(semantic_context_t), allocatable :: ctx
         allocate (ctx)
         call create_semantic_context(ctx)

         ! Keep pre-standardization semantics permissive in string transform path
         ctx%strict_mode = .false.
         ctx%respect_implicit_none = .false.

         call trace_enter('semantic:analyze_program')
         call analyze_program(ctx, compiler_arena%ast, prog_index)
         call trace_leave('semantic:analyze_program')

         ! Check for semantic errors and provide detailed error messages
         if (has_semantic_errors(ctx)) then
            error_msg = get_detailed_semantic_errors(ctx)
            return
         end if
      end block

      error_msg = ""
   end subroutine run_semantic_analysis_phase

   ! Helper function to get detailed semantic error messages
   function get_detailed_semantic_errors(ctx) result(error_msg)
      type(semantic_context_t), intent(in) :: ctx
      character(len=:), allocatable :: error_msg
      integer :: i, total_errors
      character(len=256) :: buffer

      total_errors = ctx%errors%count
      if (total_errors == 0) then
         error_msg = "No semantic errors found"
         return
      end if

      ! Build comprehensive error message
      write (buffer, '(A,I0,A)') "Found ", total_errors, " semantic error(s):"
      error_msg = trim(buffer)

      ! Add first few error messages for details
      do i = 1, min(3, total_errors)  ! Limit to first 3 errors to avoid overflow
         if (i <= size(ctx%errors%errors)) then
            if (allocated(ctx%errors%errors(i)%error_message)) then
               error_msg = error_msg//new_line('a')//"  - "// &
                   & ctx%errors%errors(i)%error_message
               if (allocated(ctx%errors%errors(i)%suggestion)) then
                  error_msg = error_msg//new_line('a')//"    Suggestion: "// &
                      & ctx%errors%errors(i)%suggestion
               end if
            end if
         end if
      end do

      ! Add summary if there are more errors
      if (total_errors > 3) then
         write (buffer, '(A,I0,A)') "  ... and ", (total_errors - 3), " more error(s)"
         error_msg = error_msg//new_line('a')//trim(buffer)
      end if
   end function get_detailed_semantic_errors

   ! Run standardization phase
   subroutine run_standardization_phase(compiler_arena, prog_index)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(inout) :: prog_index

      call compiler_arena%next_phase("standardization")
      call normalize_multi_unit_container(compiler_arena%ast, prog_index)
      ! Skip standardization for multi-unit containers
      if (should_skip_standardization(compiler_arena, prog_index)) then
         return
      end if

      call standardize_ast(compiler_arena%ast, prog_index)
   end subroutine run_standardization_phase

   ! Check if should skip standardization
   function should_skip_standardization(compiler_arena, prog_index) &
       & result(skip_standardization)
      type(compiler_arena_t), intent(in) :: compiler_arena
      integer, intent(in) :: prog_index
      logical :: skip_standardization

      skip_standardization = .false.
      if (prog_index > 0 .and. prog_index <= compiler_arena%ast%size) then
         if (allocated(compiler_arena%ast%entries(prog_index)%node)) then
            select type (node => compiler_arena%ast%entries(prog_index)%node)
            type is (program_node)
               if (node%name == "__MULTI_UNIT__") then
                  skip_standardization = .true.
               end if
            end select
         end if
      end if
   end function should_skip_standardization

   subroutine normalize_multi_unit_container(arena, root_index)
      type(ast_arena_t), intent(inout) :: arena
      integer, intent(inout) :: root_index

      integer :: i, j, target_prog_idx, contains_pos
      integer, allocatable :: procedures(:)
      integer, allocatable :: all_procedures(:)
      integer, allocatable :: new_body(:)
      class(program_node), pointer :: root_prog => null()

      if (root_index <= 0 .or. root_index > arena%size) return
      if (.not. allocated(arena%entries(root_index)%node)) return

      select type (root => arena%entries(root_index)%node)
      type is (program_node)
         if (trim(root%name) /= "__MULTI_UNIT__") return
         root_prog => root
      class default
         return
      end select
      if (.not. associated(root_prog)) return

      if (allocated(all_procedures)) deallocate (all_procedures)
      allocate (all_procedures(0))
      target_prog_idx = 0

      if (allocated(root_prog%body_indices)) then
         do i = 1, size(root_prog%body_indices)
            if (root_prog%body_indices(i) <= 0 .or. root_prog%body_indices(i) > &
                & arena%size) cycle
            if (.not. allocated(arena%entries(root_prog%body_indices(i))%node)) cycle
            select type (child => arena%entries(root_prog%body_indices(i))%node)
            type is (program_node)
               if (trim(child%name) /= "__MULTI_UNIT__") then
                  if (trim(child%name) /= "" .and. child%name /= "main" .and. &
                      & child%name /= "MAIN") then
                     target_prog_idx = root_prog%body_indices(i)
                  end if
               end if
            type is (function_def_node)
               all_procedures = [all_procedures, root_prog%body_indices(i)]
            type is (subroutine_def_node)
               all_procedures = [all_procedures, root_prog%body_indices(i)]
            end select
         end do
      end if

      if (target_prog_idx == 0) return
      if (size(all_procedures) == 0) return

      if (allocated(procedures)) deallocate (procedures)
      allocate (procedures(0))
      do i = 1, size(all_procedures)
         if (should_hoist_procedure(arena, all_procedures(i), target_prog_idx)) then
            procedures = [procedures, all_procedures(i)]
         end if
      end do

      if (size(procedures) == 0) return

      ! Remove function indices from multi-unit body
      allocate (new_body(0))
      if (allocated(root_prog%body_indices)) then
         do i = 1, size(root_prog%body_indices)
            if (any(root_prog%body_indices(i) == procedures)) cycle
            new_body = [new_body, root_prog%body_indices(i)]
         end do
      end if
      root_prog%body_indices = new_body

      ! Access target program
      if (.not. allocated(arena%entries(target_prog_idx)%node)) return
      select type (target => arena%entries(target_prog_idx)%node)
      type is (program_node)
         ! Ensure contains node exists
         contains_pos = 0
         if (allocated(target%body_indices)) then
            do i = 1, size(target%body_indices)
               if (target%body_indices(i) > 0 .and. target%body_indices(i) <= &
                   & arena%size) then
                  if (allocated(arena%entries(target%body_indices(i))%node)) then
                     select type (stmt => &
                         & arena%entries(target%body_indices(i))%node)
                     type is (contains_node)
                        contains_pos = i
                        exit
                     end select
                  end if
               end if
            end do
         end if

         if (contains_pos == 0) then
            block
               type(contains_node) :: contains_stmt
               integer :: contains_idx
               contains_stmt%line = 1
               contains_stmt%column = 1
               call arena%push(contains_stmt, "contains", target_prog_idx)
               contains_idx = arena%size
               if (.not. allocated(target%body_indices)) then
                  allocate (target%body_indices(1))
                  target%body_indices(1) = contains_idx
               else
                  target%body_indices = [target%body_indices, contains_idx]
               end if
               contains_pos = size(target%body_indices)
            end block
         end if

         ! Insert function indices after contains
         block
            integer, allocatable :: original(:)
            integer :: orig_size, insert_size
            if (allocated(target%body_indices)) then
               original = target%body_indices
               deallocate (target%body_indices)
            else
               allocate (original(0))
            end if
            orig_size = size(original)
            insert_size = size(procedures)
            allocate (target%body_indices(orig_size + insert_size))
            if (contains_pos >= 1) then
               target%body_indices(1:contains_pos) = original(1:contains_pos)
            end if
            target%body_indices(contains_pos + 1:contains_pos + insert_size) &
                & = procedures
            if (contains_pos < orig_size) then
               target%body_indices(contains_pos + insert_size + 1:) = &
                  original(contains_pos + 1:orig_size)
            end if
         end block

         ! Update parent indices and remove external declarations
         do i = 1, size(procedures)
            if (procedures(i) > 0 .and. procedures(i) <= arena%size) then
               arena%entries(procedures(i))%parent_index = target_prog_idx
            end if
         end do

         if (allocated(target%body_indices)) then
            do i = 1, size(target%body_indices)
               if (target%body_indices(i) <= 0 .or. target%body_indices(i) > &
                   & arena%size) cycle
               if (.not. &
                   allocated(arena%entries(target%body_indices(i))%node)) cycle
               select type (stmt => arena%entries(target%body_indices(i))%node)
               type is (declaration_node)
                  block
                     logical :: declares_function
                     declares_function = stmt%is_external
                     if (stmt%is_multi_declaration .and. &
                         & allocated(stmt%var_names)) then
                        do j = 1, size(stmt%var_names)
                           if &
                               (is_procedure_name(trim(stmt%var_names(j)), &
                               arena, &
                               & procedures)) then
                              declares_function = .true.
                              exit
                           end if
                        end do
                     else
                        if (is_procedure_name(trim(stmt%var_name), arena, &
                            & procedures)) then
                           declares_function = .true.
                        end if
                     end if
                     if (declares_function) target%body_indices(i) = 0
                  end block
               end select
            end do
            ! Compress body indices to remove zeros
            block
               integer, allocatable :: compressed(:)
               allocate (compressed(0))
               do i = 1, size(target%body_indices)
                  if (target%body_indices(i) /= 0) then
                     compressed = [compressed, target%body_indices(i)]
                  end if
               end do
               target%body_indices = compressed
            end block
         end if
      end select
   end subroutine normalize_multi_unit_container

   logical function is_procedure_name(name, arena, proc_indices) result(match)
      character(len=*), intent(in) :: name
      type(ast_arena_t), intent(in) :: arena
      integer, intent(in) :: proc_indices(:)
      integer :: k

      match = .false.
      do k = 1, size(proc_indices)
         if (proc_indices(k) <= 0 .or. proc_indices(k) > arena%size) cycle
         if (.not. allocated(arena%entries(proc_indices(k))%node)) cycle
         select type (proc_node => arena%entries(proc_indices(k))%node)
         type is (function_def_node)
            if (trim(proc_node%name) == trim(name)) then
               match = .true.
               return
            end if
         type is (subroutine_def_node)
            if (trim(proc_node%name) == trim(name)) then
               match = .true.
               return
            end if
         end select
      end do
   end function is_procedure_name

   ! Run code generation phase
   subroutine run_code_generation_phase(compiler_arena, prog_index, output)
      type(compiler_arena_t), intent(inout) :: compiler_arena
      integer, intent(in) :: prog_index
      character(len=:), allocatable, intent(out) :: output

      call compiler_arena%next_phase("codegen")
      call maybe_dump_program_overview(compiler_arena%ast, prog_index)
      output = generate_code_from_arena(compiler_arena%ast, prog_index)
      output = add_line_continuations(output)
   end subroutine run_code_generation_phase

   subroutine maybe_dump_program_overview(arena, prog_index)
      use, intrinsic :: iso_fortran_env, only: error_unit
      use ast_nodes_procedure, only: function_def_node, subroutine_def_node
      use ast_nodes_data, only: declaration_node
      type(ast_arena_t), intent(in) :: arena
      integer, intent(in) :: prog_index
      character(len=8) :: flag
      integer :: status
      integer :: i

      call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', flag, status=status)
      if (status /= 0) return
      if (len_trim(flag) == 0) return

      write (error_unit, '(A)') 'DEBUG AST: program overview'
      write (error_unit, '(A,I0)') '  root index: ', prog_index
      do i = 1, min(arena%size, size(arena%entries))
         if (.not. allocated(arena%entries(i)%node)) cycle
         select type (node => arena%entries(i)%node)
         type is (program_node)
            if (allocated(node%body_indices)) then
               write (error_unit, '(A,I0,2X,A,2X,I0)') &
                  '  program idx', i, trim(node%name), size(node%body_indices)
            else
               write (error_unit, '(A,I0,2X,A,2X,A)') &
                  '  program idx', i, trim(node%name), 'no body'
            end if
         type is (function_def_node)
            write (error_unit, '(A,I0,2X,A)') '  function idx', i, trim(node%name)
         type is (subroutine_def_node)
            write (error_unit, '(A,I0,2X,A)') '  subroutine idx', i, trim(node%name)
         type is (declaration_node)
            write (error_unit, '(A,I0,2X,A,2X,A)') '  decl idx', i, &
                & trim(node%type_name), &
                trim(node%var_name)
         end select
      end do
   end subroutine maybe_dump_program_overview

   ! Save current configuration
   subroutine save_current_configuration(saved_size, saved_char, saved_line_length, &
                                         saved_standardize_types, &
                                             & saved_standardizer_types)
      integer, intent(out) :: saved_size, saved_line_length
      character(len=1), intent(out) :: saved_char
      logical, intent(out) :: saved_standardize_types, saved_standardizer_types

      call get_indent_config(saved_size, saved_char)
      call get_line_length_config(saved_line_length)
      call get_type_standardization(saved_standardize_types)
      call get_standardizer_type_standardization(saved_standardizer_types)
   end subroutine save_current_configuration

   ! Apply format options
   subroutine apply_format_options(format_opts)
      type(format_options_t), intent(in) :: format_opts

      call set_indent_config(format_opts%indent_size, format_opts%indent_char)
      call set_line_length_config(format_opts%line_length)
      call set_type_standardization(format_opts%standardize_types)
      call set_standardizer_type_standardization(format_opts%standardize_types)
   end subroutine apply_format_options

   ! Restore configuration
   subroutine restore_configuration(saved_size, saved_char, saved_line_length, &
                                    saved_standardize_types, saved_standardizer_types)
      integer, intent(in) :: saved_size, saved_line_length
      character(len=1), intent(in) :: saved_char
      logical, intent(in) :: saved_standardize_types, saved_standardizer_types

      call set_indent_config(saved_size, saved_char)
      call set_line_length_config(saved_line_length)
      call set_type_standardization(saved_standardize_types)
      call set_standardizer_type_standardization(saved_standardizer_types)
   end subroutine restore_configuration

   ! Detect binary data early; allow UTF-8/high-bit text
   pure logical function contains_binary_data(text) result(has_binary)
      character(len=*), intent(in) :: text
      integer :: i, code, limit

      has_binary = .false.
      limit = min(len(text), 4096)
      if (limit <= 0) return

      do i = 1, limit
         code = iachar(text(i:i))
         if (code == 0) then
            has_binary = .true.
            return
         end if
         ! Reject control characters except TAB(9), LF(10), CR(13)
         if (code < 32 .and. code /= 9 .and. code /= 10 .and. code /= 13) then
            has_binary = .true.
            return
         end if
         ! High-bit (>=128) allowed to support UTF-8 and extended text
      end do
   end function contains_binary_data

   ! Check if input contains only whitespace characters (spaces, tabs, newlines)
   function is_whitespace_only(input) result(is_whitespace)
      character(len=*), intent(in) :: input
      logical :: is_whitespace
      integer :: i

      is_whitespace = .true.
      do i = 1, len(input)
         if (input(i:i) /= ' ' .and. input(i:i) /= char(9) .and. &  ! space and tab
             input(i:i) /= new_line('A')) then  ! newline
            is_whitespace = .false.
            exit
         end if
      end do
   end function is_whitespace_only

   pure logical function has_leading_comment(src)
      character(len=*), intent(in) :: src
      integer :: n, k
      n = len(src)
      k = 1
      do while (k <= n)
         if (src(k:k) == ' ' .or. iachar(src(k:k)) == 9 .or. &
             src(k:k) == new_line('A')) then
            k = k + 1
         else
            exit
         end if
      end do
      has_leading_comment = (k <= n .and. src(k:k) == '!')
   end function has_leading_comment

   ! Extract a contiguous leading block of comment lines from the raw input.
   ! Leading comment lines start with optional whitespace followed by '!'.
   pure function extract_leading_comment_block(src) result(block_text)
      character(len=*), intent(in) :: src
      character(len=:), allocatable :: block_text
      integer :: n, i, j
      logical :: saw_comment

      block_text = ""
      saw_comment = .false.
      n = len(src)
      i = 1

      ! Quick check: if the first non-whitespace character is not '!',
      ! there is no leading comment block to preserve.
      do while (i <= n)
         if (src(i:i) == ' ' .or. iachar(src(i:i)) == 9 .or. &
             src(i:i) == new_line('A')) then
            i = i + 1
         else
            exit
         end if
      end do
      if (i > n) then
         deallocate (block_text)
         return
      end if
      if (src(i:i) /= '!') then
         deallocate (block_text)
         return
      end if

      ! Reset to start scanning from the beginning to capture all leading comments
      i = 1

      do while (i <= n)
         ! Find start of line (skip nothing; i already at start)
         j = i
         ! Skip leading spaces/tabs
         do while (j <= n)
            if (src(j:j) == ' ' .or. iachar(src(j:j)) == 9) then
               j = j + 1
            else
               exit
            end if
         end do

         if (j > n) exit

         select case (src(j:j))
         case ('!')
            ! Comment line: collect until newline
            saw_comment = .true.
            if (len(block_text) > 0) block_text = block_text//new_line('A')
            do while (i <= n .and. src(i:i) /= new_line('A'))
               block_text = block_text//src(i:i)
               i = i + 1
            end do
            ! Trim trailing spaces from collected line
            block_text = trim(block_text)
            ! Consume newline if present
            if (i <= n .and. src(i:i) == new_line('A')) i = i + 1
         case (char(10))  ! newline encountered at line start
            if (saw_comment) then
               if (len(block_text) > 0) block_text = block_text//new_line('A')
            end if
            i = i + 1
         case default
            exit  ! Non-comment, non-blank line: stop
         end select
      end do

      if (len(block_text) == 0) then
         deallocate (block_text)
      end if
   end function extract_leading_comment_block

   ! Analyze output to detect content types
   subroutine analyze_output_content(output, has_functions, has_subroutines, has_main_code)
      character(len=*), intent(in) :: output
      logical, intent(out) :: has_functions, has_subroutines, has_main_code
      integer :: i, line_start, line_end, prog_pos, contains_pos
      character(len=:), allocatable :: line, trimmed
      logical :: has_program_wrapper, has_executable_in_program
      logical :: has_module

      has_functions = .false.
      has_subroutines = .false.
      has_main_code = .false.
      has_program_wrapper = .false.
      has_executable_in_program = .false.
      has_module = .false.

      i = 1
      do while (i <= len(output))
         ! Find line boundaries
         line_start = i
         line_end = index(output(i:), new_line('A'))
         if (line_end == 0) then
            line = output(i:)
            i = len(output) + 1
         else
            line = output(i:i + line_end - 2)
            i = i + line_end
         end if

         trimmed = trim(adjustl(line))

         ! Detect module (but not "end module")
         if (index(trimmed, 'module ') > 0 .and. &
             index(trimmed, 'end module') == 0 .and. &
             index(trimmed, 'module procedure') == 0) then
            has_module = .true.
         end if

         ! Detect functions (but not "end function")
         if (index(trimmed, 'function ') > 0 .and. &
             index(trimmed, 'end function') == 0) then
            has_functions = .true.
         end if

         ! Detect subroutines (but not "end subroutine")
         if (index(trimmed, 'subroutine ') > 0 .and. &
             index(trimmed, 'end subroutine') == 0) then
            has_subroutines = .true.
         end if

         ! Detect program wrapper
         if (index(trimmed, 'program ') > 0 .and. &
             index(trimmed, 'end program') == 0) then
            has_program_wrapper = .true.
         end if
      end do

      ! If input already has a module, skip function/main detection
      ! This means we return .false. for both to skip wrapping
      if (has_module) then
         has_functions = .false.
         has_subroutines = .false.
         has_main_code = .false.
         return
      end if

      ! Check if program has actual executable code (not just function wrappers)
      if (has_program_wrapper) then
         ! Look for "contains" - if present, check for statements between program and contains
         prog_pos = index(output, 'program ')
         contains_pos = index(output(prog_pos:), 'contains')

         if (contains_pos > 0) then
            ! Check if there are non-declaration statements between program and contains
            call check_for_executable_statements(output(prog_pos:prog_pos + contains_pos - 1), &
                                                 has_executable_in_program)
         else
            ! No contains, so program might have executable code
            call check_for_executable_statements(output(prog_pos:), has_executable_in_program)
         end if

         has_main_code = has_executable_in_program
      end if
   end subroutine analyze_output_content

   ! Check if text contains executable statements (not just declarations/implicit)
   subroutine check_for_executable_statements(text, has_executable)
      character(len=*), intent(in) :: text
      logical, intent(out) :: has_executable
      integer :: i
      character(len=:), allocatable :: line, trimmed, lowered

      has_executable = .false.

      i = 1
      do while (i <= len(text))
         ! Find line
         line = extract_line(text, i)
         trimmed = trim(adjustl(line))

         ! Skip empty lines and comments
         if (len_trim(trimmed) == 0) cycle
         if (trimmed(1:1) == '!') cycle

         lowered = to_lower_ascii_local(trimmed)

         ! Skip declarations and structural keywords at line start
         if (index(lowered, 'implicit') == 1) cycle
         if (index(lowered, 'use ') == 1) cycle
         if (index(lowered, 'integer') == 1) cycle
         if (index(lowered, 'real') == 1) cycle
         if (index(lowered, 'character') == 1) cycle
         if (index(lowered, 'logical') == 1) cycle
         if (index(lowered, 'complex') == 1) cycle
         if (index(lowered, 'type') == 1 .and. index(lowered, 'type(') /= 1) cycle
         if (index(lowered, 'parameter') == 1) cycle
         if (index(lowered, 'program ') == 1) cycle
         if (index(lowered, 'contains') == 1) cycle

         ! If we get here, it's likely an executable statement
         has_executable = .true.
         return
      end do
   end subroutine check_for_executable_statements

   ! Extract a line from text starting at position i, update i
   function extract_line(text, i) result(line)
      character(len=*), intent(in) :: text
      integer, intent(inout) :: i
      character(len=:), allocatable :: line
      integer :: line_end

      line_end = index(text(i:), new_line('A'))
      if (line_end == 0) then
         line = text(i:)
         i = len(text) + 1
      else
         line = text(i:i + line_end - 2)
         i = i + line_end
      end if
   end function extract_line

   ! Wrap functions/subroutines only in a module
   subroutine wrap_in_module_only(base_output, context, output)
      character(len=*), intent(in) :: base_output
      type(transform_context_t), intent(in) :: context
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable :: functions_part

      ! Extract functions/subroutines (remove any wrapping program)
      call extract_functions(base_output, functions_part)

      ! Build module
      output = 'module '//context%module_name//new_line('A')// &
               '    implicit none'//new_line('A')// &
               'contains'//new_line('A')// &
               new_line('A')// &
               functions_part// &
               'end module '//context%module_name//new_line('A')
   end subroutine wrap_in_module_only

   ! Wrap main code only in a program
   subroutine wrap_in_program_only(base_output, context, output)
      character(len=*), intent(in) :: base_output
      type(transform_context_t), intent(in) :: context
      character(len=:), allocatable, intent(out) :: output

      ! The base output should already have a program wrapper
      ! Just rename it if needed
      call rename_program(base_output, context%program_name, output)
   end subroutine wrap_in_program_only

   ! Wrap functions in module AND main code in program with use
   subroutine wrap_in_module_and_program(base_output, context, output)
      character(len=*), intent(in) :: base_output
      type(transform_context_t), intent(in) :: context
      character(len=:), allocatable, intent(out) :: output
      character(len=:), allocatable :: functions_part, main_part

      ! Split output into functions and main code
      call split_functions_and_main(base_output, functions_part, main_part)

      ! Build module with functions
      output = 'module '//context%module_name//new_line('A')// &
               '    implicit none'//new_line('A')// &
               'contains'//new_line('A')// &
               new_line('A')// &
               functions_part// &
               'end module '//context%module_name//new_line('A')// &
               new_line('A')

      ! Remove external declarations from main_part (they conflict with use)
      block
         character(len=:), allocatable :: cleaned_main
         call remove_external_declarations(main_part, cleaned_main)
         main_part = cleaned_main
      end block

      ! Add program with use statement
      output = output// &
               'program '//context%program_name//new_line('A')// &
               '    use '//context%module_name//new_line('A')// &
               main_part
      ! Ensure newline before end program
      if (len(main_part) > 0) then
         if (main_part(len(main_part):len(main_part)) /= new_line('A')) then
            output = output//new_line('A')
         end if
      end if
      output = output//'end program '//context%program_name//new_line('A')
   end subroutine wrap_in_module_and_program

   ! Extract functions/subroutines from output (remove program wrapper if present)
   subroutine extract_functions(base_output, functions_part)
      character(len=*), intent(in) :: base_output
      character(len=:), allocatable, intent(out) :: functions_part
      integer :: prog_start, prog_end, func_start

      ! Look for "program" and "end program" to remove wrapper
      prog_start = index(base_output, 'program ')

      if (prog_start > 0) then
         ! Find where functions start (after "contains")
         func_start = index(base_output(prog_start:), 'contains')
         if (func_start > 0) then
            func_start = prog_start + func_start + 7  ! Skip "contains"
            ! Find "end program"
            prog_end = index(base_output(func_start:), 'end program')
            if (prog_end > 0) then
               functions_part = base_output(func_start:func_start + prog_end - 2)
            else
               functions_part = base_output(func_start:)
            end if
         else
            ! No contains, just take everything before end program
            prog_end = index(base_output(prog_start:), 'end program')
            if (prog_end > 0) then
               functions_part = base_output(1:prog_start - 1)
            else
               functions_part = base_output
            end if
         end if
      else
         ! No program wrapper, return as-is
         functions_part = base_output
      end if
   end subroutine extract_functions

   ! Rename program in output
   subroutine rename_program(base_output, new_name, output)
      character(len=*), intent(in) :: base_output
      character(len=*), intent(in) :: new_name
      character(len=:), allocatable, intent(out) :: output
      integer :: prog_pos, end_prog_pos, name_start, name_end
      character(len=:), allocatable :: before_name, after_name

      prog_pos = index(base_output, 'program ')
      if (prog_pos == 0) then
         output = base_output
         return
      end if

      ! Find the old program name
      name_start = prog_pos + 8  ! After "program "
      name_end = name_start
      do while (name_end <= len(base_output))
         if (base_output(name_end:name_end) == new_line('A') .or. &
             base_output(name_end:name_end) == ' ') then
            exit
         end if
         name_end = name_end + 1
      end do
      name_end = name_end - 1

      ! Replace with new name
      before_name = base_output(1:name_start - 1)
      after_name = base_output(name_end + 1:)
      output = before_name//trim(new_name)//after_name

      ! Also replace in "end program"
      end_prog_pos = index(output, 'end program ')
      if (end_prog_pos > 0) then
         name_start = end_prog_pos + 12
         name_end = name_start
         do while (name_end <= len(output))
            if (output(name_end:name_end) == new_line('A') .or. &
                output(name_end:name_end) == ' ') then
               exit
            end if
            name_end = name_end + 1
         end do
         name_end = name_end - 1

         before_name = output(1:name_start - 1)
         after_name = output(name_end + 1:)
         output = before_name//trim(new_name)//after_name
      end if
   end subroutine rename_program

   ! Split output into functions and main code parts
   subroutine split_functions_and_main(base_output, functions_part, main_part)
      character(len=*), intent(in) :: base_output
      character(len=:), allocatable, intent(out) :: functions_part, main_part
      integer :: prog_start, func_end
      character(len=:), allocatable :: temp_main

      ! Find where functions end and program begins
      prog_start = index(base_output, 'program ')

      if (prog_start > 1) then
         functions_part = base_output(1:prog_start - 1)
         temp_main = base_output(prog_start:)

         ! Remove "program " and "end program" lines from main_part, keep content
         call extract_program_body(temp_main, main_part)
      else
         functions_part = ''
         main_part = base_output
      end if
   end subroutine split_functions_and_main

   ! Extract program body (remove program/end program lines)
   subroutine extract_program_body(program_text, body)
      character(len=*), intent(in) :: program_text
      character(len=:), allocatable, intent(out) :: body
      integer :: start_pos, end_pos, absolute_end_pos
      integer :: i

      ! Skip "program name" line
      start_pos = index(program_text, new_line('A'))
      if (start_pos == 0) then
         body = ''
         return
      end if
      start_pos = start_pos + 1

      ! Find "end program"
      end_pos = index(program_text(start_pos:), 'end program')
      if (end_pos > 0) then
         ! end_pos is relative to start_pos
         ! Convert to absolute position
         absolute_end_pos = start_pos + end_pos - 1
         ! Back up to start of line containing "end program"
         do i = absolute_end_pos - 1, start_pos, -1
            if (program_text(i:i) == new_line('A')) then
               absolute_end_pos = i
               exit
            end if
         end do
         ! Extract body up to (but not including) "end program" line
         if (absolute_end_pos > start_pos) then
            body = program_text(start_pos:absolute_end_pos - 1)
         else
            body = ''
         end if
      else
         body = program_text(start_pos:)
      end if
   end subroutine extract_program_body

   ! Remove external declarations (conflicts with module use)
   subroutine remove_external_declarations(input, output)
      character(len=*), intent(in) :: input
      character(len=:), allocatable, intent(out) :: output
      integer :: i
      character(len=:), allocatable :: line, trimmed, result, lowered
      logical :: skip_line

      result = ''
      i = 1

      do while (i <= len(input))
         line = extract_line(input, i)
         trimmed = trim(adjustl(line))
         lowered = to_lower_ascii_local(trimmed)

         ! Check if this line is an external declaration
         skip_line = .false.
         if (index(lowered, 'external') > 0) then
            if (index(lowered, 'external ::') > 0 .or. &
                index(lowered, 'external::') > 0 .or. &
                index(lowered, 'external ') == 1) then
               skip_line = .true.
            end if
         end if

         ! Add line if not skipping
         if (.not. skip_line) then
            if (len_trim(result) > 0) then
               result = trim(result)//new_line('A')//line
            else
               result = line
            end if
         end if
      end do

      output = result
   end subroutine remove_external_declarations

end module frontend_transformation
