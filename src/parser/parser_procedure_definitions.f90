module parser_procedure_definitions_module
   ! Parser module for function, subroutine, and interface definitions
   use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
                         TK_STRING, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                         TK_WHITESPACE
   use parser_state_module, only: parser_state_t, create_parser_state
   use parser_parameter_handling_module, only: parse_typed_parameters, &
                                               merge_parameter_attributes
   use parser_statement_utilities_module, only: parse_statement_in_if_block
   use parser_expressions_module, only: parse_comparison
   use parser_prefix_state, only: consume_pending_prefixes
   use ast_arena_modern, only: ast_arena_t
   use ast_factory, only: push_function_def, push_subroutine_def, push_interface_block
   use ast_factory
   implicit none
   private

   public :: parse_function_definition, parse_subroutine_definition, parse_interface_block

contains

   function parse_function_definition(parser, arena) result(func_index)
      type(parser_state_t), intent(inout) :: parser
      type(ast_arena_t), intent(inout) :: arena
      integer :: func_index

      type(token_t) :: token
      character(len=:), allocatable :: function_name, return_type_str, result_variable_name
      integer :: line, column
      integer, allocatable :: param_indices(:), body_indices(:)
      logical :: has_recursive_keyword
      logical :: infer_recursive_from_body
      character(len=16), allocatable :: prefix_keywords(:)
      integer :: i
      character(len=16), allocatable :: pending_prefixes(:)

      ! Initialize
      return_type_str = ""
      result_variable_name = ""
      has_recursive_keyword = .false.
      infer_recursive_from_body = .false.

      allocate (character(len=16) :: prefix_keywords(0))
      call consume_pending_prefixes(pending_prefixes)
      if (allocated(pending_prefixes)) then
         if (size(pending_prefixes) > 0) then
            do i = 1, size(pending_prefixes)
               call append_prefix_keyword(prefix_keywords, pending_prefixes(i))
               if (trim(pending_prefixes(i)) == "recursive") then
                  has_recursive_keyword = .true.
               end if
            end do
         end if
      end if

      ! Optional prefix keywords before "function"
      do
         token = parser%peek()
         if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
            select case (trim(to_lower_local(token%text)))
            case ("recursive")
               has_recursive_keyword = .true.
               call append_prefix_keyword(prefix_keywords, "recursive")
               token = parser%consume()
            case ("pure")
               call append_prefix_keyword(prefix_keywords, "pure")
               token = parser%consume()
            case ("elemental")
               call append_prefix_keyword(prefix_keywords, "elemental")
               token = parser%consume()
            case default
               exit
            end select
         else
            exit
         end if
      end do

      ! Check if we have a return type before "function"
      token = parser%peek()
      if (token%kind == TK_KEYWORD) then
         select case (trim(to_lower_local(token%text)))
         case ("real", "integer", "logical", "character")
            return_type_str = token%text
            token = parser%consume()
         end select
      end if

      ! Consume function keyword
      token = parser%peek()
      if (token%kind == TK_KEYWORD .and. token%text == "function") then
         line = token%line
         column = token%column
         token = parser%consume()
      else
         func_index = 0
         return
      end if

      ! Get function name
      token = parser%peek()
      if (token%kind == TK_IDENTIFIER) then
         function_name = token%text
         token = parser%consume()
      else if (token%kind == TK_KEYWORD .and. &
               keyword_can_be_function_name(parser, token)) then
         function_name = token%text
         token = parser%consume()
      else
         function_name = "unnamed_function"
      end if

      ! Parse parameters with protective error handling
      token = parser%peek()
      if (token%kind == TK_OPERATOR .and. token%text == "(") then
         token = parser%consume()
         ! Parse typed parameters safely
         call parse_typed_parameters(parser, arena, param_indices)
         token = parser%peek()
         if (token%kind == TK_OPERATOR .and. token%text == ")") then
            token = parser%consume()
         end if
      else
         allocate (param_indices(0))
      end if

      ! Check for result clause
      token = parser%peek()
      if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
         token = parser%consume()
         token = parser%peek()
         if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
               result_variable_name = token%text
               token = parser%consume()
            end if
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
               token = parser%consume()
            end if
         end if
      end if

      ! Parse function body until "end function"
      allocate (body_indices(0))

      do while (.not. parser%is_at_end())
         token = parser%peek()

         ! Check for end of function
         if (token%kind == TK_KEYWORD .and. token%text == "end") then
            ! Look ahead for "function"
            block
               type(token_t), allocatable, target :: all_tokens(:)
               integer :: next_idx
               if (associated(parser%tokens)) then
                  allocate (all_tokens(size(parser%tokens)))
                  all_tokens = parser%tokens
               else
                  allocate (all_tokens(0))
               end if
               next_idx = parser%current_token + 1
               if (next_idx <= size(all_tokens)) then
                  if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                      all_tokens(next_idx)%text == "function") then
                     ! Consume "end function"
                     token = parser%consume()  ! consume "end"
                     token = parser%consume()  ! consume "function"
                     ! Optionally consume function name
                     if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER .and. &
                            token%text == function_name) then
                           token = parser%consume()
                        end if
                     end if
                     exit  ! Exit the body parsing loop
                  end if
               end if
            end block
         end if

         ! Skip empty lines
         if (token%kind == TK_NEWLINE) then
            token = parser%consume()
            cycle
         end if

         ! Collect tokens for the current statement line
         block
            type(token_t), allocatable, target :: stmt_tokens(:), all_tokens(:)
            integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
            type(parser_state_t) :: block_parser

            if (associated(parser%tokens)) then
               allocate (all_tokens(size(parser%tokens)))
               all_tokens = parser%tokens
            else
               allocate (all_tokens(0))
            end if
            stmt_start = parser%current_token
            stmt_end = stmt_start

            if (token%kind == TK_KEYWORD .and. token%text == "if") then
               block
                  integer :: depth, pos
                  logical :: preceded_by_end, preceded_by_else

                  depth = 0
                  pos = stmt_start
                  do while (pos <= size(all_tokens))
                     if (all_tokens(pos)%kind == TK_KEYWORD) then
                        select case (all_tokens(pos)%text)
                        case ("if")
                           preceded_by_end = .false.
                           preceded_by_else = .false.
                           if (pos > 1) then
                              if (all_tokens(pos - 1)%kind == TK_KEYWORD) then
                                 if (all_tokens(pos - 1)%text == "end") preceded_by_end = .true.
                                 if (all_tokens(pos - 1)%text == "else") preceded_by_else = .true.
                              end if
                           end if
                           if (.not. preceded_by_end .and. .not. preceded_by_else) then
                              depth = depth + 1
                           end if
                        case ("end")
                           if (pos < size(all_tokens)) then
                              if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                                  all_tokens(pos + 1)%text == "if") then
                                 depth = depth - 1
                                 if (depth <= 0) then
                                    stmt_end = min(size(all_tokens), pos + 1)
                                    exit
                                 end if
                              end if
                           end if
                        end select
                     end if
                     stmt_end = pos
                     pos = pos + 1
                  end do
               end block
            else
               do i = stmt_start, size(all_tokens)
                  if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                  stmt_end = i
               end do
            end if

            ! Extract statement tokens
            stmt_size = stmt_end - stmt_start + 1
            if (stmt_size > 0) then
               allocate (stmt_tokens(stmt_size + 1))
               stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
               ! Add EOF token
               stmt_tokens(stmt_size + 1)%kind = TK_EOF
               stmt_tokens(stmt_size + 1)%text = ""
               stmt_tokens(stmt_size + 1)%line = token%line
               stmt_tokens(stmt_size + 1)%column = token%column + 1

               if (.not. infer_recursive_from_body) then
                  do i = 1, stmt_size
                     if (stmt_tokens(i)%kind == TK_IDENTIFIER) then
                        if (trim(stmt_tokens(i)%text) == trim(function_name)) then
                           if (i < stmt_size) then
                              if (stmt_tokens(i + 1)%kind == TK_OPERATOR .and. &
                                  stmt_tokens(i + 1)%text == "(") then
                                 infer_recursive_from_body = .true.
                                 exit
                              end if
                           end if
                        end if
                     end if
                  end do
               end if

               ! Parse the statement (handle multi-line IF blocks)
               if (token%kind == TK_KEYWORD .and. (token%text == "if" .or. token%text == "IF")) then
                  stmt_index = parse_if_statement_tokens(stmt_tokens, arena)
                  if (stmt_index <= 0) then
                     block_parser = create_parser_state(stmt_tokens)
                     stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
                  end if
               else
                  block_parser = create_parser_state(stmt_tokens)
                  stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
               end if

               ! Add to body
               if (stmt_index > 0) then
                  body_indices = [body_indices, stmt_index]
               end if

               ! Advance parser position
               parser%current_token = stmt_end + 1
            end if
         end block
      end do

      ! Merge parameter attributes from body declarations
      if (allocated(param_indices) .and. allocated(body_indices)) then
         if (size(param_indices) > 0 .and. size(body_indices) > 0) then
            call merge_parameter_attributes(arena, param_indices, body_indices)
         end if
      end if

      ! Create function node
      if (.not. has_recursive_keyword .and. infer_recursive_from_body) then
         has_recursive_keyword = .true.
         call append_prefix_keyword(prefix_keywords, "recursive")
      end if

      func_index = push_function_def(arena, function_name, param_indices, &
                                     return_type_str, body_indices, &
                                     line, column, result_variable=result_variable_name, &
                                     is_recursive=has_recursive_keyword, &
                                     prefix_keywords=prefix_keywords)
   end function parse_function_definition

   function parse_if_statement_tokens(stmt_tokens, arena) result(if_index)
      type(token_t), intent(in) :: stmt_tokens(:)
      type(ast_arena_t), intent(inout) :: arena
      integer :: if_index
      integer :: token_count
      integer :: then_pos, else_pos, end_pos
      integer :: i, condition_length
      type(token_t), allocatable, target :: condition_tokens(:)
      type(parser_state_t) :: condition_parser
      integer :: condition_index
      integer, allocatable :: then_body_indices(:), else_body_indices(:)
      integer :: then_start, then_end, else_start, else_end

      token_count = size(stmt_tokens)
      if (token_count <= 1) then
         if_index = 0
         return
      end if
      token_count = token_count - 1  ! Ignore EOF token

      then_pos = -1
      else_pos = -1
      end_pos = -1

      do i = 2, token_count
         if (stmt_tokens(i)%kind == TK_KEYWORD) then
            select case (stmt_tokens(i)%text)
            case ("then", "THEN")
               if (then_pos < 0) then_pos = i
            case ("else", "ELSE")
               if (else_pos < 0) else_pos = i
            case ("end", "END")
               if (i < token_count) then
                  if (stmt_tokens(i + 1)%kind == TK_KEYWORD .and. &
                      (stmt_tokens(i + 1)%text == "if" .or. stmt_tokens(i + 1)%text == "IF")) then
                     end_pos = i
                     exit
                  end if
               end if
            end select
         end if
      end do

      if (then_pos < 0 .or. end_pos < 0) then
         if_index = 0
         return
      end if

      condition_length = then_pos - 2
      if (condition_length >= 1) then
         allocate (condition_tokens(condition_length + 1))
         condition_tokens(1:condition_length) = stmt_tokens(2:then_pos - 1)
         condition_tokens(condition_length + 1)%kind = TK_EOF
         condition_tokens(condition_length + 1)%text = ""
         condition_tokens(condition_length + 1)%line = stmt_tokens(2)%line
         condition_tokens(condition_length + 1)%column = stmt_tokens(2)%column

         condition_parser = create_parser_state(condition_tokens)
         condition_index = parse_comparison(condition_parser, arena)
         deallocate (condition_tokens)
      else
         condition_index = 0
      end if

      then_start = then_pos + 1
      if (else_pos > 0) then
         then_end = else_pos - 1
      else
         then_end = end_pos - 1
      end if
      then_body_indices = parse_if_body_tokens(stmt_tokens, then_start, then_end, arena)

      if (else_pos > 0) then
         else_start = else_pos + 1
         else_end = end_pos - 1
         else_body_indices = parse_if_body_tokens(stmt_tokens, else_start, else_end, arena)
      else
         allocate (else_body_indices(0))
      end if

      if_index = push_if(arena, condition_index, then_body_indices, &
                         else_body_indices=else_body_indices, &
                         line=stmt_tokens(1)%line, column=stmt_tokens(1)%column)
   end function parse_if_statement_tokens

   function parse_if_body_tokens(stmt_tokens, start_idx, end_idx, arena) &
      result(body_indices)
      type(token_t), intent(in) :: stmt_tokens(:)
      integer, intent(in) :: start_idx, end_idx
      type(ast_arena_t), intent(inout) :: arena
      integer, allocatable :: body_indices(:)
      integer :: body_len
      type(token_t), allocatable, target :: body_tokens(:), line_tokens(:)
      type(parser_state_t) :: body_parser, line_parser
      integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
      type(token_t) :: token

      if (end_idx < start_idx) then
         allocate (body_indices(0))
         return
      end if

      body_len = end_idx - start_idx + 1
      if (body_len <= 0) then
         allocate (body_indices(0))
         return
      end if

      allocate (body_tokens(body_len + 1))
      body_tokens(1:body_len) = stmt_tokens(start_idx:end_idx)
      body_tokens(body_len + 1)%kind = TK_EOF
      body_tokens(body_len + 1)%text = ""
      body_tokens(body_len + 1)%line = stmt_tokens(start_idx)%line
      body_tokens(body_len + 1)%column = stmt_tokens(start_idx)%column

      body_parser = create_parser_state(body_tokens)
      allocate (body_indices(0))

      do while (.not. body_parser%is_at_end())
         token = body_parser%peek()
         if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE) then
            token = body_parser%consume()
            cycle
         end if
         if (token%kind == TK_EOF) exit

         stmt_start = body_parser%current_token
         stmt_end = stmt_start

         do i = stmt_start, size(body_tokens)
            if (i > stmt_start .and. body_tokens(i)%line /= token%line) exit
            stmt_end = i
         end do

         stmt_size = stmt_end - stmt_start + 1
         if (stmt_size > 0) then
            allocate (line_tokens(stmt_size + 1))
            line_tokens(1:stmt_size) = body_tokens(stmt_start:stmt_end)
            line_tokens(stmt_size + 1)%kind = TK_EOF
            line_tokens(stmt_size + 1)%text = ""
            line_tokens(stmt_size + 1)%line = token%line
            line_tokens(stmt_size + 1)%column = token%column

            line_parser = create_parser_state(line_tokens)
            stmt_index = parse_statement_in_if_block(line_parser, arena, line_tokens(1))
            if (stmt_index > 0) then
               body_indices = [body_indices, stmt_index]
            end if
            deallocate (line_tokens)
            body_parser%current_token = stmt_end + 1
         else
            body_parser%current_token = body_parser%current_token + 1
         end if
      end do

      deallocate (body_tokens)
   end function parse_if_body_tokens

   subroutine append_prefix_keyword(prefixes, value)
      character(len=16), allocatable, intent(inout) :: prefixes(:)
      character(len=*), intent(in) :: value
      integer :: n, i
      character(len=16), allocatable :: temp(:)
      logical :: already_present

      already_present = .false.
      if (allocated(prefixes)) then
         do i = 1, size(prefixes)
            if (trim(prefixes(i)) == trim(value)) then
               already_present = .true.
               exit
            end if
         end do
      else
         allocate (character(len=16) :: prefixes(0))
      end if

      if (already_present) return

      n = size(prefixes)
      allocate (character(len=16) :: temp(n + 1))
      if (n > 0) temp(1:n) = prefixes
      temp(n + 1) = trim(value)
      call move_alloc(temp, prefixes)
   end subroutine append_prefix_keyword

   function parse_subroutine_definition(parser, arena) result(sub_index)
      type(parser_state_t), intent(inout) :: parser
      type(ast_arena_t), intent(inout) :: arena
      integer :: sub_index

      type(token_t) :: token
      character(len=:), allocatable :: subroutine_name
      integer :: line, column
      integer, allocatable :: param_indices(:), body_indices(:)

      ! Consume subroutine keyword
      token = parser%consume()
      line = token%line
      column = token%column

      ! Get subroutine name
      token = parser%peek()
      if (token%kind == TK_IDENTIFIER) then
         subroutine_name = token%text
         token = parser%consume()
      else
         subroutine_name = "unnamed_subroutine"
      end if

      ! Parse parameters
      token = parser%peek()
      if (token%kind == TK_OPERATOR .and. token%text == "(") then
         token = parser%consume()
         call parse_typed_parameters(parser, arena, param_indices)
         token = parser%peek()
         if (token%kind == TK_OPERATOR .and. token%text == ")") then
            token = parser%consume()
         end if
      else
         allocate (param_indices(0))
      end if

      ! Parse subroutine body until "end subroutine"
      allocate (body_indices(0))

      do while (.not. parser%is_at_end())
         token = parser%peek()

         ! Check for end of subroutine
         if (token%kind == TK_KEYWORD .and. token%text == "end") then
            ! Look ahead for "subroutine"
            block
               type(token_t), allocatable, target :: all_tokens(:)
               integer :: next_idx
               if (associated(parser%tokens)) then
                  allocate (all_tokens(size(parser%tokens)))
                  all_tokens = parser%tokens
               else
                  allocate (all_tokens(0))
               end if
               next_idx = parser%current_token + 1
               if (next_idx <= size(all_tokens)) then
                  if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                      all_tokens(next_idx)%text == "subroutine") then
                     ! Consume "end subroutine"
                     token = parser%consume()  ! consume "end"
                     token = parser%consume()  ! consume "subroutine"
                     ! Optionally consume subroutine name
                     if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER .and. &
                            token%text == subroutine_name) then
                           token = parser%consume()
                        end if
                     end if
                     exit  ! Exit the body parsing loop
                  end if
               end if
            end block
         end if

         ! Skip empty lines
         if (token%kind == TK_NEWLINE) then
            token = parser%consume()
            cycle
         end if

         ! Collect tokens for the current statement line
         block
            type(token_t), allocatable, target :: stmt_tokens(:), all_tokens(:)
            integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
            type(parser_state_t) :: block_parser

            if (associated(parser%tokens)) then
               allocate (all_tokens(size(parser%tokens)))
               all_tokens = parser%tokens
            else
               allocate (all_tokens(0))
            end if
            stmt_start = parser%current_token
            stmt_end = stmt_start

            if (token%kind == TK_KEYWORD .and. token%text == "if") then
               block
                  integer :: depth, pos
                  logical :: preceded_by_end, preceded_by_else

                  depth = 0
                  pos = stmt_start
                  do while (pos <= size(all_tokens))
                     if (all_tokens(pos)%kind == TK_KEYWORD) then
                        select case (all_tokens(pos)%text)
                        case ("if")
                           preceded_by_end = .false.
                           preceded_by_else = .false.
                           if (pos > 1) then
                              if (all_tokens(pos - 1)%kind == TK_KEYWORD) then
                                 if (all_tokens(pos - 1)%text == "end") preceded_by_end = .true.
                                 if (all_tokens(pos - 1)%text == "else") preceded_by_else = .true.
                              end if
                           end if
                           if (.not. preceded_by_end .and. .not. preceded_by_else) then
                              depth = depth + 1
                           end if
                        case ("end")
                           if (pos < size(all_tokens)) then
                              if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                                  all_tokens(pos + 1)%text == "if") then
                                 depth = depth - 1
                                 if (depth <= 0) then
                                    stmt_end = min(size(all_tokens), pos + 1)
                                    exit
                                 end if
                              end if
                           end if
                        end select
                     end if
                     stmt_end = pos
                     pos = pos + 1
                  end do
               end block
            else
               do i = stmt_start, size(all_tokens)
                  if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                  stmt_end = i
               end do
            end if

            ! Extract statement tokens
            stmt_size = stmt_end - stmt_start + 1
            if (stmt_size > 0) then
               allocate (stmt_tokens(stmt_size + 1))
               stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
               ! Add EOF token
               stmt_tokens(stmt_size + 1)%kind = TK_EOF
               stmt_tokens(stmt_size + 1)%text = ""
               stmt_tokens(stmt_size + 1)%line = token%line
               stmt_tokens(stmt_size + 1)%column = token%column + 1

               ! Parse the statement
               block_parser = create_parser_state(stmt_tokens)
               stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))

               ! Add to body
               if (stmt_index > 0) then
                  body_indices = [body_indices, stmt_index]
               end if

               ! Advance parser position
               parser%current_token = stmt_end + 1
            end if
         end block
      end do

      ! Merge parameter attributes from body declarations
      if (allocated(param_indices) .and. allocated(body_indices)) then
         if (size(param_indices) > 0 .and. size(body_indices) > 0) then
            call merge_parameter_attributes(arena, param_indices, body_indices)
         end if
      end if

      ! Create subroutine node
      sub_index = push_subroutine_def(arena, subroutine_name, param_indices, body_indices, &
                                      line, column)
   end function parse_subroutine_definition

   function parse_interface_block(parser, arena) result(interface_index)
      type(parser_state_t), intent(inout) :: parser
      type(ast_arena_t), intent(inout) :: arena
      integer :: interface_index

      type(token_t) :: token
      character(len=:), allocatable :: interface_name
      integer :: line, column
      integer, allocatable :: body_indices(:)

      ! Consume interface keyword
      token = parser%consume()
      line = token%line
      column = token%column

      ! Get interface name (optional)
      token = parser%peek()
      if (token%kind == TK_IDENTIFIER) then
         token = parser%consume()
         interface_name = token%text
      else
         interface_name = ""
      end if

      ! Simplified parsing for refactoring
      allocate (body_indices(0))

      ! Create interface node
      interface_index = push_interface_block(arena, interface_name, body_indices, &
                                             line, column)
   end function parse_interface_block

   logical function keyword_can_be_function_name(parser, token) result(can_use)
      type(parser_state_t), intent(in) :: parser
      type(token_t), intent(in) :: token
      type(token_t) :: lookahead
      character(len=len(token%text)) :: token_lower
      character(len=:), allocatable :: next_lower
      integer :: next_index

      token_lower = to_lower_local(token%text)
      can_use = .false.

      select case (trim(token_lower))
      case ('double')
         next_index = parser%current_token + 1
         lookahead = parser%get_token_at_index(next_index)
         next_lower = to_lower_local(trim(lookahead%text))
         if (next_lower /= 'precision') then
            can_use = .true.
         end if
      case default
         can_use = .false.
      end select
   end function keyword_can_be_function_name

   pure function to_lower_local(value) result(lower_value)
      character(len=*), intent(in) :: value
      character(len=len(value)) :: lower_value
      integer :: i, code

      lower_value = value
      do i = 1, len(lower_value)
         code = iachar(lower_value(i:i))
         if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower_value(i:i) = achar(code + 32)
         end if
      end do
   end function to_lower_local

end module parser_procedure_definitions_module
