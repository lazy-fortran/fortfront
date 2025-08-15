// F2023 Parser Grammar for fortfront validation
parser grammar Fortran2023Parser;
options { tokenVocab=Fortran2023Lexer; }

// Program structure
program : PROGRAM IDENTIFIER statement* END PROGRAM IDENTIFIER? ;
module_definition : MODULE IDENTIFIER statement* END MODULE IDENTIFIER? ;
submodule_definition : SUBMODULE LPAREN IDENTIFIER RPAREN IDENTIFIER statement* END SUBMODULE IDENTIFIER? ;

// Statements
statement : declaration
          | assignment
          | type_definition
          | procedure_definition
          | control_flow
          ;

// Declarations
declaration : type_spec DOUBLE_COLON? variable_list
            | type_spec COMMA attribute_list DOUBLE_COLON variable_list
            ;

type_spec : INTEGER
          | REAL
          | COMPLEX
          | LOGICAL
          | CHARACTER
          | CLASS LPAREN IDENTIFIER RPAREN
          | TYPE LPAREN IDENTIFIER RPAREN
          ;

attribute_list : attribute (COMMA attribute)* ;
attribute : IDENTIFIER ;

variable_list : variable (COMMA variable)* ;
variable : IDENTIFIER (LPAREN expression_list RPAREN)? ;

// F2023 Type definitions
type_definition : TYPE DOUBLE_COLON? IDENTIFIER
                    component*
                  END TYPE IDENTIFIER?
                | TYPE COMMA EXTENDS LPAREN IDENTIFIER RPAREN DOUBLE_COLON IDENTIFIER
                    component*
                  END TYPE IDENTIFIER?
                ;

component : type_spec DOUBLE_COLON variable_list ;

// Procedures
procedure_definition : subroutine_definition | function_definition ;

subroutine_definition : SUBROUTINE IDENTIFIER LPAREN parameter_list? RPAREN
                       statement*
                       END SUBROUTINE IDENTIFIER?
                     ;

function_definition : FUNCTION IDENTIFIER LPAREN parameter_list? RPAREN
                     statement*
                     END FUNCTION IDENTIFIER?
                   ;

parameter_list : parameter (COMMA parameter)* ;
parameter : IDENTIFIER ;

// Control flow
control_flow : if_construct | do_construct ;

if_construct : 'if' LPAREN expression RPAREN statement* 'end' 'if' ;
do_construct : 'do' statement* 'end' 'do' ;

// Expressions
expression : term ((PLUS | MINUS) term)* ;
term : factor ((MULTIPLY | DIVIDE) factor)* ;
factor : IDENTIFIER | INTEGER_LITERAL | REAL_LITERAL | STRING_LITERAL | LPAREN expression RPAREN ;

expression_list : expression (COMMA expression)* ;

// Assignment
assignment : IDENTIFIER ASSIGN expression ;
