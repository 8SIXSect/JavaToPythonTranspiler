"""
This module contains methods for transpiling source (Java) to source (Python).
"""

from dataclasses import dataclass
from io import TextIOWrapper
from typing import Optional
from java_to_python_transpiler.java_to_python import (
    ArgumentList, ArithmeticOperator, BlockStatement, ClassDeclaration,
    ComparisonExpression, ComparisonOperator, ExpressionNode, FactorNode,
    IfStatement, InlineStatement, MethodDeclaration, MethodDeclarationList,
    ParameterList, StatementList, LexerResult, MethodCall, ParserFailure,
    ParserResult, ReturnStatement, TermNode, LexerFailure, VariableIncrement,
    VariableInitialization, WhileStatement, emit_ast_into_output,
    scan_and_tokenize_input, parse_tokens, Node
)


PROMPT = ">>> "


@dataclass
class TranspilerFailure:
    """
    This class represents a failure that occurred in the Transpiler.

    `error_message` represents the error that occurred; it is a String of
    information that informs the user what went wrong.
    """

    error_message: str


def java_to_python_from_string(user_input: str) -> str | TranspilerFailure:
    """
    This is the primary function provided by this library.

    This function takes in a String (`user_input`) and transpiles it into a
    result.

    This function will return either a String (which is the python source), or
    it may fail, and return a TranspilerFailure object. This object contains a
    singular field, `error_message`, which is a String representing the error
    that occurred.
    """

    lexer_result: LexerResult = scan_and_tokenize_input(user_input)
    if isinstance(lexer_result, LexerFailure):
        return TranspilerFailure(lexer_result.error_message)

    assert isinstance(lexer_result, tuple)

    parser_result: ParserResult = parse_tokens(lexer_result)
    if isinstance(parser_result, ParserFailure):
        return TranspilerFailure(parser_result.error_messasge)

    return emit_ast_into_output(parser_result)


def java_to_python_from_file(file_path: Optional[str] = None,
                             file_object: Optional[TextIOWrapper] = None
                             ) -> str | TranspilerFailure:
    """
    This function is the secondary purpose of this library; it allows one to
    transpile the contents of a file (of java source code) to Python source.

    `file_path` (optional) is a String that defaults to None; if set to None,
    then `file_object` must have a value.

    `file_object` (optional) is a TextIOWrapper that defaults to None; if set
    to None, then `file_path` must have a value.

    if both `file_path` and `file_object` each have a value of None, then a
    ValueError will be thrown.
    """

    source_code: str | None = None

    if file_path is not None:
        with open(file_path, "r") as java_source_file:
            source_code = java_source_file.read()

    if file_object is not None:
        source_code = file_object.read()

    if source_code is None:
        raise ValueError("Both `file_path` and `file_object` are None")

    return java_to_python_from_string(source_code)
    

def test_parser(user_input: str):
    """
    This function is an internal tool for debugging the parser when given a
    user_input.
    """

    lexer_result: LexerResult = scan_and_tokenize_input(user_input)

    if isinstance(lexer_result, LexerFailure):
        print(lexer_result.error_message)
        return

    assert isinstance(lexer_result, tuple)
    
    parser_result: ParserResult = parse_tokens(lexer_result)

    if isinstance(parser_result, ParserFailure):
        print(parser_result.error_messasge)
        return

    format_ast(0, parser_result)


def format_ast(indent_level: int, node: Node | None):
    """
    This function is an internal tool for debugging the parser when given a
    `node`, it will print a formatted representation of the Node object.

    `indent_level` should be given a value of 0 to start off.
    """

    def print_output(output: str, with_pipe_symbol: bool = False):
        indent: str = " " * indent_level
        prefix: str = ("| " if with_pipe_symbol else "")
        
        print(indent + prefix + output)

    
    def format_ast_with_extra_indent(node: Node | ArithmeticOperator | None):
        extra_indent_level = indent_level + 4
        format_ast(extra_indent_level, node)


    match node:
        case ComparisonExpression(expression, operator, additional_expression):
            print_output("-> compare_expr")
            format_ast_with_extra_indent(expression)
            format_ast_with_extra_indent(operator)
            format_ast_with_extra_indent(additional_expression)

        case ExpressionNode(single_term_node, operator, additional_expression_node):
            print_output("-> expr")
            format_ast_with_extra_indent(single_term_node) 
            format_ast_with_extra_indent(operator)
            format_ast_with_extra_indent(additional_expression_node)

        case TermNode(single_factor_node, operator, additional_term_node):
            print_output("-> term")
            format_ast_with_extra_indent(single_factor_node)
            format_ast_with_extra_indent(operator)
            format_ast_with_extra_indent(additional_term_node)

        case FactorNode(number_or_identifier, None):
            print_output(number_or_identifier, True)

        case FactorNode("", method_call):
            format_ast_with_extra_indent(method_call)

        case MethodCall(identifier, argument_list):
            print_output("-> method_call")
            print_output(identifier, True)
            format_ast_with_extra_indent(argument_list)

        case ArgumentList(argument, additional_argument_list):
            print_output("-> argument_list")
            format_ast_with_extra_indent(argument)
            format_ast_with_extra_indent(additional_argument_list)

        case VariableInitialization(identifier, expression):
            print_output("-> variable_init")
            print_output(identifier, True)
            format_ast_with_extra_indent(expression)

        case ReturnStatement(expression):
            print_output("-> return_stmt")
            format_ast_with_extra_indent(expression)

        case VariableIncrement(identifier, expression):
            print_output("-> variable_inc")
            print_output(identifier, True)
            format_ast_with_extra_indent(expression)
        
        case InlineStatement(statement):
            print_output("-> inline_stmt")
            format_ast_with_extra_indent(statement)

        case WhileStatement(comparison_expression, statement_list):
            print_output("-> while_stmt")
            format_ast_with_extra_indent(comparison_expression)
            format_ast_with_extra_indent(statement_list)

        case IfStatement(comparison_expression, statement_list,
                         else_if_statement, else_statement_list):
            
            print_output("-> if_stmt")
            format_ast_with_extra_indent(comparison_expression)
            format_ast_with_extra_indent(statement_list)
            
            if else_if_statement is not None:
                print_output("-> else_if_stmt")
                format_ast_with_extra_indent(else_if_statement.comparison_expression)
                format_ast_with_extra_indent(else_if_statement.statement_list)
                format_ast_with_extra_indent(else_if_statement.additional_if_statement)
                format_ast_with_extra_indent(else_if_statement.else_clause)

            if else_statement_list is not None:
                print_output("-> else_stmt")
                format_ast_with_extra_indent(else_statement_list)

        case BlockStatement(statement):
            print_output("-> block_stmt")
            format_ast_with_extra_indent(statement)

        case StatementList(statement, additional_statement_list):
            print_output("-> stmt_list")
            format_ast_with_extra_indent(statement)
            format_ast_with_extra_indent(additional_statement_list)

        case ParameterList(identifier, additional_parameter_list):
            print_output("-> param_lst")
            if identifier is not None:
                print_output(identifier, True)
            format_ast_with_extra_indent(additional_parameter_list)
        
        case MethodDeclaration(identifier, parameter_list, statement_list):
            print_output("-> method_dec")
            print_output(identifier, True)
            format_ast_with_extra_indent(parameter_list)
            format_ast_with_extra_indent(statement_list)

        case MethodDeclarationList(method_declaration, additional_method_dec_list):
            print_output("-> method_dec_list")
            format_ast_with_extra_indent(method_declaration)
            format_ast_with_extra_indent(additional_method_dec_list)

        case ClassDeclaration(identifier, method_declaration_list):
            print_output("-> class_dec")
            print_output(identifier, True)
            format_ast_with_extra_indent(method_declaration_list)

        case _:
            OPERATOR_TYPES: tuple = (ArithmeticOperator, ComparisonOperator)
            if isinstance(node, OPERATOR_TYPES):
                print_output(node.value, True)

