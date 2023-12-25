"""
This module contains methods for transpiling source to source
"""

from typing import Callable, List, Optional, Union
from java_to_python_transpiler.java_to_python import (
    ArgumentList, ArithmeticOperator,
    ExpressionNode, FactorNode, InlineStatement, LexerResult, MethodCall, NodeResult, NodeSuccess,
    ParserFailure, ParserResult, ReturnStatement, TermNode,
    LexerFailure, VariableInitialization, parse_tokens_for_inline_statement, parse_tokens_for_return_statement,
    parse_tokens_for_variable_initialization,
    scan_and_tokenize_input,
    parse_list_of_tokens
)


PROMPT = ">>> "


def test_parser(): 
    user_input = "meth(123, 456)"

    lexer_result: LexerResult = scan_and_tokenize_input(user_input)

    if isinstance(lexer_result, LexerFailure):
        print(lexer_result.error_message)
        return

    assert isinstance(lexer_result, tuple)

    parser_result: ParserResult = parse_list_of_tokens(lexer_result)

    if isinstance(parser_result, ParserFailure):
        print(parser_result.error_messasge)
        return

    format_ast(0, parser_result)


def test_inline_statement():

    while True: 
        user_input: str = input(PROMPT)

        lexer_result: LexerResult = scan_and_tokenize_input(user_input)

        if isinstance(lexer_result, LexerFailure):
            print(lexer_result.error_message)
            return
 
        assert isinstance(lexer_result, tuple)

        node_result: NodeResult = parse_tokens_for_inline_statement(lexer_result)

        assert isinstance(node_result, NodeSuccess)

        format_ast(0, node_result.node)


Node = Union[
    ExpressionNode, TermNode, FactorNode,
    MethodCall, ArgumentList,
    VariableInitialization, ReturnStatement,
    InlineStatement,
]


def format_ast(indent_level: int, node: Node | ArithmeticOperator | None):

    def print_output(output: str, with_pipe_symbol: bool = False):
        indent: str = " " * indent_level
        prefix: str = ("| " if with_pipe_symbol else "")
        
        print(indent + prefix + output)

    
    def format_ast_with_extra_indent(node: Node | ArithmeticOperator | None):
        extra_indent_level = indent_level + 4
        format_ast(extra_indent_level, node)

    match node:
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

        case (
                ArithmeticOperator.PLUS, ArithmeticOperator.MINUS,
                ArithmeticOperator.MULTIPLY, ArithmeticOperator.DIVIDE
            ):

            print_output(node.value, True)

        case FactorNode(number_or_identifier, None):
            print_output(number_or_identifier, True)

        case FactorNode(None, method_call):
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

        case InlineStatement(statement):
            print_output("-> inline_stmt")
            format_ast_with_extra_indent(statement)

        case _:
            pass

