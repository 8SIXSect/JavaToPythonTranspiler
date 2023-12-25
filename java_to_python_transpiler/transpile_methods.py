"""
This module contains methods for transpiling source to source
"""

from typing import List, Optional, Union
from java_to_python_transpiler.java_to_python import (
    ArgumentList, ArithmeticOperator,
    ExpressionNode, FactorNode, LexerResult, MethodCall, NodeResult, NodeSuccess,
    ParserFailure, ParserResult, ReturnStatement, TermNode,
    LexerFailure, VariableInitialization, parse_tokens_for_return_statement,
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


def test_return_statement():

    while True: 
        user_input: str = input(PROMPT)

        lexer_result: LexerResult = scan_and_tokenize_input(user_input)

        if isinstance(lexer_result, LexerFailure):
            print(lexer_result.error_message)
            return
 
        assert isinstance(lexer_result, tuple)

        node_result: NodeResult = parse_tokens_for_return_statement(lexer_result)

        assert isinstance(node_result, NodeSuccess)

        format_ast(0, node_result.node)

# OMFG ADD CURRYING IN HERE
def print_with_indent(indent_level: int, output: str):
    indent: str = indent_level * " "
    print(indent + output)


Node = Union[
    ExpressionNode, TermNode, FactorNode,
    MethodCall, ArgumentList,
    VariableInitialization, ReturnStatement,

    ]

def format_ast(indent_level: int, node: Node | ArithmeticOperator | None):
    extra_indent_level = indent_level + 4

    if isinstance(node, ExpressionNode):
        print_with_indent(indent_level, "-> expr")
        format_ast(extra_indent_level, node.single_term_node) 
        format_ast(extra_indent_level, node.operator)
        format_ast(extra_indent_level, node.additional_expression_node)

    elif isinstance(node, TermNode):
        print_with_indent(indent_level, "-> term")
        format_ast(extra_indent_level, node.single_factor_node)
        format_ast(extra_indent_level, node.operator)
        format_ast(extra_indent_level, node.additional_term_node)

    elif isinstance(node, ArithmeticOperator):
        print_with_indent(indent_level, "|" + " " + node.value)

    elif isinstance(node, FactorNode):

        if node.method_call is None:
            print_with_indent(indent_level, "|" + " " + node.number_or_identifier)
        else:
            format_ast(extra_indent_level, node.method_call)

    elif isinstance(node, MethodCall):
        print_with_indent(indent_level, "-> method_call")
        print_with_indent(indent_level, "|" + " " + node.identifier)
        format_ast(extra_indent_level, node.argument_list)

    elif isinstance(node, ArgumentList):
        print_with_indent(indent_level, "-> argument_list")
        format_ast(extra_indent_level, node.argument)
        format_ast(extra_indent_level, node.additional_argument_list)

    elif isinstance(node, VariableInitialization):
        print_with_indent(indent_level, "-> variable_init")
        print_with_indent(indent_level, "|" + " " + node.identifier)
        format_ast(extra_indent_level, node.expression)

    elif isinstance(node, ReturnStatement):
        print_with_indent(indent_level, "-> return_stmt")
        format_ast(extra_indent_level, node.expression)

