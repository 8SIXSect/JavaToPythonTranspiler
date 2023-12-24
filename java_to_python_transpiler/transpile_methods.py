"""
This module contains methods for transpiling source to source
"""

from typing import List, Optional, Union
from java_to_python_transpiler.java_to_python import (ArgumentList, ArithmeticOperator, ExpressionNode, FactorNode, LexerResult, MethodCall, Node, NodeResult, NodeSuccess,
                                                      ParserFailure, ParserResult, TermNode,
                                                      Token,
                                                      LexerFailure, VariableInitialization, parse_tokens_for_variable_initialization,
                                                      scan_and_tokenize_input,
                                                      parse_list_of_tokens
                                                      )


PROMPT = ">>> "


def test_parser(): 
    user_input: str = "meth(123, 456)"

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


def test_variable_initialization():
    PROMPT: str = "? - "

    while True: 
        user_input: str = input(PROMPT)

        lexer_result: LexerResult = scan_and_tokenize_input(user_input)

        if isinstance(lexer_result, LexerFailure):
            print(lexer_result.error_message)
            return
 
        assert isinstance(lexer_result, tuple)

        node_result: NodeResult = parse_tokens_for_variable_initialization(lexer_result)

        assert isinstance(node_result, NodeSuccess)

        format_ast(0, node_result.node)


def print_with_indent(indent_level: int, output: str):
    indent: str = indent_level * " "
    print(indent + output)


def format_ast(indent: int, node: Node | ArithmeticOperator | None):
    extra_indent: int = indent + 4

    if isinstance(node, ExpressionNode):
        print_with_indent(indent, "-> expr")
        format_ast(extra_indent, node.single_term_node) 
        format_ast(extra_indent, node.operator)
        format_ast(extra_indent, node.additional_expression_node)

    elif isinstance(node, TermNode):
        print_with_indent(indent, "-> term")
        format_ast(extra_indent, node.single_factor_node)
        format_ast(extra_indent, node.operator)
        format_ast(extra_indent, node.additional_term_node)

    elif isinstance(node, ArithmeticOperator):
        print_with_indent(indent, "|" + " " + node.value)

    elif isinstance(node, FactorNode):

        if node.method_call is None:
            print_with_indent(indent, "|" + " " + node.number_or_identifier)
        else:
            format_ast(extra_indent, node.method_call)

    elif isinstance(node, MethodCall):
        print_with_indent(indent, "-> method_call")
        print_with_indent(indent, "|" + " " + node.identifier)
        format_ast(extra_indent, node.argument_list)

    elif isinstance(node, ArgumentList):
        print_with_indent(indent, "-> argument_list")
        format_ast(extra_indent, node.argument)
        format_ast(extra_indent, node.additional_argument_list)

    elif isinstance(node, VariableInitialization):
        print_with_indent(indent, "-> variable_init")
        print_with_indent(indent, "|" + " " + node.identifier)
        format_ast(extra_indent, node.expression)

