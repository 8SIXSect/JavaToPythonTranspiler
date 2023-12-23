"""
This module contains methods for transpiling source to source
"""

from typing import List, Optional, Union
from java_to_python_transpiler.java_to_python import (ArgumentList, ArithmeticOperator, ExpressionNode, FactorNode, LexerResult, MethodCall, Node,
                                                      ParserFailure, ParserResult, TermNode,
                                                      Token,
                                                      LexerFailure,
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


def format_ast(indent: int, node: Node | ArithmeticOperator | None):
    extra_indent: int = indent + 4

    if isinstance(node, ExpressionNode):
        print(indent * " " + "-> expr")
        format_ast(extra_indent, node.single_term_node) 
        format_ast(extra_indent, node.operator)
        format_ast(extra_indent, node.additional_expression_node)

    elif isinstance(node, TermNode):
        print(indent * " " + "-> term")
        format_ast(extra_indent, node.single_factor_node)
        format_ast(extra_indent, node.operator)
        format_ast(extra_indent, node.additional_term_node)

    elif isinstance(node, ArithmeticOperator):
        print(indent * " " + "|" + " " + node.value)

    elif isinstance(node, FactorNode):

        if node.method_call is None:
            print(indent * " " + "|" + " " + node.number_or_identifier)
        else:
            format_ast(extra_indent, node.method_call)

    elif isinstance(node, MethodCall):
        print(indent * " " + "-> method_call")
        print(indent * " " + "|" + " " + node.identifier)
        format_ast(extra_indent, node.argument_list)

    elif isinstance(node, ArgumentList):
        print(indent * " " + "-> argument_list")
        format_ast(extra_indent, node.argument)
        format_ast(extra_indent, node.additional_argument_list)

