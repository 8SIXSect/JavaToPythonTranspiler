"""
This module contains methods for transpiling source to source
"""

from typing import Callable, List, Optional, Union
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


Node = Union[
    ExpressionNode, TermNode, FactorNode,
    MethodCall, ArgumentList,
    VariableInitialization, ReturnStatement,

    ]


def format_ast(indent_level: int, node: Node | ArithmeticOperator | None):

    def print_output(output: str, with_pipe_symbol: bool = False):
        indent: str = " " * indent_level
        prefix: str = ("| " if with_pipe_symbol else "")
        
        print(indent + prefix + output)

    
    def format_ast_with_extra_indent(node: Node | ArithmeticOperator | None):
        extra_indent_level = indent_level + 4
        format_ast(extra_indent_level, node)


    if isinstance(node, ExpressionNode):
        print_output("-> expr")
        format_ast_with_extra_indent(node.single_term_node) 
        format_ast_with_extra_indent(node.operator)
        format_ast_with_extra_indent(node.additional_expression_node)

    elif isinstance(node, TermNode):
        print_output("-> term")
        format_ast_with_extra_indent(node.single_factor_node)
        format_ast_with_extra_indent(node.operator)
        format_ast_with_extra_indent(node.additional_term_node)

    elif isinstance(node, ArithmeticOperator):
        print_output(node.value, True)

    elif isinstance(node, FactorNode):

        if node.method_call is None:
            print_output(node.number_or_identifier, True)
        else:
            format_ast_with_extra_indent(node.method_call)

    elif isinstance(node, MethodCall):
        print_output("-> method_call")
        print_output(node.identifier, True)
        format_ast_with_extra_indent(node.argument_list)

    elif isinstance(node, ArgumentList):
        print_output("-> argument_list")
        format_ast_with_extra_indent(node.argument)
        format_ast_with_extra_indent(node.additional_argument_list)

    elif isinstance(node, VariableInitialization):
        print_output("-> variable_init")
        print_output(node.identifier, True)
        format_ast_with_extra_indent(node.expression)

    elif isinstance(node, ReturnStatement):
        print_output("-> return_stmt")
        format_ast_with_extra_indent(node.expression)

