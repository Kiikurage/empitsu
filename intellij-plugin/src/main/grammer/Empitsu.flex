// Copyright 2000-2022 JetBrains s.r.o. and other contributors. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package com.example.empitsuintellijplugin;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.example.empitsuintellijplugin.psi.EmpitsuTypes;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;

%%

%class EmpitsuLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

%state IN_BLOCK_COMMENT
%state IN_STRING_LITERAL

%%

<IN_BLOCK_COMMENT> {
	"*/" 							{ yybegin(YYINITIAL); return EmpitsuTypes.COMMENT; }
	[^] 							{ return EmpitsuTypes.COMMENT; }
}

<IN_STRING_LITERAL> {
	"\""				 			{ yybegin(YYINITIAL); return EmpitsuTypes.DOUBLE_QUOTE; }
	[^] 							{ return EmpitsuTypes.STRING_CHARACTER; }
}

<YYINITIAL> {
	[ \r\t\n]+ 							  { return EmpitsuTypes.WHITE_SPACE; }
	"//" [^\r\n]* 						{ return EmpitsuTypes.COMMENT; }
	"/*" 											{ yybegin(IN_BLOCK_COMMENT); return EmpitsuTypes.COMMENT; }
	"\"" 											{ yybegin(IN_STRING_LITERAL); return EmpitsuTypes.DOUBLE_QUOTE; }
	\d+("." \d+)? 	 				  { return EmpitsuTypes.NUMBER; }
	"if" 											{ return EmpitsuTypes.IF; }
	"let" 										{ return EmpitsuTypes.LET; }
	"for" 										{ return EmpitsuTypes.FOR; }
	"function" 								{ return EmpitsuTypes.FUNCTION; }
	"true" 										{ return EmpitsuTypes.TRUE; }
	"false" 									{ return EmpitsuTypes.FALSE; }
	"else" 										{ return EmpitsuTypes.ELSE; }
	"return" 									{ return EmpitsuTypes.RETURN; }
	"break" 									{ return EmpitsuTypes.BREAK; }
	"struct" 									{ return EmpitsuTypes.STRUCT; }
	"null" 										{ return EmpitsuTypes.NULL; }
	"in" 											{ return EmpitsuTypes.IN; }
	[a-zA-Z_][a-zA-Z0-9_]* 		{ return EmpitsuTypes.IDENTIFIER; }
	"+" 											{ return EmpitsuTypes.PLUS; }
	"-"				 							  { return EmpitsuTypes.MINUS; }
	"*" 											{ return EmpitsuTypes.MULTIPLY; }
	"/" 											{ return EmpitsuTypes.DIVIDE; }
	"(" 											{ return EmpitsuTypes.LEFT_PAREN; }
	")" 											{ return EmpitsuTypes.RIGHT_PAREN; }
	"{" 											{ return EmpitsuTypes.LEFT_BRACE; }
	"}" 											{ return EmpitsuTypes.RIGHT_BRACE; }
	";" 											{ return EmpitsuTypes.SEMICOLON; }
	"," 											{ return EmpitsuTypes.COMMA; }
	"=" 											{ return EmpitsuTypes.ASSIGN; }
	"!" 											{ return EmpitsuTypes.LOGICAL_NOT; }
	"&&" 											{ return EmpitsuTypes.LOGICAL_AND; }
	"||" 											{ return EmpitsuTypes.LOGICAL_OR; }
	"." 											{ return EmpitsuTypes.DOT; }
	":" 											{ return EmpitsuTypes.COLON; }
	"=="											{ return EmpitsuTypes.EQUAL; }
	"!="											{ return EmpitsuTypes.NOT_EQUAL; }
	"<" 											{ return EmpitsuTypes.LESS_THAN; }
	"<=" 											{ return EmpitsuTypes.LESS_THAN_OR_EQUAL; }
	">" 											{ return EmpitsuTypes.GREATER_THAN; }
	">="											{ return EmpitsuTypes.GREATER_THAN_OR_EQUAL; }
	"?"							 				  { return EmpitsuTypes.QUESTION; }
	"|"							  				{ return EmpitsuTypes.BITWISE_OR; }
	[^] 						 				  { return TokenType.BAD_CHARACTER; }
}