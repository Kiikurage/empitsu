package com.example.empitsuintellijplugin

import com.example.empitsuintellijplugin.psi.EmpitsuTypes
import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class EmpitsuSyntaxHighlighter : SyntaxHighlighterBase() {
    companion object {
        private val STRING = createTextAttributesKey("STRING", DefaultLanguageHighlighterColors.STRING)
        private val NUMBER = createTextAttributesKey("NUMBER", DefaultLanguageHighlighterColors.NUMBER)
        private val KEYWORD = createTextAttributesKey("KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
        private val COMMENT = createTextAttributesKey("SIMPLE_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
        private val BAD_CHARACTER = createTextAttributesKey("SIMPLE_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)

        private val STRINGS = arrayOf(STRING)
        private val NUMBERS = arrayOf(NUMBER)
        private val COMMENTS = arrayOf(COMMENT)
        private val KEYWORDS = arrayOf(KEYWORD)
        private val BAD_CHARACTERS = arrayOf(BAD_CHARACTER)
    }

    override fun getHighlightingLexer(): Lexer {
        return EmpitsuLexerAdapter()
    }

    override fun getTokenHighlights(tokenType: IElementType): Array<out TextAttributesKey> {
        return when (tokenType) {
            EmpitsuTypes.STRING_BEGIN,
            EmpitsuTypes.STRING_CHARACTER,
            EmpitsuTypes.STRING_END -> STRINGS

            EmpitsuTypes.NUMBER -> NUMBERS
            EmpitsuTypes.COMMENT -> COMMENTS

            EmpitsuTypes.IF,
            EmpitsuTypes.LET,
            EmpitsuTypes.FOR,
            EmpitsuTypes.FUNCTION,
            EmpitsuTypes.TRUE,
            EmpitsuTypes.FALSE,
            EmpitsuTypes.ELSE,
            EmpitsuTypes.RETURN,
            EmpitsuTypes.BREAK,
            EmpitsuTypes.STRUCT,
            EmpitsuTypes.NULL,
            EmpitsuTypes.IN -> KEYWORDS

            TokenType.BAD_CHARACTER -> BAD_CHARACTERS
            else -> emptyArray()
        }
    }
}