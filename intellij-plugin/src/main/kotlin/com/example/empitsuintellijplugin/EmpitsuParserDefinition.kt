package com.example.empitsuintellijplugin

import com.example.empitsuintellijplugin.parser.EmpitsuParser
import com.example.empitsuintellijplugin.psi.EmpitsuTypes
import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet

class EmpitsuParserDefinition : ParserDefinition {
    override fun createLexer(project: Project?): Lexer {
        return EmpitsuLexerAdapter()
    }

    override fun createParser(project: Project?): PsiParser {
        return EmpitsuParser()
    }

    override fun getFileNodeType() = IFileElementType(EmpitsuLanguage.INSTANCE)

    override fun getCommentTokens() = TokenSet.create(EmpitsuTypes.COMMENT)

    override fun getWhitespaceTokens() = TokenSet.create(EmpitsuTypes.WHITE_SPACE)

    override fun getStringLiteralElements(): TokenSet = TokenSet.EMPTY

    override fun createElement(node: ASTNode?): PsiElement {
        return EmpitsuTypes.Factory.createElement(node)
    }

    override fun createFile(viewProvider: FileViewProvider): PsiFile {
        return EmpitsuFile(viewProvider)
    }
}