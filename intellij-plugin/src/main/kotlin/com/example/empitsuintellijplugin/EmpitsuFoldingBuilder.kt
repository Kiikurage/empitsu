package com.example.empitsuintellijplugin

import com.example.empitsuintellijplugin.psi.EmpitsuBlockExpression
import com.example.empitsuintellijplugin.psi.EmpitsuStructPropertyDeclarations
import com.example.empitsuintellijplugin.psi.EmpitsuStructPropertyInitializers
import com.example.empitsuintellijplugin.psi.EmpitsuVisitor
import com.intellij.lang.ASTNode
import com.intellij.lang.folding.FoldingBuilderEx
import com.intellij.lang.folding.FoldingDescriptor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.FoldingGroup
import com.intellij.openapi.project.DumbAware
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement


class EmpitsuFoldingBuilder: FoldingBuilderEx(), DumbAware {
    override fun buildFoldRegions(root: PsiElement, document: Document, quick: Boolean): Array<FoldingDescriptor> {
        val descriptors = mutableListOf<FoldingDescriptor>()

        root.acceptChildren(object : EmpitsuVisitor() {
            override fun visitStructPropertyDeclarations(declarations: EmpitsuStructPropertyDeclarations) {
                super.visitStructPropertyDeclarations(declarations)

                descriptors.add(
                    FoldingDescriptor(
                        declarations.node,
                        TextRange(
                            declarations.textRange.startOffset + 1,
                            declarations.textRange.endOffset - 1
                        ),
                        FoldingGroup.newGroup("empitsu"),
                        "...",
                    )
                )
            }

            override fun visitStructPropertyInitializers(initializers: EmpitsuStructPropertyInitializers) {
                super.visitStructPropertyInitializers(initializers)

                descriptors.add(
                    FoldingDescriptor(
                        initializers.node,
                        TextRange(
                            initializers.textRange.startOffset + 1,
                            initializers.textRange.endOffset - 1
                        ),
                        FoldingGroup.newGroup("empitsu"),
                        "...",
                    )
                )
            }

            override fun visitBlockExpression(statement: EmpitsuBlockExpression) {
                super.visitBlockExpression(statement)

                descriptors.add(
                    FoldingDescriptor(
                        statement.node,
                        TextRange(
                            statement.textRange.startOffset + 1,
                            statement.textRange.endOffset - 1
                        ),
                        FoldingGroup.newGroup("empitsu"),
                        "...",
                    )
                )
            }

            override fun visitPsiElement(o: PsiElement) {
                o.acceptChildren(this)
            }
        })

        return descriptors.toTypedArray()
    }

    override fun getPlaceholderText(node: ASTNode): String {
        return "..."
    }

    override fun isCollapsedByDefault(node: ASTNode): Boolean {
        return false
    }
}