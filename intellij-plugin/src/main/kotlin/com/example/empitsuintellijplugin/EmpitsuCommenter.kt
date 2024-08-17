package com.example.empitsuintellijplugin

import com.intellij.lang.Commenter
import com.intellij.openapi.fileTypes.SyntaxHighlighter
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

class EmpitsuCommenter : Commenter {
    override fun getLineCommentPrefix(): String {
        return "//"
    }

    override fun getBlockCommentPrefix(): String {
        return "/*"
    }

    override fun getBlockCommentSuffix(): String {
        return "*/"
    }

    override fun getCommentedBlockCommentPrefix(): String {
        return "/*"
    }

    override fun getCommentedBlockCommentSuffix(): String {
        return "*/"
    }
}