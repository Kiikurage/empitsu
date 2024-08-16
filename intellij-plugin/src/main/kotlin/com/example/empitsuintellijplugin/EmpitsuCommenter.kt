package com.example.empitsuintellijplugin

import com.intellij.lang.Commenter

class EmpitsuCommenter : Commenter {
    override fun getLineCommentPrefix() = "//"

    override fun getBlockCommentPrefix() = "/*"

    override fun getBlockCommentSuffix() = "*/"

    override fun getCommentedBlockCommentPrefix() = "/*"

    override fun getCommentedBlockCommentSuffix() = "*/"
}