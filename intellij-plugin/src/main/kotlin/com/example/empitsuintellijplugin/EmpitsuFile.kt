package com.example.empitsuintellijplugin

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider

class EmpitsuFile(
    viewProvider: FileViewProvider
): PsiFileBase(viewProvider, EmpitsuLanguage.INSTANCE) {
    override fun getFileType(): FileType {
        return EmpitsuFileTypeInstance
    }

    override fun toString(): String {
        return "Empitsu File"
    }
}