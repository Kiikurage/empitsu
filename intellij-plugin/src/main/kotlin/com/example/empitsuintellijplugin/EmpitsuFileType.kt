package com.example.empitsuintellijplugin

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

class EmpitsuFileType : LanguageFileType(EmpitsuLanguage.INSTANCE) {
    companion object {
        @JvmStatic public val INSTANCE = EmpitsuFileType()
    }

    override fun getName(): String {
        return "Empitsu File"
    }

    override fun getDescription(): String {
        return "Empitsu language file"
    }

    override fun getDefaultExtension(): String {
        return "emp"
    }

    override fun getIcon(): Icon? {
        return null
    }
}