package com.example.empitsuintellijplugin

import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.ui.IconManager
import com.intellij.ui.PlatformIcons
import javax.swing.Icon

class EmpitsuFileType : LanguageFileType(EmpitsuLanguage.INSTANCE) {
    override fun getName(): String {
        return "Empitsu File"
    }

    override fun getDescription(): String {
        return "Empitsu file"
    }

    override fun getDefaultExtension(): String {
        return "emp"
    }

    override fun getIcon(): Icon {
        return IconManager.getInstance().getPlatformIcon(PlatformIcons.TextFileType)
    }
}

val EmpitsuFileTypeInstance = EmpitsuFileType()