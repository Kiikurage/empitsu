package com.example.empitsuintellijplugin

import com.intellij.lang.Language

class EmpitsuLanguage : Language("Empitsu") {
    companion object {
        val INSTANCE = EmpitsuLanguage()
    }
}