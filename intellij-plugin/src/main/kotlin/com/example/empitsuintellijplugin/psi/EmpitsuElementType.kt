package com.example.empitsuintellijplugin.psi

import com.example.empitsuintellijplugin.EmpitsuLanguage
import com.intellij.psi.tree.IElementType

class EmpitsuElementType(debugName: String): IElementType(debugName, EmpitsuLanguage.INSTANCE)