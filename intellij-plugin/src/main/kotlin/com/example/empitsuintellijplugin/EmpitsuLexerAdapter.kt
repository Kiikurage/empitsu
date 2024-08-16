package com.example.empitsuintellijplugin

import com.intellij.lexer.FlexAdapter

class EmpitsuLexerAdapter : FlexAdapter(EmpitsuLexer(null))