package com.example.empitsuintellijplugin

import com.intellij.execution.configurations.ConfigurationTypeBase
import com.intellij.icons.AllIcons
import com.intellij.openapi.util.NotNullLazyValue


class EmpitsuRunConfigurationType :
    ConfigurationTypeBase(
        ID,
        "Empitsu",
        "Run Empitsu script",
        NotNullLazyValue.createValue { AllIcons.Nodes.Console }) {
    init {
        addFactory(EmpitsuConfigurationFactory(this))
    }

    companion object {
        const val ID: String = "EmpitsuRunConfiguration"
    }
}