package com.example.empitsuintellijplugin

import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.execution.configurations.ConfigurationType
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.components.BaseState
import com.intellij.openapi.project.Project


class EmpitsuConfigurationFactory(type: ConfigurationType) : ConfigurationFactory(type) {
    override fun getId(): String {
        return EmpitsuRunConfigurationType.ID
    }

    override fun createTemplateConfiguration(project: Project): RunConfiguration {
        return EmpitsuRunConfiguration(project, this, "Demo")
    }

    override fun getOptionsClass(): Class<out BaseState> {
        return EmpitsuRunConfigurationOptions::class.java
    }
}