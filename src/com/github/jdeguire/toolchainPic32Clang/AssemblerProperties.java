/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;

/**
 *
 * @author jose
 * Modified by jdeguire for toolchainPic32Clang.
 */
public final class AssemblerProperties extends CommonProperties {

    public AssemblerProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);

        addDebuggerNameOptions();
        commandLineProperties.put("project_cpp", shouldBuildWithCPP(ClangLanguageToolchain.CPP_SUPPORT_FIRST_VERSION));
    }
}
