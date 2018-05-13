/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;

public class AssemblerProperties extends CommonProperties {

    public AssemblerProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);

        commandLineProperties.put("PROCESSOR_NAME_FOR_ASSEMBLER", CompilerProperties.getProcessorNameForCompiler(conf.getDevice().getValue()));
        LinkerProperties.addDebuggerNameOptions(conf.getPlatformTool().getValue(), getPic(), commandLineProperties);
        commandLineProperties.put("project_cpp", CompilerProperties.buildWithGPP(projectDescriptor, conf));
    }
}
