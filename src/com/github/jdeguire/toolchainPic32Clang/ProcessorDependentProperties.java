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
public final class ProcessorDependentProperties extends CommonProperties {

    public ProcessorDependentProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) {
		super(projectDescriptor, conf, commandLineProperties);
        String commandLineOption = null;
        // Find if the project has a gld
        String gldName = getLinkerGldFileName();
        if (gldName != null && gldName.length() > 0) {
            commandLineOption = ",-T";
            if (getUseResponseFiles()) {
                // For cases where we use a response file, we cannot pass "..\t.ld", we need to pass ../t.ld.
                // In other words, the linker likes the name of the scripts to be escaped using the
                // char '\' and not by quoting the whole thing.
                gldName = calc.getLinkerGldFileName(projectDescriptor, conf);
            }
        } else {
// TODO:  We may need to manually use the default scripts found in the device libraries.
            // No gld in the project
            gldName = "";
            commandLineOption = "";
        }
        commandLineProperties.put("OPTION_TO_SPECIFY_GLD", commandLineOption);
        commandLineProperties.put("GLD_NAME", gldName);
        commandLineProperties.put("PROCESSOR_NAME", getProcessorNameForCompiler());
    }
}
