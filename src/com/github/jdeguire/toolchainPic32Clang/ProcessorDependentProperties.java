/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.XMLBaseMakefileWriter;
import java.util.Properties;


public class ProcessorDependentProperties {
    public ProcessorDependentProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {

        String commandLineOption = null;
        // Find if the project has a gld
        String gldName = LinkerProperties.getLinkerGldFileName(projectDescriptor, conf);
        if (gldName != null && gldName.length() > 0) {
            commandLineOption = ",--script=";
            if (ClangGlobalMakeRuntimeProperties.getUseResponseFiles(projectDescriptor, conf)) {
                // For cases where we use a response file, we cannot pass "..\t.ld", we need to pass ../t.ld.
                // In other words, the linker likes the name of the scripts to be escaped using the
                // char '\' and not by quoting the whole thing.
                gldName = XMLBaseMakefileWriter.getLinkerGldFileName(projectDescriptor, conf);
            }
        }
        else {
            // No gld in the project
            gldName = "";
            commandLineOption = "";
        }
        commandLineProperties.put("OPTION_TO_SPECIFY_GLD", commandLineOption);
        commandLineProperties.put("GLD_NAME", gldName);
        commandLineProperties.put("PROCESSOR_NAME", CompilerProperties.getProcessorNameForCompiler(conf.getDevice().getValue()));
    }
}
