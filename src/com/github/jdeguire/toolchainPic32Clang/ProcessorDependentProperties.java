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
            final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

		super(projectDescriptor, conf, commandLineProperties);
        // Find if the project has a gld
        String gldName = getLinkerGldFileName();
        if (gldName != null && gldName.length() > 0) {
            if (getUseResponseFiles()) {
                // For cases where we use a response file, we cannot pass "..\t.ld", we need to pass ../t.ld.
                // In other words, the linker likes the name of the scripts to be escaped using the
                // char '\' and not by quoting the whole thing.
                gldName = calc.getLinkerGldFileName(projectDescriptor, conf);
            }
        } else {
            // No gld in the project, so use the default one ('=' means "relative to sysroot").
            gldName = "=/lib/proc/";
            String procname = target.getDeviceName();

            if(target.isMips32()) {
                if(procname.startsWith("PIC32")) {
                    procname = procname.substring(3);
                }

                gldName += procname + "/p" + procname + ".ld";
            } else {
                if(procname.startsWith("SAM")) {
                    procname = "AT" + procname;
                }

                gldName += procname + "/" + procname + ".ld";                
            }
        }

        commandLineProperties.put("OPTION_TO_SPECIFY_GLD", ",-T");
        commandLineProperties.put("GLD_NAME", gldName);
// TODO:  Change this to get the target_config file for the processor
        commandLineProperties.put("PROCESSOR_NAME", getProcessorNameForCompiler());
    }
}
