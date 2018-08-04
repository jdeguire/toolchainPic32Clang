/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;

/**
 * Command line options available during makefile writer process for archiver
 * build sequence.
 * 
 * @author Marian Golea <marian.golea@microchip.com> 
 * Modified by jdeguire for toolchainPic32Clang.
 */
public final class ArchiverProperties extends CommonProperties {

    public ArchiverProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        setMProcessor();
    }

    private void setMProcessor() {
// TODO: Does llvm-ar need anything like this?  I don't see why it would.
//
//        String emission = "";
//        //TODO Marian: remove this check once xc32 v2.00 fixes its problem related to expected use of this option.
//        //The option is supposed to also work with non mips devices!
//        if (isPIC32C()) {
//            //Marian: Normally, PIC32C is only supported starting with 2.00, so next check might seem redundant.
//            //It stays here because in the future this option will also work for mips devices, when 2.00 compiler is fixed.
//            Boolean mprocessorSupported = toolchainVersionGreaterOrEqualTo("2.00");
//            if (mprocessorSupported) {
//                emission = " -mprocessor=$(MP_PROCESSOR_OPTION) ";
//            }
//        }
//        commandLineProperties.put("MPROCESSOR_CALL", emission);
    }
}
