/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import java.util.Properties;

/**
 * Command line options available during makefile writeer process for archiver
 * build sequence.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class ArchiverProperties extends CommonProperties {

    public ArchiverProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        setMProcessor(projectDescriptor, conf, commandLineProperties);
    }

    private void setMProcessor(MakeConfigurationBook projectDescriptor, MakeConfiguration conf, Properties commandLineProperties) {
        String emission = "";
        //TODO Marian: remove this check once xc32 v2.00 fixes its problem related to expected use of this option.
        //The option is supposed to also work with non mips devices!
        if (XC32LanguageToolchain.isPIC32C(getPic())) {
            //Marian: Normally, PIC32C is only supported starting with 2.00, so next check might seem redundant.
            //It stays here because in the future this option will also work for mips devices, when 2.00 compiler is fixed.
            Boolean mprocessorSupported = LTUtils.toolchainVersionGreaterOrEqualTo("2.00", conf);
            if (mprocessorSupported) {
                emission = " -mprocessor=$(MP_PROCESSOR_OPTION) ";
            }
        }
        commandLineProperties.put("MPROCESSOR_CALL", emission);
    }
}
