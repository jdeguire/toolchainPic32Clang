/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.properties.MPLABXSpecificProperties;
import java.util.Properties;

/**
 * Common runtime properties usable during makefile generation process.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class CommonProperties extends MPLABXSpecificProperties {

    public CommonProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        Boolean pic32CDeviceSelected = ClangLanguageToolchain.isPIC32C(getPic());
        commandLineProperties.put("PIC32C", pic32CDeviceSelected.toString());
        String emission = pic32CDeviceSelected ? "" : getLibcEmission(projectDescriptor, conf);
        commandLineProperties.put("LEGACY_LIBC", emission);
    }

    public static String getLibcEmission(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        return LTUtils.getLegacyLibcEmissionValue(projectDescriptor, conf, "1.41", "C32Global", "legacy-libc");
    }
}
