/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.StreamingDataCapabilitiesInterface;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;

/**
 * Handles the C compiler options.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class XC32GCCRuntimeProperties extends XC32AbstractMipsRuntimeProperties {

    public XC32GCCRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        setProperty("appio.suppress", shouldSuppressAppIO());
    }

    private String shouldSuppressAppIO() {
        if (pic32CSelected) {
            return Boolean.TRUE.toString();
        }

        StreamingDataCapabilitiesInterface sdc = assembly.getLookup().lookup(StreamingDataCapabilitiesInterface.class);
        Boolean suppress = !(sdc.isApplicationOutSupported() && sdc.isApplicationInSupported());

        return suppress.toString();
    }

}
