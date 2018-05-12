package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 *
 * @author c14014
 */
public class XC32ASAdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "C32-AS";

    public XC32ASAdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
