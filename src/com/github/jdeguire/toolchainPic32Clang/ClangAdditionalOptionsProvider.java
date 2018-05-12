package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 *
 * @author c14014
 */
public class XC32AdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "C32";

    public XC32AdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
