package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsModifier;

/**
 *
 * @author c14014
 */
public class XC32AdditionalOptionsModifier extends AdditionalOptionsModifier {
    
    public static final String OPT_ID = "C32";

    public XC32AdditionalOptionsModifier() {
        super(OPT_ID);
    }

}
