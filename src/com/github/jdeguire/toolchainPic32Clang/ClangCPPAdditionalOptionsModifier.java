package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsModifier;

/**
 *
 * @author c14014
 */
public class XC32CPPAdditionalOptionsModifier extends AdditionalOptionsModifier {
    
    public static final String OPT_ID = "C32CPP";

    public XC32CPPAdditionalOptionsModifier() {
        super(OPT_ID);
    }

}
