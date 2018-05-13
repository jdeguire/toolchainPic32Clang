package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsModifier;

/**
 *
 * @author c14014
 */
public class ClangCAdditionalOptionsModifier extends AdditionalOptionsModifier {
    
    public static final String OPT_ID = "C32";

    public ClangCAdditionalOptionsModifier() {
        super(OPT_ID);
    }

}