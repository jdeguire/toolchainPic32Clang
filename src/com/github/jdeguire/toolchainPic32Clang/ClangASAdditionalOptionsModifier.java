package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsModifier;

/**
 *
 * @author c14014
 */
public class ClangASAdditionalOptionsModifier extends AdditionalOptionsModifier {
    
    public static final String OPT_ID = "C32-AS";

    public ClangASAdditionalOptionsModifier() {
        super(OPT_ID);
    }

}
