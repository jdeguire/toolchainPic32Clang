package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 *
 * @author c14014
 */
public final class ClangCAdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "C32";

    public ClangCAdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
