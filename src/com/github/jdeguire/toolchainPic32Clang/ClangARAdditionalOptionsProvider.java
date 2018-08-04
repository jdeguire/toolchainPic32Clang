package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 *
 * @author c14014
 */
public final class ClangARAdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "C32-AR";

    public ClangARAdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
