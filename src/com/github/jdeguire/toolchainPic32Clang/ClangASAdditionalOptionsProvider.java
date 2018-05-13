package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 *
 * @author c14014
 */
public class ClangASAdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "C32-AS";

    public ClangASAdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
