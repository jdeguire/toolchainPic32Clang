package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserDefineProvider;

/**
 *
 * @author drmc
 */
public final class ClangCUserDefineProvider extends UserDefineProvider {

    public static final String OPT_ID = "C32";
    public static final String OPT_PROP = "preprocessor-macros"; 

    public ClangCUserDefineProvider() {
        super(OPT_ID, OPT_PROP);
    }

}
