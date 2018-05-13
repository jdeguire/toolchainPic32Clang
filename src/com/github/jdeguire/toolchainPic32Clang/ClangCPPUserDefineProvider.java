package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserDefineProvider;

/**
 *
 * @author drmc
 */
public class ClangCPPUserDefineProvider extends UserDefineProvider {
    
    public static final String OPT_ID = "C32CPP";
    public static final String OPT_PROP = "preprocessor-macros";    

    public ClangCPPUserDefineProvider() {
        super("C32CPP", "preprocessor-macros");
    }

}
