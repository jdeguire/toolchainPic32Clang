package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserDefineProvider;

/**
 *
 * @author drmc
 */
public class ClangUserDefineProvider extends UserDefineProvider {

    public static final String OPT_ID = "C32";
    public static final String OPT_PROP = "preprocessor-macros"; 
        
    public ClangUserDefineProvider() {
        super(OPT_ID, OPT_PROP);
    }

}
