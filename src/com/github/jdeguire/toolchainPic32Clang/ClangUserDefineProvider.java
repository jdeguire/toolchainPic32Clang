package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserDefineProvider;

/**
 *
 * @author drmc
 */
public class XC32UserDefineProvider extends UserDefineProvider {

    public static final String OPT_ID = "C32";
    public static final String OPT_PROP = "preprocessor-macros"; 
        
    public XC32UserDefineProvider() {
        super(OPT_ID, OPT_PROP);
    }

}
