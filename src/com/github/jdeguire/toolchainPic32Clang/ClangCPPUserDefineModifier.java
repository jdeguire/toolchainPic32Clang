/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserDefineModifier;

/**
 *
 * @author Venkat
 */
public class XC32CPPUserDefineModifier extends UserDefineModifier {

    public XC32CPPUserDefineModifier() {
        super(XC32CPPUserDefineProvider.OPT_ID, XC32CPPUserDefineProvider.OPT_PROP);
    }
}
