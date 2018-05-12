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
public class XC32UserDefineModifier extends UserDefineModifier {

    public XC32UserDefineModifier() {
        super(XC32UserDefineProvider.OPT_ID, XC32UserDefineProvider.OPT_PROP);
    }
}
