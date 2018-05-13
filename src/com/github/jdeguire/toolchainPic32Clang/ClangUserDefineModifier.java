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
public class ClangUserDefineModifier extends UserDefineModifier {

    public ClangUserDefineModifier() {
        super(ClangUserDefineProvider.OPT_ID, ClangUserDefineProvider.OPT_PROP);
    }
}
