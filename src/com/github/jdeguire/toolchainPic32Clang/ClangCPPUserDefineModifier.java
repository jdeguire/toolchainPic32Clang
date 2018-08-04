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
public final class ClangCPPUserDefineModifier extends UserDefineModifier {

    public ClangCPPUserDefineModifier() {
        super(ClangCPPUserDefineProvider.OPT_ID, ClangCPPUserDefineProvider.OPT_PROP);
    }
}
