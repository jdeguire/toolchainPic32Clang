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
public final class ClangCUserDefineModifier extends UserDefineModifier {

    public ClangCUserDefineModifier() {
        super(ClangCUserDefineProvider.OPT_ID, ClangCUserDefineProvider.OPT_PROP);
    }
}
