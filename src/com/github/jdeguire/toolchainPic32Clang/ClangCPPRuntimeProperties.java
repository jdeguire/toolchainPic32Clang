/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;

/**
 * Handles the C++ compiler options.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public final class ClangCPPRuntimeProperties extends ClangAbstractMipsRuntimeProperties{
    
    public ClangCPPRuntimeProperties(final MakeConfigurationBook desc, final MakeConfiguration conf) 
                              throws IllegalArgumentException, MakeConfigurationException {
        super(desc, conf);
    }
}
