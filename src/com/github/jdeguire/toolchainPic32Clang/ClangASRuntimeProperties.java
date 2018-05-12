/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;

/**
 * Handles the AS compiler options.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class XC32ASRuntimeProperties extends XC32AbstractMipsRuntimeProperties {

    public XC32ASRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
    }
}
