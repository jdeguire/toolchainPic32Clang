/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.opt.OptionLanguage;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ui.CustomizerNode;

/**
 *
 * @author Administrator
 */
public class XC32CustomizerNode extends XC32AbstractCustomizerNode {

    public XC32CustomizerNode(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig) {
        super(id, name, children, sig, true);
    }

    @Override
    protected XC32AbstractMipsRuntimeProperties getPropertiesFile(final MakeConfigurationBook desc) {
        return new XC32RuntimeProperties(desc, conf);
    }
}
