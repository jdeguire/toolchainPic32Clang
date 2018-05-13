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
public class ClangCustomizerNodeAR extends ClangAbstractCustomizerNode {

    public ClangCustomizerNodeAR(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig) {
        super(id, name, children, sig);
    }

    @Override
    protected ClangAbstractMipsRuntimeProperties getPropertiesFile(final MakeConfigurationBook desc) {
        return new ClangRuntimeProperties(desc, conf);
    }
}
