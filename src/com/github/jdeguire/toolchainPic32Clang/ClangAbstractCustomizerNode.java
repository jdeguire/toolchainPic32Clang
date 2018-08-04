/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

// TODO:  This class was removed in SDK 5.00.  Can we remove it here?

import com.microchip.crownking.opt.OptionLanguage;
import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ConfigurationBookProvider;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ui.CustomizerNode;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ui.LanguageToolchainCustomizerNode;
import java.util.Properties;
import java.util.logging.Level;

/**
 * Abstract customizer node. All inheritors must implement the properties class generation method.
 * @author marian.golea <marian.golea@mirochip.com>
 */
public abstract class ClangAbstractCustomizerNode extends LanguageToolchainCustomizerNode {

    public ClangAbstractCustomizerNode(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig) {
        this(id, name, children, sig, false);
    }

    public ClangAbstractCustomizerNode(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig, boolean showUserComments) {
        super(id, name, children, sig, showUserComments);
    }

    protected abstract ClangAbstractMipsRuntimeProperties getPropertiesFile(final MakeConfigurationBook desc);

    @Override
    public Properties getOptionLanguageContextProperties() {
        Properties ret = null;
        ConfigurationBookProvider descProvider = project.getLookup().lookup(ConfigurationBookProvider.class);
        if (descProvider == null) {
            MPLABLogger.mplog.log(Level.SEVERE, "ClangCustomizerNode::getOptionLanguageContextProperties, Couldn't get ConfigurationDescriptorProvider!");
        } else {
            MakeConfigurationBook desc = descProvider.getConfigurationBook();
            if (desc == null) {
                MPLABLogger.mplog.log(Level.SEVERE, "ClangCustomizerNode::getOptionLanguageContextProperties, Couldn't get MakeConfigurationBook!");
            } else {
                ret = getPropertiesFile(desc);
            }
        }
        return ret;
    }
}
