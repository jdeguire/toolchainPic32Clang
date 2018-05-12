/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

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
public abstract class XC32AbstractCustomizerNode extends LanguageToolchainCustomizerNode {

    public XC32AbstractCustomizerNode(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig) {
        this(id, name, children, sig, false);
    }

    public XC32AbstractCustomizerNode(String id, String name, CustomizerNode[] children, OptionLanguage.Signature sig, boolean showUserComments) {
        super(id, name, children, sig, showUserComments);
    }

    protected abstract XC32AbstractMipsRuntimeProperties getPropertiesFile(final MakeConfigurationBook desc);

    @Override
    public Properties getOptionLanguageContextProperties() {
        Properties ret = null;
        ConfigurationBookProvider descProvider = project.getLookup().lookup(ConfigurationBookProvider.class);
        if (descProvider == null) {
            MPLABLogger.mplog.log(Level.SEVERE, "XC32CustomizerNode::getOptionLanguageContextProperties, Couldn't get ConfigurationDescriptorProvider!");
        } else {
            MakeConfigurationBook desc = descProvider.getConfigurationBook();
            if (desc == null) {
                MPLABLogger.mplog.log(Level.SEVERE, "XC32CustomizerNode::getOptionLanguageContextProperties, Couldn't get MakeConfigurationBook!");
            } else {
                ret = getPropertiesFile(desc);
            }
        }
        return ret;
    }
}
