/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.opt.OptionLanguage;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.LanguageToolRuntimePropertiesAccessor;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ui.CustomizerNode;
import com.microchip.mplab.nbide.toolchainCommon.customizers.CommonAbstractCustomizerNode;
import com.microchip.mplab.nbide.toolchainCommon.properties.CommonToolchainPropertiesAccessor;

/**
 *
 * @author Administrator
 */
public final class ClangCustomizerNode extends CommonAbstractCustomizerNode {

    public ClangCustomizerNode(final String id, final String name, final CustomizerNode[] children, final OptionLanguage.Signature sig) {
        super(id, name, children, sig, true);
    }

    @Override
    protected LanguageToolRuntimePropertiesAccessor getPropertiesFile(final MakeConfigurationBook desc) {
        try {
            return new ClangRuntimeProperties(desc, conf);
        } catch(Exception ex) {
            // Cannot throw because the superclass method does not declare itself as throwing.
            return new CommonToolchainPropertiesAccessor(desc, conf);
        }
    }
}
