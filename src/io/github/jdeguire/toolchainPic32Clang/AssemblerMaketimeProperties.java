/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;

/**
 *
 * @author jose
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */
public final class AssemblerMaketimeProperties extends CommonMaketimeProperties {

    public AssemblerMaketimeProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties)
        throws com.microchip.crownking.Anomaly, 
        org.xml.sax.SAXException,
        java.io.IOException, 
        javax.xml.parsers.ParserConfigurationException, 
        IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);

        addDebuggerNameOptions();
        commandLineProperties.setProperty("project_cpp", shouldBuildWithCPP(ClangLanguageToolchain.CPP_SUPPORT_FIRST_VERSION).toString());
    }
}
