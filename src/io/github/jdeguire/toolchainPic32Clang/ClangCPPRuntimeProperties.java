/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;

/**
 * Handles the C++ compiler options.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public final class ClangCPPRuntimeProperties extends ClangAbstractTargetRuntimeProperties{
    
    public ClangCPPRuntimeProperties(final MakeConfigurationBook desc, final MakeConfiguration conf) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException,
		MakeConfigurationException {

        super(desc, conf);
    }

    // TODO:  Since this class doesn't do anything, I might be able to remove it and use the
    //        ClangRuntimeProperties class instead in the main XML file for the CPP options.
}
