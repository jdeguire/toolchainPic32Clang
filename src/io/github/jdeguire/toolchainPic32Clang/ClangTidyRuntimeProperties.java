/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;

/**
 * @author  Jesse DeGuire
 *
 * This is used to provide runtime support for Clang-tidy options.  This and the other "Runtime 
 * Properties" are used to support project options that you pick in the Project Properties with
 * Java code that can do things like react to options, set options for you, or blank out options
 * that are not valid for a particular target.  Basically, this lets us do extra stuff that we
 * cannot achieve using the XML options files alone.
 */
public final class ClangTidyRuntimeProperties extends ClangAbstractTargetRuntimeProperties{

    public ClangTidyRuntimeProperties(final MakeConfigurationBook desc, final MakeConfiguration conf) 
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
