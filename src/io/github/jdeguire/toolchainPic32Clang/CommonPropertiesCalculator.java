/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.properties.CommonLanguageToolchainPropertiesUtils;

/**
 *
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by jdeguire for toolchainPic32Clang.
 *
 */
//TODO:  This is not referenced in Clang.languageToolchain.xml.  Is it safe to remove?
public final class CommonPropertiesCalculator extends CommonLanguageToolchainPropertiesUtils {

    /* These are meant to check if a given version of an XC compiler supports certain
     * Microchip-specific features.  None of these apply to Clang, so they'll just return False.
     * They are still here just in case MPLAB X depends on them.
     */
    final boolean supportsMemorySummary(final MakeConfiguration conf) {
    	return false;
    }

    final boolean supportsSkipLicenseCheck(final MakeConfiguration conf) {
        return false;
    }

    final boolean supportsBuildComparison(final MakeConfiguration conf) {
        return false;
    }
    
    final boolean supportsLegacyLibc(final MakeConfiguration conf) {
        return false;
    }

    final boolean supportMDFPEmission(final MakeConfiguration conf) {
        return false;
    }
    
    public final boolean supportMDFPEmission(final String version) {
        return false;
    }    
}
