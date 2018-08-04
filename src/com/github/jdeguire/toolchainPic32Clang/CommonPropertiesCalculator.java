/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.properties.CommonLanguageToolchainPropertiesUtils;

/**
 *
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by jdeguire for toolchainPic32Clang.
 *
 */
public final class CommonPropertiesCalculator extends CommonLanguageToolchainPropertiesUtils {

// TODO:  We might be able to remove this because we use the "target.arch.isARM" property now.
    final boolean isPIC32C(final xPIC pic) {
        if (pic == null) {
            return false;
        }
        boolean isPIC32C = FamilyDefinitions.Family.ARM32BIT == pic.getFamily();
        return isPIC32C;
    }

    final boolean supportsMemorySummary(final MakeConfiguration conf) {
//        return toolchainVersionGreaterOrEqualTo(XC32LanguageToolchain.MEMORY_SUMMARY_SUPPORT_FIRST_VERSION, conf);
		return false;
    }

    final boolean supportsSkipLicenseCheck(final MakeConfiguration conf) {
//        return toolchainVersionGreaterOrEqualTo(XC32LanguageToolchain.SKIP_LICENSE_CHECK_SUPPORT_FIRST_VERSION, conf);
		return false;
    }

    final boolean supportsBuildComparison(final MakeConfiguration conf) {
//        return toolchainVersionGreaterOrEqualTo(XC32LanguageToolchain.BUILD_COMPARISON_SUPPORT_FIRST_VERSION, conf);
		return false;
    }
    
     final boolean supportsLegacyLibc(final MakeConfiguration conf) {
//        return toolchainVersionGreaterOrEqualTo(XC32LanguageToolchain.LIBC_SUPPORT_FIRST_VERSION, conf);
		 return false;
    }

}
