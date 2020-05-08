/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.icon.license.LicenseInfoRetriever;
import com.microchip.mplab.nbide.toolchainCommon.predicate.CompilerIsEligibleForRoamingPredicate;
import com.microchip.mplab.nbide.toolchainCommon.predicate.IPredicate.ToolchainData;

/** 
 * @author Constantin Dumitrascu 
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */ 
// TOOD:  Can we just remove this?
public final class ClangLicensingInfoProvider extends LicenseInfoRetriever {

    public ClangLicensingInfoProvider(final String pathToBin, final String toolchainID, final String version) {
        super(pathToBin, "Clang", version, new CompilerIsEligibleForRoamingPredicate(new ToolchainData("Clang", pathToBin, "0.1")));
    }
}
