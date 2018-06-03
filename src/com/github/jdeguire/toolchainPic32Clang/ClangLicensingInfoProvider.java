/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.icon.license.LicenseInfoRetriever;
import com.microchip.mplab.nbide.toolchainCommon.predicate.CompilerIsEligibleForRoamingPredicate;
import com.microchip.mplab.nbide.toolchainCommon.predicate.IPredicate.ToolchainData;


// TOOD:  Can we just remove this?
public class ClangLicensingInfoProvider extends LicenseInfoRetriever {

    public ClangLicensingInfoProvider(String pathToBin, String toolchainID, String version) {
        super(pathToBin, "Clang", version, new CompilerIsEligibleForRoamingPredicate(new ToolchainData("Clang", pathToBin, "1.10")));
    }
}
