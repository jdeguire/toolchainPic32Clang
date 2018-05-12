/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.icon.license.LicenseInfoRetriever;
import com.microchip.mplab.nbide.toolchainCommon.predicate.CompilerIsEligibleForRoamingPredicate;
import com.microchip.mplab.nbide.toolchainCommon.predicate.IPredicate.ToolchainData;

/**
 * @author Constantin Dumitrascu
 */
public class XC32LicensingInfoProvider extends LicenseInfoRetriever {

    public XC32LicensingInfoProvider(String pathToBin, String toolchainID, String version) {
        super(pathToBin, "xc32", version, new CompilerIsEligibleForRoamingPredicate(new ToolchainData("XC32", pathToBin, "1.10")));
    }
}
