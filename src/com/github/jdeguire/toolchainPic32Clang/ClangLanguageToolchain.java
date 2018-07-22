/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

/**
 * 
 * @author jose 
 * Modified by jdeguire for toolchainPic32Clang.
 */ 
public class ClangLanguageToolchain {

    // For optimization, since it takes running the compiler to get its version
    // type, keep a map of version strings indexed by installation directory
    private static Map<String, String> mapOfInstallationDirToVersion = new HashMap<String, String>();

    /**
     * Note: this function WILL run the compiler once
     *
     * @param baseDir - the base directory of the toolchain
     * @return true <=> the toolchain at the 'baseDir' location has C++ support
     */
    public static boolean xcHasCPPSupport(String baseDir) {
        String version = null;
        if (mapOfInstallationDirToVersion.containsKey(baseDir)) {
            version = mapOfInstallationDirToVersion.get(baseDir);
        } else {
            // JLD: Note the following call will invoke the compiler. This is slow but needed.
            // There is one place where this method is called. And at that point, we
            // do not have the MakeConfiguration. So we have no choice. We must run
            // the compiler to get the version. 
            version = new ClangVersionProvider().getVersion(baseDir);
            mapOfInstallationDirToVersion.put(baseDir, version);
        }
        if (null == version || version.isEmpty()) {
            MPLABLogger.mplog.log(Level.SEVERE, "[Clang]ClangLanguageToolchain::xcHasCPPSupport, can't determine the toolchain version.");
            return true;
        }
        return version.compareTo("1.00") > 0;
    }

    /**
     *
     * @param conf
     * @param baseDir- the base directory of the toolchain
     * @return true <=> the toolchain at the 'baseDir' location has C++ support
     */
    public static boolean xcHasCPPSupport(MakeConfiguration conf, String baseDir) {
        String version = LTUtils.getVersion(conf);
        if (null == version || version.isEmpty()) {
            MPLABLogger.mplog.log(Level.SEVERE, "[Clang]ClangLanguageToolchain::xcHasCPPSupport with conf, can't determine the toolchain version.");
            return true;
        }
        return version.compareTo("1.00") > 0;
    }

    public static boolean supportsMemorySummary(final MakeConfiguration conf) {
        boolean supported = LTUtils.toolchainVersionGreaterOrEqualTo("1.40", conf);
        return supported;
    }

    public static boolean supportsSkipLicenseCheck(final MakeConfiguration conf) {
        boolean supported = LTUtils.toolchainVersionGreaterOrEqualTo("1.43", conf);
        return supported;
    }

    public static String getBuildComparisonCommandLineArgument(final MakeConfiguration conf) {
        boolean supported = LTUtils.toolchainVersionGreaterOrEqualTo("1.42", conf);
        return supported ? "-mafrlcsj" : "";
    }

    // TODO:  Can this go away (or be replaced by a check for target.arch.isARM)?
    public static boolean isPIC32C(final xPIC pic) {
        LTUtils.preventNullObjects(pic);
        boolean isPIC32C = FamilyDefinitions.Family.ARM32BIT == pic.getFamily();
        return isPIC32C;
    }
}
