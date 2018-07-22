/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.properties.MPLABXSpecificProperties;
import java.io.File;
// TODO: Remove commented-out stuff in this class once we know stuff actually works.
//import java.io.File;
//import java.nio.file.Paths;
//import java.nio.file.Path;
import java.util.Properties;
import org.netbeans.api.project.Project;

/**
 * Common runtime properties usable during makefile generation process.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class CommonProperties extends MPLABXSpecificProperties {

//    private String toolchainPath;
    
    public CommonProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        Boolean pic32CDeviceSelected = ClangLanguageToolchain.isPIC32C(getPic());
        commandLineProperties.put("PIC32C", pic32CDeviceSelected.toString());
        String emission = pic32CDeviceSelected ? "" : getLibcEmission(projectDescriptor, conf);
        commandLineProperties.put("LEGACY_LIBC", emission);

        commandLineProperties.put("XC32_COMPAT_MACROS", getXC32CompatibilityMacros(projectDescriptor, conf));
//        commandLineProperties.put("SYSROOT_DIR", "${MP_CC_DIR}");
// TODO:  Do we even need sysroot?

//        toolchainPath = findToolchainPath(commandLineProperties);
//        toolchainPath = findToolchainPath(conf);
    }

    /* Get the current value of the given option using the MakeConfiguration supplied to this class.
     * The optionBookId is the name given to the mp:configurationObject in the Clang.languageToolchain.xml
     * file, such as "C32Global", "C32", "C32CPP", etc.  The optionId is the name of the option itself.
     * This will return the given default value if the option could not be read for some reason.
     */
    public static String getProjectOption(MakeConfigurationBook confBook, MakeConfiguration conf, 
                                             String optionBookId, String optionId, String defaultVal) {
        String ret = defaultVal;
        Project project = confBook.getProject();

        if(null != project) {
            String val = EmbeddedProjectSupport.getSynthesizedOption(project, conf, optionBookId, 
                                                              optionId, null); // NOI18N

            if(null != val)
                ret = val;
        }
        
        return ret;
    }
    
    /* Return the base install path for the current toolchain with the file separator always at the
     * end (so "/foo/bar/" instead of "/foo/bar").
     */
    public String getToolchainBasePath(MakeConfiguration conf) {
        String toolchainPath = conf.getLanguageToolchain().getDir().getValue();

        if(File.separatorChar != toolchainPath.charAt(toolchainPath.length() - 1))
            toolchainPath += File.separator;

        return toolchainPath;
    }

    public static String getLibcEmission(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        return LTUtils.getLegacyLibcEmissionValue(projectDescriptor, conf, "1.41", "C32Global", "legacy-libc");
    }
    
    private static String getXC32CompatibilityMacros(MakeConfigurationBook confBook, MakeConfiguration conf) {
        String ret = "";

        String doCompat = getProjectOption(confBook, conf, "C32Global", "fake-xc32", "false");

        if(doCompat.equalsIgnoreCase("true")) {
            String compatVersion = getProjectOption(confBook, conf, "C32Global", "fake-xc32-version", "");

            if(!compatVersion.isEmpty()) {
                ret = "-D__XC -D__XC32 -D__XC32_VERSION__=" + compatVersion;
            }
        }

        return ret;
    }
    
/*
    private String findToolchainPath(Properties commandLineProperties) {
        String toolchainDir;

        try {
            Path ccPath = Paths.get(commandLineProperties.getProperty("MP_CC_DIR", ""));
            toolchainDir = ccPath.getParent().getParent().toString();
    
            // We want the path to end in the separator (so "/foo/bar/" instead of "/foo/bar" because
            // calling methods will probably build upon this.
            if(File.separatorChar != toolchainDir.charAt(toolchainDir.length() - 1))
                toolchainDir += File.separator;
        }
        catch(Exception e) {
            toolchainDir = "";
        }
        
        return toolchainDir;
    }
*/
/*
    private String findToolchainPath(MakeConfiguration conf) {
        String toolchainDir = conf.getLanguageToolchain().getDir().getValue();

        // We want the path to end in the separator (so "/foo/bar/" instead of "/foo/bar" because
        // calling methods will probably build upon this.
        if(File.separatorChar != toolchainDir.charAt(toolchainDir.length() - 1))
            toolchainDir += File.separator;

        return toolchainDir;
    }
*/
}