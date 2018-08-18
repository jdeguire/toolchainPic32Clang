/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.OptionConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.properties.MPLABXSpecificProperties;
import java.io.File;
import java.util.List;
import java.util.Properties;
import org.openide.util.Utilities;
import org.netbeans.api.project.Project;

/**
 * Common runtime properties usable during makefile generation process.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by jdeguire for toolchainPic32Clang.
 */
public class CommonProperties extends MPLABXSpecificProperties {

    final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();
    
    public CommonProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        Boolean pic32CDeviceSelected = isPIC32C();
        commandLineProperties.put("PIC32C", pic32CDeviceSelected.toString());
        String emission = pic32CDeviceSelected ? "" : getLibcEmission();
        commandLineProperties.put("LEGACY_LIBC", emission);

        commandLineProperties.put("XC32_COMPAT_MACROS", getXC32CompatibilityMacros(projectDescriptor, conf));
        commandLineProperties.put("SYS_INCLUDE_OPT", getSystemIncludeDirOpt());
    }

    final boolean isPIC32C() {
        return calc.isPIC32C(getPic());
    }

    private String getLibcEmission() {
        return getLegacyLibcEmissionValue(ClangLanguageToolchain.LIBC_SUPPORT_FIRST_VERSION, "C32Global", "legacy-libc");
    }

    final void addDebuggerNameOptions() {

        String debuggerOptionsAsSymbol = "";
        String debuggerOptionsAsMacro = "";
        String debuggerName = getCommonDebuggerMacro();
        if (debuggerName != null && debuggerName.length() > 0) {
            debuggerOptionsAsSymbol = "--defsym=" + debuggerName + "=1";
            debuggerOptionsAsMacro = "-D" + debuggerName + "=1";
        }
        if (debuggerOptionsAsSymbol.length() > 0) {
            commandLineProperties.put("COMMA_BEFORE_DEBUGGER_NAME", ",");
            boolean isPIC32C = isPIC32C();
            String debuggerOptionString = isPIC32C ? "" : "-mdebugger";
            commandLineProperties.put("DEBUGGER_OPTION_TO_LINKER", debuggerOptionString);
        } else {
            commandLineProperties.put("COMMA_BEFORE_DEBUGGER_NAME", "");
            commandLineProperties.put("DEBUGGER_OPTION_TO_LINKER", "");
        }

        commandLineProperties.put("DEBUGGER_NAME_AS_SYMBOL", debuggerOptionsAsSymbol);
        commandLineProperties.put("DEBUGGER_NAME_AS_MACRO", debuggerOptionsAsMacro);
    }

    final String getLinkerGldFileName() {
        List<String> filesInGldDir = desc.getFilesFromLinkerFolder(conf);
        String res = null;

        if (filesInGldDir != null && filesInGldDir.size() > 0) {
            res = filesInGldDir.get(0);
            // File has unix style escape sequence.
            res = getFileNameQuoted(res);
            // TODO add logging if size() > 1
        }

        return res; // NOI18N
    }

    final boolean getUseResponseFiles() {
        boolean res = false;
        if (!Utilities.isWindows()) {
            return res;
        }
        // Check the option value
        OptionConfiguration confObject = desc.getSynthesizedOptionConfiguration(conf.getName(), "C32-LD", null);
        if (confObject != null) {
            ClangRuntimeProperties props = new ClangRuntimeProperties(desc, conf);
            List<Pair<String, String>> emissionPairs = confObject.getEmissionPairs(props, null);
            if (emissionPairs != null) {
                for (Pair<String, String> p : emissionPairs) {
                    if (p.first.equals("additional-options-use-response-files") && p.second.equals("true")) {
                        res = true;
                        break;
                    }
                }
            }
        }
        return res;
    }

    /**
     * See return
     *
     * @param deviceName
     * @return value of processor to be fed to assembler. Override as needed
     */
    final String getProcessorNameForCompiler() {
        if (deviceName.toUpperCase().startsWith("PIC32")) {
            return deviceName.substring(3, deviceName.length());
        } else {
            return deviceName;
        }
    }
    
// TODO:  Remove 'confBook' and 'conf' from this signature.
// TODO:  Does this need to be static?
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

    private String getSystemIncludeDirOpt() {
        String arch = getProjectOption(desc, conf, "C32Global", "target.arch", "");
        String opt = "-isystem " + getToolchainBasePath(conf);
        
        if(arch.equals("mipsel-unknown-elf"))
            opt += "include/mipsel";
        else if(arch.equals("arm-none-eabi"))
            opt += "include/arm";

        return opt;
    }

    /* Get the current value of the given option using the MakeConfiguration supplied to this class.
     * The optionBookId is the name given to the mp:configurationObject in the Clang.languageToolchain.xml
     * file, such as "C32Global", "C32", "C32CPP", etc.  The optionId is the name of the option itself.
     * This will return the given default value if the option could not be read for some reason.
     */
// TODO:  Remove 'confBook' and 'conf' from this signature.
// TODO:  Does this need to be static?
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
// TODO:  Remove 'conf' from this signature.
    public String getToolchainBasePath(MakeConfiguration conf) {
        String toolchainPath = conf.getLanguageToolchain().getDir().getValue();

        if(File.separatorChar != toolchainPath.charAt(toolchainPath.length() - 1))
            toolchainPath += File.separator;

        return toolchainPath;
    }

}
