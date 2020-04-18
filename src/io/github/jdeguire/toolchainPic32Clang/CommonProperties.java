/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.properties.MPLABXSpecificProperties;
import java.io.File;
import java.util.List;
import java.util.Properties;
import org.openide.util.Utilities;

/**
 * Common runtime properties usable during makefile generation process.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by jdeguire for toolchainPic32Clang.
 */
public class CommonProperties extends MPLABXSpecificProperties {

    final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();
    final protected ProjectOptionAccessor optAccessor;
    final protected TargetDevice target;

    public CommonProperties(final MakeConfigurationBook projectDescriptor,
							final MakeConfiguration conf,
							final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);

        optAccessor = new ProjectOptionAccessor(projectDescriptor, conf);
        target = new TargetDevice(conf.getDevice().getValue());

        Boolean armDeviceSelected = target.isArm();
        commandLineProperties.put("IS_ARM", armDeviceSelected.toString());

        commandLineProperties.put("XC32_COMPAT_MACROS", getXC32CompatibilityMacros());
        commandLineProperties.put("SYSROOT_OPT", getSysrootOpt());
        commandLineProperties.put("TARGET_CONFIG_OPT", "--config \"" + getTargetConfigPath() + "\"");
    }

    final void addDebuggerNameOptions() {

        String debuggerOptionsAsSymbol = "";
        String debuggerOptionsAsMacro = "";
        String debuggerName = getCommonDebuggerMacro();
        if (debuggerName != null && debuggerName.length() > 0) {
            debuggerOptionsAsSymbol = "--defsym=" + debuggerName + "=1";
            debuggerOptionsAsMacro = "-D" + debuggerName + "=1";
        }

        // TODO:  Am I right in that we don't want this at all?
/*        if (debuggerOptionsAsSymbol.length() > 0) {
            commandLineProperties.put("COMMA_BEFORE_DEBUGGER_NAME", ",");
            String debuggerOptionString = target.isArm() ? "" : "-mdebugger";
            commandLineProperties.put("DEBUGGER_OPTION_TO_LINKER", debuggerOptionString);
        } else */{
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

// TODO:  Commented out code
        // Check the option value
//        OptionConfiguration confObject = desc.getSynthesizedOptionConfiguration(conf.getName(), "C32-LD", null);
//        if (confObject != null) {
//            try {
//                ClangRuntimeProperties props = new ClangRuntimeProperties(desc, conf);
//                List<Pair<String, String>> emissionPairs = confObject.getEmissionPairs(props, null);
//                if (emissionPairs != null) {
//                    for (Pair<String, String> p : emissionPairs) {
//                        if (p.first.equals("additional-options-use-response-files") && p.second.equals("true")) {
//                            res = true;
//                            break;
//                        }
//                    }
//                }
//            }
//            catch(Exception e) {
//                res = false;
//            }
//        }
//
//        return res;

        return optAccessor.getBooleanProjectOption("C32-LD",
                                                   "additional-options-use-response-files",
                                                   false);
    }

    /**
     * See return
     *
     * @param deviceName
     * @return value of processor to be fed to assembler. Override as needed
     */
    final String getProcessorNameForCompiler() {
        return target.getDeviceName();
    }

    private String getXC32CompatibilityMacros() {
        String ret = "";

        if(optAccessor.getBooleanProjectOption("C32Global", "fake-xc32", false)) {
            String compatVersion = optAccessor.getProjectOption("C32Global", "fake-xc32-version", "");

            if(!compatVersion.isEmpty()) {
                ret = "-D__XC -D__XC__ -D__XC32 -D__XC32__";
                ret += " -D__XC32_VERSION=" + compatVersion;
                ret += " -D__XC32_VERSION__=" + compatVersion;
            }
        }

        return ret;
    }

    private String getSysrootOpt() {
        return "--sysroot=\"" + getToolchainRootPath() + "\"";
    }
    
    private String getTargetConfigPath() {
        String userConfig = optAccessor.getProjectOption("C32Global", "user-target-config", "");

        if(!userConfig.isEmpty()) {
            return userConfig;
        } else {
            // This should be relative to SYSROOT because of the '=' at the start.
            return "=/target/config/" + target.getDeviceName().toLowerCase() + ".cfg";
        }
    }

    /* Return the base install path for the current toolchain with the file separator always at the
     * end (so "/foo/bar/" instead of "/foo/bar").  The path returned is what would appear in the
     * MPLAB X Build Tool options; that is, it will be the where the executables are located and so
     * will end in "bin/".  This will use Unix forward slashes, even on Windows, because Clang 
     * supports them on all platforms.
     */
    public String getToolchainExecPath() {
        String execPath = conf.getLanguageToolchain().getDir().getValue();

        if('/' != File.separatorChar)
            execPath = execPath.replace(File.separatorChar, '/');

        if('/' != execPath.charAt(execPath.length() - 1))
            execPath += '/';

        return execPath;
    }

    /* Like above, but will return the top-level path of the Clang toolchain; that is, the parent
     * of the path returned by getToolchainExecPath().  This path will end in '/'.
     */
    public String getToolchainRootPath() {
        String rootPath = getToolchainExecPath();

        int i = rootPath.length() - 2;      // Last entry is '/', so skip it.
        while(i >= 0  &&  '/' != rootPath.charAt(i)) {
            --i;
        }

        return rootPath.substring(0, i+1);
    }
}
