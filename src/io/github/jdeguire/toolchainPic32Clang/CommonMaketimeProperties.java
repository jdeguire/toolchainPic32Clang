/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.properties.MPLABXSpecificProperties;
import java.util.List;
import java.util.Properties;
import org.openide.util.Utilities;

/**
 * Common runtime properties usable during makefile generation process.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */
public class CommonMaketimeProperties extends MPLABXSpecificProperties {

    final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();
    final protected ProjectOptionAccessor optAccessor;
    final protected TargetDevice target;

    public CommonMaketimeProperties(final MakeConfigurationBook projectDescriptor,
							final MakeConfiguration conf,
							final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);

        optAccessor = new ProjectOptionAccessor(projectDescriptor.getProject(), conf);
        target = new TargetDevice(conf.getDevice().getValue());

        String toolchain_root = ClangLanguageToolchain.getToolchainRootPath(conf);
        String config_path = ClangLanguageToolchain.getTargetConfigPath(target, conf, optAccessor);

        commandLineProperties.setProperty("xc32_compat_macros", getXC32CompatibilityMacroOptions());
        commandLineProperties.setProperty("sysroot_opt", "--sysroot=\"" + toolchain_root + "\"");
        commandLineProperties.setProperty("target_config_opt", "--config \"" + config_path + "\"");
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
            commandLineProperties.setProperty("COMMA_BEFORE_DEBUGGER_NAME", ",");
        } else {
            commandLineProperties.setProperty("COMMA_BEFORE_DEBUGGER_NAME", "");
        }

        commandLineProperties.setProperty("DEBUGGER_NAME_AS_SYMBOL", debuggerOptionsAsSymbol);
        commandLineProperties.setProperty("DEBUGGER_NAME_AS_MACRO", debuggerOptionsAsMacro);
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

    final Boolean getUseResponseFiles() {
        if (!Utilities.isWindows()) {
            return false;
        } else {
            return optAccessor.getBooleanProjectOption("C32-LD",
                                                       "additional-options-use-response-files",
                                                       false);
        }
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

    private String getXC32CompatibilityMacroOptions() {
        String ret = "";
        List<String> macros = ClangLanguageToolchain.getXC32CompatibilityMacros(optAccessor);

        for(String macro : macros) {
            ret += "-D" + macro + " ";
        }

        return ret;
    }
}
