package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.XMLBaseMakefileWriter;
import com.microchip.mplab.nbide.embedded.spi.DynamicCapabilities;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.provider.DefaultCompilerMacrosProvider;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;
import org.openide.util.Utilities;

/**
 * Dynamic capabilities provider for Clang compiler tool plugin.
 *
 * @author Marian Golea <marian.golea@microchip.com>
 */
public final class ClangDynamicCapabilitiesProvider implements DynamicCapabilities {

	private final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();

    @Override
   public Pair<Boolean, String> hasCapability(final Project project, final ProjectConfiguration projectConf, final String capability) {
        Pair<Boolean, String> res = new Pair<>(false, "");    // for the future assume false is default
        MakeConfiguration conf = calc.getMakeConfiguration(projectConf);
        if (capability.equalsIgnoreCase("memoryfile")) {
            res.first = calc.supportsMemorySummary(conf);
            if (res.first) {
                res.second = LTUtils.MEMORY_FILE_ADDRESS;
            }
        } else if (capability.equalsIgnoreCase(DefaultCompilerMacrosProvider.SKIP_LICENSE_CHECK_CAPABILITY)) {
            res.first = calc.supportsSkipLicenseCheck(conf);
            if (res.first) {
                res.second = " -mskip-license-check";
            }
        } else if (capability.equalsIgnoreCase("parallel")) {
            res.first = true;
        } else if (capability.equalsIgnoreCase("responsefiles")) {
            res.first = Utilities.isWindows();
        } else if (capability.equalsIgnoreCase(XMLBaseMakefileWriter.BUILD_COMPARISON)) {
            boolean supported = calc.supportsBuildComparison(conf);
            res.second = supported ? "-mafrlcsj" : "";
            res.first = res.second != null && !res.second.isEmpty();
        }
        return res;
    }
}
