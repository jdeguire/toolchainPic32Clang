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
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class ClangDynamicCapabilitiesProvider implements DynamicCapabilities{

    @Override
    public Pair<Boolean, String> hasCapability(Project project, ProjectConfiguration projectConf, String capability) {
        Pair<Boolean, String> res = new Pair<Boolean, String>(false, "");    // for the future assume false is default
        if (capability.equalsIgnoreCase("memoryfile")){
            MakeConfiguration conf = LTUtils.getMakeConfiguration(projectConf);
            res.first = ClangLanguageToolchain.supportsMemorySummary(conf);
            if (res.first){
                res.second = LTUtils.MEMORY_FILE_ADDRESS;
            }
        } else if (capability.equalsIgnoreCase(DefaultCompilerMacrosProvider.SKIP_LICENSE_CHECK_CAPABILITY)) {
            MakeConfiguration conf = LTUtils.getMakeConfiguration(projectConf);
            res.first = ClangLanguageToolchain.supportsSkipLicenseCheck(conf);
            if (res.first) {
                res.second = " -mskip-license-check";
            }
        } else if (capability.equalsIgnoreCase("parallel")) {
            res.first = true;
        } else if (capability.equalsIgnoreCase("responsefiles")){
            res.first = Utilities.isWindows();
        } else if (capability.equalsIgnoreCase(XMLBaseMakefileWriter.BUILD_COMPARISON)){
            MakeConfiguration conf = LTUtils.getMakeConfiguration(projectConf);
            res.second = ClangLanguageToolchain.getBuildComparisonCommandLineArgument(conf);
            res.first = res.second != null && !res.second.isEmpty();
        }
        return res;
    }
}
