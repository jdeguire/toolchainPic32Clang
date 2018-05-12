package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.OptionConfiguration;
import java.util.List;
import java.util.Properties;
import org.netbeans.api.project.Project;
import org.openide.util.Utilities;

/**
 *
 * @author jose
 */
public class XC32GlobalMakeRuntimeProperties {

    public XC32GlobalMakeRuntimeProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        commandLineProperties.put("USE_RESPONSE_FILES", getUseResponseFiles(projectDescriptor, conf));
        commandLineProperties.put("USE_LTO", getUseLTO(projectDescriptor, conf));
        commandLineProperties.put("CHOP_AR_LINE", getChoppedArchiverLine(projectDescriptor, conf));
    }

    public final String getChoppedArchiverLine(MakeConfigurationBook projectDescriptor, MakeConfiguration projectConf) {
        String ret = "false";
        if (!Utilities.isWindows()) {
            return ret;
        }
        final Project project = projectDescriptor.getProject();
        if (null != project) {
            ret = EmbeddedProjectSupport.getSynthesizedOption(project, projectConf, "C32-AR", "additional-options-chop-files", null); // NOI18N
            if (ret == null) {
                ret = "false";
            }
        }
        return ret;
    }
    
    public final String getUseLTO(MakeConfigurationBook projectDescriptor, MakeConfiguration projectConf) {
        String ret = "false";
        final Project project = projectDescriptor.getProject();
        if (null != project) {
            ret = EmbeddedProjectSupport.getSynthesizedOption(project, projectConf, "C32Global", "wpo-lto", null); // NOI18N
            if (ret == null) {
                ret = "false";
            }
        }
        return ret;
    }

    public static boolean getUseResponseFiles(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        boolean res = false;
        if (!Utilities.isWindows()) {
            return res;
        }
        // Check the option value
        OptionConfiguration confObject = projectDescriptor.getSynthesizedOptionConfiguration(conf.getName(), "C32-LD", null);
        if (confObject != null) {
            XC32RuntimeProperties rtp = new XC32RuntimeProperties(projectDescriptor, conf);
            if (rtp != null) {
                XC32RuntimeProperties props = rtp;
                if (props != null) {
                    List<Pair<String, String>> emissionPairs = confObject.getEmissionPairs(props, null);
                    if (emissionPairs != null) {
                        for (Pair p : emissionPairs) {
                            if (p.first.equals("additional-options-use-response-files") && p.second.equals("true")) {
                                res = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
        return res;
    }
}
