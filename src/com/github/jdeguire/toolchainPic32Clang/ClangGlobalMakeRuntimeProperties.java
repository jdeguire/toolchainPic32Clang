package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;
import org.netbeans.api.project.Project;
import org.openide.util.Utilities;

/**
 * @author jose
 * Modified by jdeguire for toolchainPic32Clang.
*/
public class ClangGlobalMakeRuntimeProperties extends CommonProperties {

    public ClangGlobalMakeRuntimeProperties(final MakeConfigurationBook projectDescriptor,
											final MakeConfiguration conf,
											final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);
        commandLineProperties.put("USE_RESPONSE_FILES", getUseResponseFiles());
        commandLineProperties.put("USE_LTO", getUseLTO());
        commandLineProperties.put("CHOP_AR_LINE", getChoppedArchiverLine());
    }

    public final String getChoppedArchiverLine() {
        if (!Utilities.isWindows()) {
            return "false";
        }

        return optAccessor.getProjectOption("C32-AR", "additional-options-chop-files", "false");
    }

    public final String getUseLTO() {
        String ret = "false";

        if(optAccessor.getBooleanProjectOption("C32Global", "wpo-lto", false)) {
            // ThinLTO is handled like a normal build, so we want this False if it is enabled.
            if(!optAccessor.getBooleanProjectOption("C32Global", "lto.enable-thin", false)) {
                ret = "true";
            }
        }

        return ret;
    }
}
