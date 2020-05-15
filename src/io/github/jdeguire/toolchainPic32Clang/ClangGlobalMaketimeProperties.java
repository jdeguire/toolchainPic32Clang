package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;
import org.openide.util.Utilities;

/**
 * @author jose
 * Modified by Jesse DeGuire for toolchainPic32Clang.
*/
public class ClangGlobalMaketimeProperties extends CommonMaketimeProperties {

    public ClangGlobalMaketimeProperties(final MakeConfigurationBook projectDescriptor,
											final MakeConfiguration conf,
											final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);
        commandLineProperties.setProperty("USE_RESPONSE_FILES", getUseResponseFiles().toString());
        commandLineProperties.setProperty("USE_LTO", getUseLTO().toString());
        commandLineProperties.setProperty("CHOP_AR_LINE", getChoppedArchiverLine());
        commandLineProperties.setProperty("run_clang_tidy", getRunClangTidy().toString());
    }

    public final String getChoppedArchiverLine() {
        if (!Utilities.isWindows()) {
            return "false";
        }

        return optAccessor.getProjectOption("C32-AR", "additional-options-chop-files", "false");
    }

    public final Boolean getUseLTO() {
        Boolean ret = Boolean.FALSE;

        if(optAccessor.getBooleanProjectOption("C32Global", "wpo-lto", false)) {
            // ThinLTO is handled like a normal build, so we want to return False if ThinLTO is enabled.
            if(!optAccessor.getBooleanProjectOption("C32Global", "lto.enable-thin", false)) {
                ret = Boolean.TRUE;
            }
        }

        return ret;
    }

    public final Boolean getRunClangTidy() {
        if(optAccessor.getBooleanProjectOption("Tidy", "run-clang-tidy", false)) {
            return Boolean.TRUE;
        } else {
            return Boolean.FALSE;
        }
    }
}
