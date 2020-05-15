package io.github.jdeguire.toolchainPic32Clang;

// TODO:  This logger might be handy for logging exceptions I can't throw.  Hold onto it for now.
//import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.spi.IncludeProvider;
import java.util.ArrayList;
import java.util.List;
//import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 * System include directory provider for Clang toolchain.
 * @author Jesse DeGuire
 * Normally I'd have the original author's name here from the XC32 plugin upon which this plugin
 * was based, but this class is very different from the XC32 version, so I'll take all the blame.
 */
public class ClangSystemIncludeProvider implements IncludeProvider {

//    TODO:  This logger might come in handy for logging exceptions I can't throw.  Hmmmm.
//    private static final Logger logger = MPLABLogger.mplog;

    /* Read system directories from the target config file and return them in a list.  MPLAB X is
     * supposed to invoke the compiler to figure this out, but it may not be including the target
     * config files when it does so, so we'll do this manually.
     *
     * Each supported device has a target config file that tells Clang things like CPU architecture,
     * macros, and directories.  The user can also specify a custom file through the project options.
     *
     * The 'itemPath' parameter appears to be null.
     */
    @Override
    public List<String> getIncludes(Project project, ProjectConfiguration conf, String itemPath) {
        List<String> res = new ArrayList<>();
        MakeConfiguration makeConf = (MakeConfiguration) conf;

        try {
            TargetDevice target = new TargetDevice(makeConf.getDevice().getValue());
            ProjectOptionAccessor optAccessor = new ProjectOptionAccessor(project, makeConf);
            String cfgPath = ClangLanguageToolchain.getTargetConfigPath(target, makeConf, optAccessor);
            List<String> cfgContents = ClangLanguageToolchain.getTargetConfigContents(makeConf, project, cfgPath);

            for(String line : cfgContents) {
                if(line.startsWith("-isystem")) {
                    // Remove "-isystem" and quotes since we don't need them here.
                    line = line.substring(8);
                    line = line.replace('\"', ' ');
                    line = line.trim();

                    // We need to convert to an absolute path here so MPLAB X can find it.
                    line = ClangLanguageToolchain.convertRelativeToAbsolutePath(makeConf, project, line);
                    res.add(line);
                }
            }
        } catch (Exception e) {
            // Do nothing for now because the interface this method implements does not throw,
            // meaning we are not allowed to, either.
        }

        return res;
    }
}
