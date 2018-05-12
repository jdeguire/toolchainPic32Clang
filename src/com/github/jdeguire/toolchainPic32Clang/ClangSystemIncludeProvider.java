package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.nbide.embedded.spi.IncludeProvider;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 *
 * @author jose
 */
public class XC32SystemIncludeProvider implements IncludeProvider {

    private static final Logger logger = MPLABLogger.mplog;

    public XC32SystemIncludeProvider() {
    }

    @Override
    public List<String> getIncludes(Project project, ProjectConfiguration conf, String itemPath) {
        
        //TODO Marian: temporarily commented out implementaiton since I added dynamic invocation of include paths for C.
        //TODO Marian: will remove this class once the new xc16/32 option to get both assembly and C paths is added.
        
        List<String> res = new ArrayList<String>();
//        MakeConfiguration makeConf = (MakeConfiguration) conf;
//        LanguageToolchain cs = makeConf.getLanguageToolchain().findToolchain();
//        if (cs == null) {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not get language tool chain");
//            return res;
//        }
//        LanguageTool t = cs.getTool(LanguageTool.CCompiler);
//        if (t == null) {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not get language");
//            return res;
//        }
//        String exe = t.getPath();
//        if (exe == null) {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not get path to executable");
//            return res;
//        }
//        String path = getDirName(exe);
//        if (path == null) {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not path");
//            return res;
//        }
//
//        LanguageTool lt = cs.getTool(LanguageTool.CCompiler);
//        if (lt == null) {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not get language tool");
//            return res;
//        }
//        // Was String version = lt.getVersion(); which calls the compiler
//        String version = LTUtils.getVersion(makeConf); // not sure why we need to do this since version is not used.
//        if (version != null && version.length() > 0) {
//            String gccVersion = "4.5.1";
//            if (LTUtils.toolchainVersionGreaterOrEqualTo("1.20", makeConf)){
//                gccVersion = "4.5.2";
//            }
//
//            res.add(path + "/../lib/gcc/pic32mx/" + gccVersion + "/include");
//            res.add(path + "/../lib/gcc/pic32mx/" + gccVersion + "/include-fixed");
//            if (CompilerProperties.buildWithGPP(project, makeConf)) {
//                res.add(path + "/../pic32mx/include/Cpp/c");
//                res.add(path + "/../pic32mx/include/Cpp");
//            }
//            res.add(path + "/../pic32mx/include");
//        } else {
//            logger.log(Level.SEVERE, "XC32SystemIncludeProvider::getIncludes, Could not get language tool version");
//            return res;
//        }
        return res;
    }

    /**
     * Same as the C library dirname function: given a path, return its
     * directory name. Unlike dirname, however, return null if the file is in
     * the current directory rather than ".".
     */
    public static final String getDirName(String path) {
        int sep = path.lastIndexOf('/');
        if (sep == -1) {
            sep = path.lastIndexOf('\\');
        }
        if (sep != -1) {
            return path.substring(0, sep);
        }
        return null;
    }
}
