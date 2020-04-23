/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.netbeans.api.project.Project;

/**
 * @author jose 
 * Modified by jdeguire for toolchainPic32Clang.
 * 
 * This seemed to originally be a dumping ground for some static variables, so I'm also making it a
 * dumping ground for useful static methods!
 * 
 * This class is referenced by the <mp:languageToolchain> node, which is the main top-level node
 * in Clang.languageToolchain.xml that describes pretty much everything.  The schema guide in the
 * MPLAB X SDK docs say thae following:
 * 
 * "The mp:class attribute is the fully qualified name of a class within the plugin JAR that will be
 * used as a reference for opening streams to resources in the same JAR."
 * 
 * I'm not at all sure what the means.
 */ 
public class ClangLanguageToolchain {
	// TODO:  We might be able to remove ones that do not apply to Clang; namely, the ones
	//        referenced in CommonPropertiesCalculator.
    public static final String CPP_SUPPORT_FIRST_VERSION = "0.01";
//    public static final String MDFP_SUPPORTED_VERSION = "2.20";
//    public static final String LIBC_SUPPORT_FIRST_VERSION = "1.41";
//    public static final String CHIPKIT_SUPPORT_FIRST_VERSION = "1.34";
//    public static final String MEMORY_SUMMARY_SUPPORT_FIRST_VERSION = "1.40";
//    public static final String SKIP_LICENSE_CHECK_SUPPORT_FIRST_VERSION = "1.43";
//    public static final String BUILD_COMPARISON_SUPPORT_FIRST_VERSION = "1.42";
//    public static final String MEM_RESERVATION_SUPPORT_FIRST_VERSION = "1.30";
    public static final String TARGET_CFG_PATH = "target/config";
    public static final String MIPS_LIB_DIR = "target/mips32/lib";
    public static final String CORTEX_M_LIB_DIR = "target/cortex-m/lib";
    public static final String CORTEX_A_LIB_DIR = "target/cortex-a/lib";

    private static final ArrayList<String> cachedTargetConfigContents = new ArrayList<String>();
    private static String cachedTargetConfigPath = "";


    /* Return the base install path for the current toolchain with the file separator always at the
     * end (so "/foo/bar/" instead of "/foo/bar").  The path returned is what would appear in the
     * MPLAB X Build Tool options; that is, it will be the where the executables are located and so
     * will end in "bin/".  This will use Unix forward slashes, even on Windows, because Clang 
     * supports them on all platforms.
     */
    public static String getToolchainExecPath(MakeConfiguration conf) {
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
    public static String getToolchainRootPath(MakeConfiguration conf) {
        String rootPath = getToolchainExecPath(conf);

        int i = rootPath.length() - 2;      // Last entry is '/', so skip it.
        while(i >= 0  &&  '/' != rootPath.charAt(i)) {
            --i;
        }

        return rootPath.substring(0, i+1);
    }

    /* If the given path starts with a '=', then it is meant to be relative to SYSROOT (where Clang
     * will assume is the base of its directory structure).  Convert such paths to absolute paths by
     * substituting the '=' for the actual SYSROOT path.
     * 
     * If the path does not start with '=' but is realtive, then this will convert it to an absolute
     * path assuming that the path should be relative to the project directory (the Project.X 
     * directory).  
     *
     * This will just return the given path if it does not start with '='.
     */
    public static String convertRelativeToAbsolutePath(MakeConfiguration conf,
                                                       Project proj,
                                                       String pathStr) {
        if('=' == pathStr.charAt(0)) {
            String rootPath = getToolchainRootPath(conf);
            rootPath = rootPath.substring(0, rootPath.length()-1);   // strip trailing '/'
            
            pathStr = rootPath + pathStr.substring(1);
        } else {
            Path p = Paths.get(pathStr);

            if(!p.isAbsolute()) {
                String projPath = proj.getProjectDirectory().getPath();

                pathStr = Paths.get(projPath, pathStr).normalize().toString();
            }
        }

        return pathStr;
    }

    /* Get the path to the target config file that will be used for the given target.  The user can
     * provide their own config via a project option, so we need to check for that and use that if
     * the user specified one.
     */
    public static String getTargetConfigPath(TargetDevice target, ProjectOptionAccessor optAccessor) {
        String userConfig = optAccessor.getProjectOption("C32Global", "user-target-config", "");

        if(!userConfig.isEmpty()) {
            return userConfig;
        } else {
            // This should be relative to SYSROOT because of the '=' at the start.
            String target_cfg = ClangLanguageToolchain.TARGET_CFG_PATH;
            return "=/" + target_cfg + "/" + target.getDeviceName().toLowerCase() + ".cfg";
        }
    }



    /* Return a list of macros that will be provided by this plugin to Clang to maintain some 
     * compatibility with code that expecting XC32.  Each macro will be formatted in a way suitable
     * to be used with the Clang or GCC "-D" option.  That is, each one will look like either
     * "MACRO_NAME" or "MACRO_NAME=MACRO_VALUE".  The version number to use for version macros is
     * provided as a user option and so must be read using the given accessor.
     */
    public static List<String> getXC32CompatibilityMacros(ProjectOptionAccessor optAccessor) {
        if(optAccessor.getBooleanProjectOption("C32Global", "fake-xc32", false)) {
            String compatVersion = optAccessor.getProjectOption("C32Global", "fake-xc32-version", "");

            if(!compatVersion.isEmpty()) {
                ArrayList<String> macros = new ArrayList<>(6);

                macros.add("__XC");
                macros.add("__XC__");
                macros.add("__XC32");
                macros.add("__XC32__");
                macros.add("__XC32_VERSION=" + compatVersion);
                macros.add("__XC32_VERSION__=" + compatVersion);

                return macros;
            }
        }

        return Collections.<String>emptyList();
    }

    /* Read the given target config file and return its contents in an array in which each entry is
     * a line that has had its leading and trailing whitespace removed.  Comments, which are lines
     * that start with '#', are ignored and will not appear in the output.
     */
    public static List<String> getTargetConfigContents(MakeConfiguration conf, 
                                                       Project proj,
                                                       String targetCfgPath)
                                                throws IOException {
        targetCfgPath = convertRelativeToAbsolutePath(conf, proj, targetCfgPath);

        if(!targetCfgPath.equals(cachedTargetConfigPath)) {
            updateTargetConfigCache(targetCfgPath);
            cachedTargetConfigPath = targetCfgPath;
        }

        return cachedTargetConfigContents;
    }

    /* Update our file contents cache by reading the given file, removing unneeded whitespace and
     * comments, and breaking the file into lines.
     */
    private static void updateTargetConfigCache(String targetCfgPath) throws IOException {
        File targetCfgFile = new File(targetCfgPath);
        List<String> lines = Files.readAllLines(targetCfgFile.toPath());

        cachedTargetConfigContents.clear();

        for(String line : lines) {
            line = line.trim();

            // Ignore empty lines and comments, which are lines that start with '#'.
            if(!line.isEmpty()  &&  '#' != line.charAt(0)) {
                cachedTargetConfigContents.add(line);
            }
        }
    }
}
