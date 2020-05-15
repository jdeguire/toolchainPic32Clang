/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.spi.DefineProvider;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 * Abstract system defines provider for Clang plugin.
 *
 * Marian: added getDefinesHook.
 * @author jose
 * @author Marian Golea <marian.golea@microchip.com>
 * 
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */
abstract class ClangAbstractSystemDefineProvider implements DefineProvider {

    /* Search for the first occurrence of 'find' in the given string and replace it with 'repl'.
     * Oddly enough, there's no String method to do this in Java 7.  The ones present either use
     * a regex or replace all characters.  This returns the original string if the desired character
     * was not found.
     */
    private String replaceFirstChar(String str, char find, char repl) {
        int pos = str.indexOf(find);

        if(pos < 0) {
            // Not found.
            return str;
        } else if(0 == pos) {
            // Start of string.
            return repl + str.substring(1);
        } else if((str.length()-1) == pos) {
            // End of string.
            return str.substring(0, pos) + repl;
        } else {
            // Middle of string.
            return str.substring(0, pos) + repl + str.substring(pos+1);
        }
    }

    /* Read the target config file and add macros defined in there to our list of predefined macros.
     * Each device supported by this toolchain will have a file with compiler options, such as 
     * macros and CPU architecture, in it to be passed to Clang.  The user can also provide their
     * own config file, so we need the ProjectOptionAccessor to check for it.
    */
    private void addTargetSpecificMacros(List<String> list,
                                         TargetDevice target,
                                         MakeConfiguration makeConf,
                                         Project proj) 
                                         throws IOException {
        ProjectOptionAccessor optAccessor = new ProjectOptionAccessor(proj, makeConf);
        String cfgPath = ClangLanguageToolchain.getTargetConfigPath(target, makeConf, optAccessor);
        List<String> cfgContents = ClangLanguageToolchain.getTargetConfigContents(makeConf, proj, cfgPath);

        // Find lines that contain Clang options to define a macro.  Clang wants an '=' between a 
        // macro name and value, but MPLAB X wants a space, so we have to replace those.
        for(String line : cfgContents) {
            if(line.startsWith("-D")  &&  !Character.isWhitespace(2)) {
                list.add(replaceFirstChar(line.substring(2), '=', ' '));
            } else if(line.startsWith("--define-macro")) {
                if('=' == line.charAt(14)  ||  ' ' == line.charAt(14)) {
                    list.add(replaceFirstChar(line.substring(15), '=', ' '));
                }
            }
        }
    }

    /* If the option was selected by the user, add macros that are provided by the plugin to Clang
     * to keep some compatibility with code expecting XC32.
     */
    private void addXC32CompatibilityMacros(List<String> list,
                                            ProjectOptionAccessor optAccessor) {
        List<String> macros = ClangLanguageToolchain.getXC32CompatibilityMacros(optAccessor);

        for(String macro : macros) {
            list.add(replaceFirstChar(macro.substring(2), '=', ' '));
        }
    }

    /**
     * Hook method for getDefines. Inheritors are forced to implement this method. They can mock it if no additional behavior is needed.
     * @param makeConf make configuration
     * @param project project
     * @param res list of already computed macros
     */
    abstract void getDefinesHook(final MakeConfiguration makeConf, final Project project, final List<String> res);

    /* Provide MPLAB X with a list of predefined macros.  Some are actually built-in based on the
     * target device's CPU architecture, some are always present in Clang, and other needs to be 
     * read from user options or from the target config file.  MPLAB X is supposed to invoke the 
     * compiler to figure this out, but it may not be including the target config files when it 
     * does so, so we'll do this manually.
     *
     * Each supported device has a target config file that tells Clang things like CPU architecture,
     * macros, and directories.  The user can also specify a custom file through the project options.
     *
     * The 'itemPath' parameter seems to be the name of the source file being currently parsed.
     */
    @Override
    public List<String> getDefines(Project project,
                                   ProjectConfiguration projectConf,
                                   String itemPath) {
        List<String> res = new ArrayList<>();
        MakeConfiguration makeConf = (MakeConfiguration) projectConf;

        //add call to hook method, for inheritors who might want to do additional computations.
        getDefinesHook(makeConf, project, res);

        res.add("__clang__");
        res.add("__LANGUAGE_C 1");
        res.add("__LANGUAGE_C__ 1");
        res.add("LANGUAGE_C 1");
        res.add("_LANGUAGE_C 1");

        try {
            TargetDevice target = new TargetDevice(makeConf.getDevice().getValue());
            ProjectOptionAccessor optAccessor = new ProjectOptionAccessor(project, makeConf);

            if(target.isMips32()) {
                res.add("__MIPSEL__ 1");
                res.add("MIPSEL 1");
                res.add("_MIPSEL 1");
                res.add("__R3000 1");
                res.add("__mips_isa_rev 2");
                res.add("_mips 1");
                res.add("__MIPSEL 1");
                res.add("__MIPSEL__ 1");
                res.add("mips 1");
                res.add("__mips__ 1");
                res.add("__mips 32");
                res.add("_ABIO32 1");
                res.add("_MIPS_SIM _ABIO32");
            } else {
                res.add("__arm__ 1");
                res.add("__ARMEL__ 1");
            }

            addTargetSpecificMacros(res, target, makeConf, project);
            addXC32CompatibilityMacros(res, optAccessor);

            if(target.hasFpu()) {
                if(target.isMips32()) {
                    res.add("__mips_hard_float 1");
                } else {
                    if(target.hasFpu64()) {
                        res.add("__ARM_FP 12");
                    } else {
                        res.add("__ARM_FP 4");
                    }
                }
            } else {
                res.add("__SOFT_FLOAT 1");

                if(target.isMips32())
                    res.add("__mips_soft_float 1");
            }

            // Check if we enabled fast math and add __FAST_MATH__ if so.
            if(optAccessor.getBooleanProjectOption("C32Global", "relaxed-math", false)) {
                res.add("__FAST_MATH__");
            }

        } catch (Exception e) {
            // Do nothing for now because the interface this method implements does not throw,
            // meaning we are not allowed to, either.
        }

        return res;
    }

}
