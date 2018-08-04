/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

// TODO:  This file was remoed in SDK 5.00 for toolchainXC32.  Can we remove it, too?

import com.microchip.crownking.mplabinfo.FolderResidenceTester;
import com.microchip.crownking.mplabinfo.mpLanguageTool;
import com.microchip.crownking.mplabinfo.mpLanguageToolchain;
import com.microchip.mplab.nbide.embedded.api.LanguageToolchainMeta;
import com.microchip.mplab.nbide.embedded.api.LanguageToolchainMetaManager;
import java.util.Collections;
import java.util.Set;
import org.w3c.dom.Node;

/**
 * <pre>
 * For toolchains versions greater than 1.00 verify all executables for existence, otherwise, Clang-g++.exe is optional.
 * </pre>
 *
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com>
 */
public class ClangFolderResidenceTester extends FolderResidenceTester.Default {

    private LanguageToolchainMeta toolchainMeta;

    public ClangFolderResidenceTester() {
        toolchainMeta = LanguageToolchainMetaManager.getToolchain(
                "Clang"); // NOI18N
    }

    @Override
    public boolean isMyFolder(String dir) {
        boolean hasAllButCPP =
                hasAllExecutableFiles(toolchainMeta, dir, Collections.singleton("Clang-g++"));
        if (!hasAllButCPP) {
            return false;
        }
// TODO:  Actually fix this or just remove this file since we may not need it anymore.
//        final boolean requiregcc = ClangLanguageToolchain.xcHasCPPSupport(dir);
        final boolean requiregcc = true;
        if (requiregcc) {
            return hasAllExecutableFiles(toolchainMeta, dir);
        } else {
            return true;
        }
    }

    public static boolean hasAllExecutableFiles(mpLanguageToolchain toolchainMeta,
            String dir, Set<String> skip) {
        for (Node toolNode : toolchainMeta.getLanguageTools()) {
            mpLanguageTool toolMeta = new mpLanguageTool(toolNode);
            if (skip.contains(toolMeta.getID())) {
                continue;
            }
            String exeFilename = toolMeta.findActualExecutableFilename(dir);
            if (exeFilename == null) {
                return false;
            }
        }
        return true;
    }
}
