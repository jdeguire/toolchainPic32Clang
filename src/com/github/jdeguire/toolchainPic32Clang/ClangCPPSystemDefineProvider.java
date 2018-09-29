package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import java.util.List;
import org.netbeans.api.project.Project;

/**
 * System define provider for C++ files.
 * @author Marian Golea <marian.golea@microchip.com>
 *
 * Modified by jdeguire for toolchainPIC32Clang.
 */
public final class ClangCPPSystemDefineProvider extends ClangAbstractSystemDefineProvider {

    @Override
    void getDefinesHook(final MakeConfiguration makeConf, final Project project, final List<String> res) {
        // This is apparently required, so just do nothing.
    }

}
