package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import java.util.List;
import org.netbeans.api.project.Project;

/**
 * System define provider for C files.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public final class ClangCSystemDefineProvider extends ClangAbstractSystemDefineProvider {

    @Override
    void getDefinesHook(final MakeConfiguration makeConf, final Project project, final List<String> res) {
        super.addCCIMacro(makeConf, project, res, "C32", "use-cci");
    }
}
