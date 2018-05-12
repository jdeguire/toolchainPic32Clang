package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import java.util.List;
import org.netbeans.api.project.Project;

/**
 * System define provider for C++ files.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class XC32CPPSystemDefineProvider extends XC32AbstractSystemDefineProvider {

    @Override
    void getDefinesHook(MakeConfiguration makeConf, Project project, List<String> res) {
        super.addCCIMacro(makeConf, project, res, "C32CPP", "use-cci");
    }

}
