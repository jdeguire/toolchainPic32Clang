package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import java.util.List;
import org.netbeans.api.project.Project;

/**
 * System define provider for C files.
 * @author Marian Golea <marian.golea@microchip.com>
 *
 * Modified by jdeguire for toolchainPIC32Clang.
 */
public final class ClangCSystemDefineProvider extends ClangAbstractSystemDefineProvider {

    @Override
    void getDefinesHook(final MakeConfiguration makeConf, final Project project, final List<String> res) {
        ProjectOptionAccessor optAccessor = new ProjectOptionAccessor(project, makeConf);
        String cStd = optAccessor.getProjectOption("C32", "c-standard", "gnu11");

        switch(cStd) {
            case "c89":
            case "gnu89":
                // __STDC_VERSION__ is not defined in C89.
                break;
            case "c99":
            case "gnu99":
                res.add("__STDC_VERSION__ 199901L");
                break;
            case "c11":
            case "gnu11":
                res.add("__STDC_VERSION__ 201112L");
                break;
            case "c17":
            case "gnu17":
                res.add("__STDC_VERSION__ 201710L");
                break;
            default:
                res.add("__STDC_VERSION__ 201112L");
                break;
        }
    }
}
