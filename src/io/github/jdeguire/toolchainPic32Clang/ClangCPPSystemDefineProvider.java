package io.github.jdeguire.toolchainPic32Clang;

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
        ProjectOptionAccessor optAccessor = new ProjectOptionAccessor(project, makeConf);
        String cppStd = optAccessor.getProjectOption("C32CPP", "cxx-standard", "gnu++14");

        switch(cppStd) {
            case "c++98":
            case "gnu++98":
                res.add("__cplusplus 199711L");
                break;
            case "c++11":
            case "gnu++11":
                res.add("__cplusplus 201103L");
                break;
            case "c++14":
            case "gnu++14":
                res.add("__cplusplus 201402L");
                break;
            case "c++17":
            case "gnu++17":
                res.add("__cplusplus 201703L");
                break;
            case "c++2a":
            case "gnu++2a":
            case "c++20":
            case "gnu++20":
                res.add("__cplusplus 202002L");
                break;
            default:
                res.add("__cplusplus 201402L");
                break;
        }
    }

}
