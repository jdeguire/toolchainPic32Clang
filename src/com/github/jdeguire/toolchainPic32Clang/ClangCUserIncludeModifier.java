/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserIncludeModifier;
import java.util.List;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 *
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com>
 */
public final class ClangCUserIncludeModifier extends UserIncludeModifier {

    private final UserIncludeModifier uip;

    public ClangCUserIncludeModifier() {
        super(ClangCUserIncludeProvider.OPT_ID, ClangCUserIncludeProvider.OPT_PROP);
        uip = new UserIncludeModifier("C32Global", "common-include-directories");
    }

    @Override
    public List<String> getIncludes(final Project project, final ProjectConfiguration projectConf, final String itemPath) {
        List<String> includes = super.getIncludes(project, projectConf, itemPath);
        includes.addAll(uip.getIncludes(project, projectConf, itemPath));
        return includes;
    }
}
