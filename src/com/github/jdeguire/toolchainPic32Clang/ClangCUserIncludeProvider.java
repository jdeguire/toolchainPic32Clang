package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.UserIncludeProvider;
import java.util.List;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 *
 * @author drmc
 */
public final class ClangCUserIncludeProvider extends UserIncludeProvider {
    public static final String OPT_ID = "C32";
    public static final String OPT_PROP = "extra-include-directories";

    private final UserIncludeProvider uip;

    public ClangCUserIncludeProvider() {
        super(OPT_ID, OPT_PROP);
        uip = new UserIncludeProvider("C32Global", "common-include-directories");
    }

    @Override
    public List<String> getIncludes(final Project project, final ProjectConfiguration projectConf, final String itemPath) {
        List<String> includes = super.getIncludes(project, projectConf, itemPath);
        includes.addAll(uip.getIncludes(project, projectConf, itemPath));
        return includes;
    }
}
