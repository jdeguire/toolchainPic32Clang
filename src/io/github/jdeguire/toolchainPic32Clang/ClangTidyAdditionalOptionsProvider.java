package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.spi.configurations.AdditionalOptionsProvider;

/**
 * @author Jesse DeGuire
 *
 * I have no idea what this does and it's not mentioned in the SDK docs, but other tools have them
 * so I guess I'll include this for clang-tidy until I figure out otherwise.
 */
public final class ClangTidyAdditionalOptionsProvider extends AdditionalOptionsProvider {
    
    public static final String OPT_ID = "Tidy";

    public ClangTidyAdditionalOptionsProvider() {
        super(OPT_ID);
    }

}
