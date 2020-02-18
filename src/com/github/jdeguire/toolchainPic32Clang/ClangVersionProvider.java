package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.provider.CommonVersionProvider;

/**
 * Version provider for Clang toolchain.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class ClangVersionProvider extends CommonVersionProvider
{
    public ClangVersionProvider() {
// TODO:  This needs to read a PKGCONFIG file instead of asking Clang directly.
// TODO:  This code needs to be used to query the Clang version to show the user in the options pages.
        super("clang", "version\\s*([\\d\\.]+)", 1, false);
    }
}
