package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.provider.CommonVersionProvider;

/**
 * Version provider for Clang toolchain.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class ClangVersionProvider extends CommonVersionProvider
{
    public ClangVersionProvider() {
        super("Clang-gcc", "Microchip.+v([\\d\\.]+)", 1, false);
    }
}
