package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.provider.CommonVersionProvider;

/**
 * Version provider for xc32 toolchain.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class XC32VersionProvider extends CommonVersionProvider
{
    public XC32VersionProvider() {
        super("xc32-gcc", "Microchip.+v([\\d\\.]+)", 1, false);
    }
}
