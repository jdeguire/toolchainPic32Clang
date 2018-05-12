/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import org.openide.util.Utilities;

/**
 * <pre>
 * Detects XC32 toolchain license type.
 * </pre>
 *
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com>
 */
public class XC32RuntimeProperties extends XC32AbstractMipsRuntimeProperties {

    public XC32RuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        supressResponseFileOption();
        setLegacyLibcDefaultState(conf);
        setImola2Properties(desc);
    }

    private void setLegacyLibcDefaultState(MakeConfiguration conf) {
        Boolean legacyLibcEnabled = !pic32CSelected && LTUtils.toolchainVersionGreaterOrEqualTo("1.41", conf);
        setProperty("legacy-libc.default", legacyLibcEnabled.toString());
    }

    private void setImola2Properties(final MakeConfigurationBook projectDescriptor) {
        //default settings, in case projectDescriptor is corrupted.
        setProperty("Imola2.suppresor", Boolean.TRUE.toString());
        setProperty("Imola2.detected", Boolean.FALSE.toString());

        if (projectDescriptor == null) {
            //corrupted.
            return;
        }
        xPIC pic = getPic();
        if (pic == null) {
            //corrupted.
            return;
        }

        FamilyDefinitions.SubFamily subFamily = pic.getSubFamily();

        //avoid calling setProperty twice in case isImola is false.
        if (FamilyDefinitions.SubFamily.PIC32WK == subFamily) {
            setProperty("Imola2.suppresor", Boolean.FALSE.toString());
            setProperty("Imola2.detected", Boolean.TRUE.toString());
        }
    }

    private void supressResponseFileOption() {
        String value = "true";
        if (Utilities.isWindows()) {
            value = "false";
        }
        setProperty("opt-xc32-linker-response-files.suppress", value);
    }

}
