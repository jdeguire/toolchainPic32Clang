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

// TODO:  LTUtils has static methods for accessing options:
// String readOptionValue(MakeConfigurationBook projectDescriptor, MakeConfiguration conf, String configurationFileID, String optionID)
// String readOptionEmittedValue(MakeConfigurationBook projectDescriptor, MakeConfiguration conf, String configurationFileID, String optionGroupID, String optionID)
//
// Can we make use of these to grab option values for checking architecture?

/**
 * <pre>
 * Detects Clang toolchain license type.
 * </pre>
 *
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com>
 */
public class ClangRuntimeProperties extends ClangAbstractMipsRuntimeProperties {

    public ClangRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        supressResponseFileOption();
        setLegacyLibcDefaultState(conf);
        setImola2Properties(desc);
    }

    private void setLegacyLibcDefaultState(MakeConfiguration conf) {
        Boolean legacyLibcEnabled = !pic32CSelected && LTUtils.toolchainVersionGreaterOrEqualTo("1.41", conf);
        setProperty("legacy-libc.default", legacyLibcEnabled.toString());
    }

    /* TODO:  "Imola2" appears to be Microchip's internal codename for the PIC32WK devices.
     *        These do not have built-in flash, but load from an on-package serial PROM.
     *        We should probably replace this with another name (if we can even support it).
     */
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
        setProperty("opt-Clang-linker-response-files.suppress", value);
    }

    /* TODO:  We will probably need to handle multilib stuff ourselves using getProperty() and 
     *        whatever linker options are set.  The base class seems to know that we have
     *        a PIC32C, so that should help.
     */
}
