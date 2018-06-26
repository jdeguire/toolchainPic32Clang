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
 * Detects Clang toolchain license type. 
 * </pre> 
 * 
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com> 
 * Modified by jdeguire for toolchainPic32Clang.
 */
public class ClangRuntimeProperties extends ClangAbstractMipsRuntimeProperties {
    
    public ClangRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        supressResponseFileOption();
        setImola2Properties(desc);
        setThinLtoThreads();
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

    private void setThinLtoThreads() {
        int maxthreads = Runtime.getRuntime().availableProcessors();
        int defaultthreads = maxthreads / 2;
        
        if(maxthreads <= 0)
            maxthreads = 1;
        if(defaultthreads <= 0)
            defaultthreads = 1;
        
        setProperty("thin-lto.link.threads.maxval", Integer.toString(maxthreads));
        setProperty("thin-lto.link.threads.default", Integer.toString(defaultthreads));
    }

    /* TODO:  We will probably need to handle multilib stuff ourselves using LTUtils and 
     *        whatever linker options are set.
     *
     *   - Optimization level (0, 1, 2, 3, fast, s, z)
     *   - Arch (mips32r2, mips32r5, cortex m0/m4/m7/a5)
     *   - FPU (soft, mips, vfp4-sp-d16, vfp4-dp-d16, vfp5-dp-d16, neon-vfpv4)
     *   - fast-math enabled
     *   - mips32/mips16e/micromips
     *   - mips DSP
     *
     * We can use the EmbeddedProjectSupport.getSynthesizedOption(Project project, ProjectConfiguration projectConf, String optionBookID, String optionID, String itemPath)
     * method to get option values.  See ClangGlobalMakeRuntimeProperties.java.  This can be used to
     * determine multilibs and support for MIPS16e/microMIPS.
     */
}
