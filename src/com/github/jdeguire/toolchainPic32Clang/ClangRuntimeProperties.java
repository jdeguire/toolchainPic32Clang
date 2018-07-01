/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
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
        
        setProperty("lto.link.threads.maxval", Integer.toString(maxthreads));
        setProperty("lto.link.threads.default", Integer.toString(defaultthreads));
    }

}
