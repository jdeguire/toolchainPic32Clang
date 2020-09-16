/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.crownking.mplabinfo.FamilyDefinitions.Family;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.crownkingx.xPICFactory;
import com.microchip.mplab.nbide.embedded.spi.DeviceServiceProvider;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import org.apache.commons.io.FileUtils;

/**
 * @author Jesse DeGuire
 * Normally I'd have the original author's name here from the XC32 plugin upon which this plugin
 * was based, but this class is very different from the XC32 version, so I'll take all the blame.
 */
public final class ClangDeviceServiceProvider implements DeviceServiceProvider {

    private static final HashSet<String> deviceCache = new HashSet<>(64);
    private static String cachedPath = "";
    private static String cachedVersion = "";

    public ClangDeviceServiceProvider() {
    }


    /* I have no idea what this does, but it's apparently required.  This just calls the default
     * implementation in the interface.
     */
    @Override
    public Pair<Boolean, String> hasPackCompatibilityPreRequisite() {
        return DeviceServiceProvider.super.hasPackCompatibilityPreRequisite();
    }

    /* I can only guess for now, but I suspect this is used to see if a toolchain could in theory
     * support a particular device.  The intent here is to support 32-bit devices, so this will
     * return True if the given device name refers to a 32-bit device.
     */
    @Override
    public boolean isDeviceSupported(String devname) {
        boolean isSupported = false;

        try {
            xPIC pic = (xPIC)xPICFactory.getInstance().get(devname);

            if(FamilyDefinitions.Family.PIC32 == pic.getFamily()  ||  
               FamilyDefinitions.Family.ARM32BIT == pic.getFamily()) {
                isSupported = true;
            }

            xPICFactory.getInstance().release(pic);

        } catch (Exception e) {
            // Do nothing for now.
        }

        return isSupported;
    }

    /* This is used by MPLAB X to ask if a single device is supported by the currently-selected 
     * toolchain.  We check for support by seeing if the toolchain has a target config file for
     * the given device.
     */
    @Override
    public boolean isDeviceSupported(String compilerPath, String compilerVersion, String deviceName) {
        boolean isSupported = false;

        if (!compilerPath.isEmpty()  &&  deviceName != null) {
            if(!compilerPath.equals(cachedPath)  ||  !compilerVersion.equals(cachedVersion)) {
                populateDeviceCache(compilerPath);
                cachedPath = compilerPath;
                cachedVersion = compilerVersion;
            }

            if(deviceCache.contains(deviceName.toLowerCase())) {
                isSupported = true;
            }
        }

        return isSupported;
    }

    /* This class probably gets called a bunch of times, so just in case we'll keep a cache of
     * supported devices that we can just look at when MPLAB X asks if we support a particular
     * device.  This essentially just makes a list of all of the target config files bundled with
     * the toolchain.
     */
    private void populateDeviceCache(String compilerPath) {
        deviceCache.clear();

        File file = new File(compilerPath);

        // If this is a file, then assume for now that it's pointing to the Clang executable.
        if(file.isFile()) {
            file = file.getParentFile();
        }

        // We need to be at the root of the toolchain location, so move up if needed.
        if(file.getName().equals("bin")) {
            file = file.getParentFile();
        }

        file = new File(file, ClangLanguageToolchain.TARGET_DIR);

        // The target directory is organized by architecture family ("mips32" vs. "cortex-a", for 
        // example), and each family has a "config" subdirectory with the target config files. Walk
        // through these directories to build the full list of supported devices.
        File[] targetFiles = file.listFiles();        
        for(File tf : targetFiles) {
            if(tf.isDirectory()) {
                File configFile = new File(tf, "config");

                if(configFile.isDirectory()) {
                    String exts[] = {"cfg", "CFG"};
                    // MPLAB X has a very old Apache Commons package that doesn't support Java generics.
                    @SuppressWarnings("unchecked")
                    Collection<File> cfgFiles = FileUtils.listFiles(file, exts, true);

                    for(File f : cfgFiles) {
                        String basename = f.getName();
                        basename = basename.substring(0, basename.lastIndexOf('.')).toLowerCase();

                        deviceCache.add(basename);
                    }
                }
            }
        }
    }
}
