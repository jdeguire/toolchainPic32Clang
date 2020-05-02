/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.DeviceSupport;
import com.microchip.mplab.nbide.embedded.spi.DeviceServiceProvider;
import java.io.File;
import java.util.Collection;
import java.util.HashSet;
import org.apache.commons.io.FileUtils;

/**
 * @author Jesse DeGuire
 * Normally I'd have the original author's name here from the XC32 plugin upon which this plugin
 * was based, but this class is very different from the XC32 version, so I'll take all the blame.
 */
public final class ClangDeviceServiceProvider implements DeviceServiceProvider {

    private final HashSet<String> deviceCache;
    private String cachedPath;
    private String cachedVersion;

    public ClangDeviceServiceProvider() {
        deviceCache = new HashSet<>(64);
        cachedPath = "";
        cachedVersion = "";
    }

    @Override
    public DeviceSupport.ToolSupport isDeviceSupported(String compilerPath, String compilerVersion, String deviceName) {
        DeviceSupport.ToolSupport toolSupport = DeviceSupport.ToolSupport.RED;

        if (!compilerPath.isEmpty()  &&  deviceName != null) {
            if(!compilerPath.equals(cachedPath)  ||  !compilerVersion.equals(cachedVersion)) {
                populateDeviceCache(compilerPath);
                cachedPath = compilerPath;
                cachedVersion = compilerVersion;
            }

            if(deviceCache.contains(deviceName.toLowerCase())) {
                toolSupport = DeviceSupport.ToolSupport.GREEN;
            }
        }

        return toolSupport;
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

        file = new File(file, ClangLanguageToolchain.TARGET_CFG_DIR);

        if(file.exists()) {
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
