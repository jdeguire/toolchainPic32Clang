/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import org.apache.commons.io.IOUtils;
import org.openide.util.Utilities;

/** 
 * <pre>
 * Detects Clang toolchain license type. 
 * </pre> 
 * 
 * @author Constantin Dumitrascu <constantin.dumitrascu@microchip.com> 
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */
public final class ClangRuntimeProperties extends ClangAbstractTargetRuntimeProperties {

    private static String cachedClangVersion = "";
    private static String cachedClangPath = "";
    private static boolean updatingVersion = false;

    public ClangRuntimeProperties(final MakeConfigurationBook desc, final MakeConfiguration conf) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException,
		MakeConfigurationException {

        super(desc, conf);

        supressResponseFileOption();
        setImola2Properties(desc);

        // Setting an option ends up making a new instance of this class while the option is being
        // set, so here's a cheesy guard against that.  Otherwise, will get infinite recursion and
        // and a stack overflow exception.
        if(!updatingVersion) {
            updatingVersion = true;
            setClangVersionOption();
            updatingVersion = false;
        }
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
        if(Utilities.isWindows()) {
            value = "false";
        }
        setProperty("response-files.suppress", value);
    }

    private void setClangVersionOption()
                    throws IOException, IllegalArgumentException, MakeConfigurationException {
        File clangExec = new File(conf.getLanguageToolchain().getDir().getValue());

        // Check if the toolchain path changed and update our cached value if needed.
        if(!cachedClangPath.equals(clangExec.getPath())) {
            cachedClangPath = clangExec.getPath();

            if(Utilities.isWindows()) {
                clangExec = new File(clangExec, "clang.exe");
            } else {
                clangExec = new File(clangExec, "clang");
            }

            try {
                Process clangProc = new ProcessBuilder(clangExec.getPath(), "--version").start();

                if(clangProc.waitFor(3, TimeUnit.SECONDS)) {
                    InputStream inStream = clangProc.getInputStream();
                    String inString = IOUtils.toString(inStream, StandardCharsets.UTF_8.name());

                    int start = inString.indexOf("version");
                    if(start >= 0) {
                        start += "version".length();

                        while(Character.isWhitespace(inString.charAt(start))) {
                            ++start;
                        }

                        int end = start+1;
                        while(!Character.isWhitespace(inString.charAt(end))) {
                            ++end;
                        }

                        cachedClangVersion = inString.substring(start, end);
                    }
                }
            } catch(InterruptedException ex) {
                // Stack Overflow says to do this.
                Thread.currentThread().interrupt();
            }
        }

        // Check if we need to update the version shown in the project properties.  Doing this check
        // lets us ensure that we can update the option value even when our cached value has not 
        // changed.  This would presumably occur when switching projects.
        String currentVer = optAccessor.getProjectOption("C32Global", "clang-version", "");

        if(!currentVer.equals(cachedClangVersion)) {
            optAccessor.setProjectOption("C32Global", "clang-version", cachedClangVersion);
        }
    }
}
