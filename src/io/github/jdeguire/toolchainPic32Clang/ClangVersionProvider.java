package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.spi.VersionProvider;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * Version provider for Clang toolchain.
 * @author Jesse DeGuire
 * Normally I'd have the original author's name here from the XC32 plugin upon which this plugin
 * was based, but this class is very different from the XC32 version, so I'll take all the blame.
 */
public class ClangVersionProvider implements VersionProvider
{
    private static String cachedVersion = "";
    private static String cachedPath = "";

    public ClangVersionProvider() {
    }

    /* Read the version number from a file in the toolchain root called "pic32clang_version".  Do
     * this instead of letting MPLAB X get the version because this will let us provide MPLAB X 
     * with our own version independent of Clang without having to modify Clang.
     *
     * The actual Clang version is shown under the global project properties and uses code in
     * ClangRuntimeProperties to do it.
     */
    @Override
    public String getVersion(String fullPathOrJustDirectory) {
        File file = new File(fullPathOrJustDirectory);

        // If this is a file, then assume for now that it's pointing to the Clang executable.
        if(file.isFile()) {
            file = file.getParentFile();
        }

        // We need to be at the root of the toolchain location, so move up if needed.
        if(file.getName().equals("bin")) {
            file = file.getParentFile();
        }

        // Check if we need to update our cache because we got a new directory.
        if(!cachedPath.equals(file.getPath())) {
            file = new File(file, ClangLanguageToolchain.VERSION_FILE_PATH);

            try(FileReader reader = new FileReader(file)) {
                char[] buf = new char[32];
                reader.read(buf);

                cachedPath = file.getParentFile().getPath();
                cachedVersion = new String(buf).trim();
            } catch(FileNotFoundException ex) {
                cachedVersion = "0.00";
            } catch (IOException ex) {
                cachedVersion =  "0.00";
            }
            // Cannot throw because the superclass method does not declare itself as throwing, so
            // the above will have to do for now.
        }

        return cachedVersion;
    }
}
