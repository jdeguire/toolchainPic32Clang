package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.toolchainCommon.provider.CommonVersionProvider;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * Version provider for Clang toolchain.
 * @author Marian Golea <marian.golea@microchip.com>
 */
public class ClangVersionProvider extends CommonVersionProvider
{
    public ClangVersionProvider() {
// TODO:  This needs to read a PKGCONFIG file instead of asking Clang directly.
// TODO:  This code needs to be used to query the Clang version to show the user in the options pages.
        super("clang", "version\\s*([\\d\\.]+)", 1, false);
    }

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

        String verPath = file.getAbsolutePath();
        if(!verPath.endsWith(File.separator)) {
            verPath += File.separator;
        }
        verPath += "pic32clang_version";

        try(FileReader reader = new FileReader(verPath)) {
            char[] buf = new char[32];
            reader.read(buf);

            return new String(buf);
        } catch(FileNotFoundException ex) {
            return "0.00";
        } catch (IOException ex) {
            return "0.00";
        }
    }
}
