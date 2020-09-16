/* @author Jese DeGuire
 * Created: 9 Sep 2018
 *
 * MPLAB X has a built-in error parser for GCC errors, which is what lets you click on errors and 
 * warnings as though they were links to be taken to the location of the error.  However, the default
 * parser does not seem to handle ANSI color codes that Clang uses when the "-fcolor-diagnostics"
 * option is used and so MPLAB X will not find errors that are colorfied.  This class is an attempt
 * to help MPLAB X with those.
 */

package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.spi.ErrorParser;
import com.microchip.mplab.nbide.embedded.spi.GCCErrorParser;
import com.microchip.mplab.nbide.embedded.spi.OutputListenerFactory;
import java.io.IOException;
import org.netbeans.modules.nativeexecution.api.ExecutionEnvironment;
import org.openide.filesystems.FileObject;

/**
 */
public class ClangErrorParser implements ErrorParser {

    GCCErrorParser gccParser_;

    public ClangErrorParser(ExecutionEnvironment execEnv, FileObject relativeTo) {
        gccParser_ = new GCCErrorParser(execEnv, relativeTo);
    }


    @Override
    public Result handleLine(String string) throws IOException {
        Result res = gccParser_.handleLine(string);
        return res;
    }

    /* This is public in GCCErrorParser, so we should probably wrap it, too.
     */
    public OutputListenerFactory getListenerFactory(){
        return gccParser_.getListenerFactory();
    }
}
