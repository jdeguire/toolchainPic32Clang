/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.provider.CCISystemDefineProvider;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 * Abstract system defines provider for xc32 plugin.
 *
 * Marian: added getDefinesHook.
 * @author jose
 * @author Marian Golea <marian.golea@microchip.com>
 */
abstract class XC32AbstractSystemDefineProvider extends CCISystemDefineProvider{
    
    public String getDeviceNameMacro(String device) {
        String res = "";

        if (device.startsWith("PIC32")) {
            res = "__" + device.substring(3) + "__";
        } else if (device.startsWith("MEC")) {
            res = "__" + device + "__";
        }
        return res;
    }

    /**
     * Hook method for getDefines. Inheritors are forced to implement this method. They can mock it if no additional behavior is needed.
     * @param makeConf make configuration
     * @param project project
     * @param res list of already computed macros
     */
    abstract void getDefinesHook(final MakeConfiguration makeConf, final Project project, final List<String> res);

    @Override
    public List<String> getDefines(Project project,
                                   ProjectConfiguration projectConf,
                                   String itemPath) {
        List<String> res = new ArrayList<String>();
        MakeConfiguration makeConf = (MakeConfiguration) projectConf;
        String device = makeConf.getDevice().getValue();
        String deviceMacro = null;
        if (device != null)
            deviceMacro = getDeviceNameMacro(device);
        if (deviceMacro != null)
            res.add(deviceMacro);

        //add call to hook method, for inheritors who might want to do additional computations.
        getDefinesHook(makeConf, project, res);

        res.add("__LANGUAGE_C 1");
        res.add("__LANGUAGE_C__ 1");
        res.add("__MIPSEL__ 1");
        res.add("_MCHP_SZPTR 32");
        res.add("MIPSEL 1");
        res.add("_MIPSEL 1");
        res.add("_MIPS_ARCH_PIC32MX 1");
        res.add("__R3000 1");
        res.add("__mips_isa_rev 2");
        res.add("_mips 1");
        res.add("__MIPSEL 1");
        res.add("mips 1");
        res.add("PIC32MX 1");
        res.add("__mips__ 1");
        res.add("LANGUAGE_C 1");
        res.add("_LANGUAGE_C 1");
        res.add("__mips 32");


        res.add("_ABIO32 1");
        res.add("_MIPS_SIM _ABIO32");

        res.add("_MIPS_SZINT 32");
        res.add("_MCHP_SZINT 32");
        res.add("_MIPS_SZLONG 32");
        res.add("_MCHP_SZLONG 32");
        res.add("_MIPS_SZPTR 32");
        res.add("_MCHP_SZPTR 32");

        res.add("__SOFT_FLOAT");
        res.add("__mips_soft_float");
        res.add("__SINGLE_FLOAT");

        res.add("__XC32 1");
        res.add("__XC32__ 1");

        if (device.startsWith("MEC")) {
            res.add("__MEC 1");
            res.add("__MEC__ 1");
            res.add("MEC 1");
        }


        if(isExcludeFloatingPointLib()){
            res.add("__NO_FLOAT");
            res.add("__mips_no_float");
            res.add("__mchp_no_float");
        }


        if (device != null && device.length() >= 10) {
            String set = device.substring(7,10);
            res.add("__PIC32_FEATURE_SET__ " + set);
        }
        res.add("__C32__");
        String version = LTUtils.getVersion(makeConf);
        if (version.startsWith("2.")) {
            res.add("__STDC_HOSTED__");
        }
        try {
            Integer C32VERSIONMACRO = Integer.parseInt(version.replaceAll("([a-zA-Z.])+", ""));
            res.add("__C32_VERSION__ " + String.valueOf(C32VERSIONMACRO));
        } catch (NumberFormatException ex) {
            MPLABLogger.mplog.log(Level.INFO, ex.getMessage());
        }

        return res;
    }

    private boolean isExcludeFloatingPointLib() {
        //TODO Ask Dave how we can get hold of opt runtime options here.
        return false;
    }
}
