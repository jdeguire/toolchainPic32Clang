/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.properties.CommonLanguageToolchainPropertiesUtils;
import com.microchip.mplab.nbide.toolchainCommon.provider.CCISystemDefineProvider;
import com.microchip.crownking.mplabinfo.FamilyDefinitions.SubFamily;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 * Abstract system defines provider for Clang plugin.
 *
 * Marian: added getDefinesHook.
 * @author jose
 * @author Marian Golea <marian.golea@microchip.com>
 */
abstract class ClangAbstractSystemDefineProvider extends CCISystemDefineProvider{

    private final CommonLanguageToolchainPropertiesUtils calculator = new CommonLanguageToolchainPropertiesUtils();

    public String getDeviceNameMacro(String device) {
        String res = "";

        if (device.startsWith("PIC32")) {
            res = "__" + device.substring(3) + "__";       // "__32MX795F512L__"
        } else if(device.startsWith("ATSAM")) {
            res = "__" + device.substring(2) + "__";       // "__SAME70Q21__"
        } else if (!device.isEmpty()) {
            res = "__" + device + "__";
        }
        return res;
    }

    private void addDeviceSpecificMacros(List<String> list, TargetDevice target) {
        if(target.isMips32()) {
            String name = target.getDeviceName();
            
            if(SubFamily.PIC32 == target.getSubFamily()) {
                list.add("__PIC32_MEMORY_SIZE " + name.substring(11,14));
                list.add("__PIC32_MEMORY_SIZE__ " + name.substring(11,14));
                list.add("__PIC32_FEATURE_SET " + name.substring(7,10));
                list.add("__PIC32_FEATURE_SET__ " + name.substring(7,10));
                list.add("__PIC32_PIN_SET \'" + name.charAt(name.length()-1) + "\'");
                list.add("__PIC32_PIN_SET__ \'" + name.charAt(name.length()-1) + "\'");
            } if(SubFamily.PIC32MZ == target.getSubFamily()  ||  SubFamily.PIC32MM == target.getSubFamily()) {
                list.add("__PIC32_FLASH_SIZE " + name.substring(7,10));
                list.add("__PIC32_FLASH_SIZE__ " + name.substring(7,10));
                list.add("__PIC32_FEATURE_SET " + name.substring(11,13));
                list.add("__PIC32_FEATURE_SET__ " + name.substring(11,13));
                list.add("__PIC32_FEATURE_SET0 \'" + name.charAt(11) + "\'");
                list.add("__PIC32_FEATURE_SET0__ \'" + name.charAt(11) + "\'");
                list.add("__PIC32_FEATURE_SET1 \'" + name.charAt(12) + "\'");
                list.add("__PIC32_FEATURE_SET1__ \'" + name.charAt(12) + "\'");
                list.add("__PIC32_PRODUCT_GROUP \'" + name.charAt(13) + "\'");
                list.add("__PIC32_PRODUCT_GROUP__ \'" + name.charAt(13) + "\'");
                list.add("__PIC32_PIN_COUNT " + name.substring(name.length()-3));
                list.add("__PIC32_PIN_COUNT__ " + name.substring(name.length()-3));
            }
            // It looks like the PIC32MK and PIC32WK do not have these macros (as of XC32 v2.10).
        }
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
        List<String> res = new ArrayList<>();
        MakeConfiguration makeConf = (MakeConfiguration) projectConf;

        //add call to hook method, for inheritors who might want to do additional computations.
        getDefinesHook(makeConf, project, res);

        res.add("__clang__");
        res.add("__pic32_clang__");
        res.add("__LANGUAGE_C 1");
        res.add("__LANGUAGE_C__ 1");
        res.add("LANGUAGE_C 1");
        res.add("_LANGUAGE_C 1");
        res.add("__PIC32 1");
        res.add("__PIC32__ 1");

// TODO:  Change this to read the "-D" options from the target config file.
        try {
            TargetDevice target = new TargetDevice(makeConf.getDevice().getValue());
            String devname = target.getDeviceName();
            res.add(getDeviceNameMacro(devname));

            if(target.isMips32()) {
                res.add("__MIPSEL__ 1");
                res.add("MIPSEL 1");
                res.add("_MIPSEL 1");
                res.add("__R3000 1");
                res.add("__mips_isa_rev 2");
                res.add("_mips 1");
                res.add("__MIPSEL 1");
                res.add("__MIPSEL__ 1");
                res.add("mips 1");
                res.add("__mips__ 1");
                res.add("__mips 32");
                res.add("_ABIO32 1");
                res.add("_MIPS_SIM _ABIO32");

                String series;
                if(devname.startsWith("M")) {
                    series = devname.substring(0, 3);
                } else if(devname.startsWith("USB")) {
                    series = devname.substring(0, 5);
                } else {
                    series = devname.substring(0, 7);
                }
                res.add("__" + series + " 1");
                res.add("__" + series + "__ 1");

                if(devname.startsWith("PIC32")) {
                    res.add("__PIC32M 1");
                    res.add("__PIC32M__ 1");
                }
            }
            else {
                res.add("__arm__ 1");
                res.add("__ARMEL__ 1");
                res.add("__PIC32C 1");
                res.add("__PIC32C__ 1");
            }

            addDeviceSpecificMacros(res, target);

            if(target.hasFpu()) {
                if(target.isMips32())
                    res.add("__mips_hard_float 1");
// TODO:  Check if we're on Arm and have an FPU; set __ARM_FP to 4 (FPU32) or 14 (FPU64).
            } else {
                res.add("__SOFT_FLOAT 1");

                if(target.isMips32())
                    res.add("__mips_soft_float 1");
            }

// TODO:  Check if we're outputting XC32 compatibility macros and include those here if so.
// TODO:  Check if we enabled fast math and add __FAST_MATH__ if so.
        } catch (Exception e) {
            // Do nothing for now.
        }
        return res;
    }

}
