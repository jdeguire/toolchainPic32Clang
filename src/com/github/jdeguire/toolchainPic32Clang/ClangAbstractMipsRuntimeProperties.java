/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.LanguageToolRuntimePropertiesAccessor;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import org.netbeans.api.project.Project;

/**
 * Handle the common behavior related to mips16 and micromips instruction 
 * sets.
 * This functionality is applicable to both GCC and GPP compilers. 
 * 
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by jdeguire for toolchainPic32Clang.
 */ 
// TODO: Rename this class because it has ARM stuff, too (Mips->Target ?).
public abstract class ClangAbstractMipsRuntimeProperties extends LanguageToolRuntimePropertiesAccessor {

    private enum TargetFamily {
      Unknown,
      PIC32MX,          // MIPS32r2, no FPU/DSP, MIPS16e
      PIC32MZ,          // MIPS32r2, DSP, microMIPS, no FPU
      PIC32MZEF,        // MIPS32r5, DSP, microMIPS, FPU -- EF is different from other MZ
      PIC32MK,          // MIPS32r5, DSP, microMIPS, FPU
      PIC32WK,          // MIPS32r5, DSP, microMIPS, FPU
      PIC32MM,          // MIPS32r2, microMIPS only, no FPU/DSP
      PIC32CX,          // Cortex-M4, no FPU
      PIC32CZ,          // Cortex-M7, FPv5 D16 FPU
      SAMD,             // Cortex-M0+, no FPU
      SAML,             // Cortex-M23, no FPU -- SAML1x is first to use M23
      SAML2,            // Cortex-M0+, no FPU -- the SAML2x is older and so uses the older M0+ core
      SAMC,             // Cortex-M0+, no FPU
      SAM4N,            // Cortex-M4, no FPU
      SAM4S,            // Cortex-M4, no FPU
      SAM4L,            // Cortex-M4, no FPU
      SAM4E,            // Cortex-M4F, FPv4 D16 single-precision FPU
      SAMG,             // Cortex-M4F, FPv4 D16 single-precision FPU
      SAMD5,            // Cortex-M4F, FPv4 D16 single-precision FPU
      SAME5,            // Cortex-M4F, FPv4 D16 single-precision FPU
      SAMS7,            // Cortex-M7, FPv5 D16 FPU
      SAME7,            // Cortex-M7, FPv5 D16 FPU
      SAMV7,            // Cortex-M7, FPv5 D16 FPU
      SAMA5D2,          // Cortex-A5, NEON-FPv4
      SAMA5D3,          // Cortex-A5, FPv4 D16 FPU
      SAMA5D4,          // Cortex-A5, NEON-FPv4
      MEC14,            // MIPS32r2, no FPU/DSP, microMIPS
      MEC17,            // Cortex-M4F, FPv4 D16 single-precision FPU
    };

    /* TODO:  I might be able to replace this "pic32C.selected" property with the "target.arch.isMIPS32"
     *        and "target.arch.isARM" properties.
     */
    public static final String PIC32C_SELECTED_PROPERTY = "pic32C.selected";
    final Boolean pic32CSelected;
    
    protected ClangAbstractMipsRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        
        setTargetDefaultProperties(conf);
        
        pic32CSelected = ClangLanguageToolchain.isPIC32C(getPic());
        setProperty(PIC32C_SELECTED_PROPERTY, pic32CSelected.toString());

        // This must be called after setTargetDefaultProperties().
        setArchSpecificBehavior(desc, conf);
    }

    /* Get the current value of the given option using the MakeConfiguration supplied to this class.
     * The optionBookId is the name given to the mp:configurationObject in the Clang.languageToolchain.xml
     * file, such as "C32Global", "C32", "C32CPP", etc.  The optionId is the name of the option itself.
     * This will return the given default value if the option could not be read for some reason.
     *
     * This just calls the implementation in CommonProperties.java and is here for convenience.
     */
    public static String getProjectOption(MakeConfigurationBook confBook, MakeConfiguration conf, 
                                             String optionBookId, String optionId, String defaultVal) {
        return CommonProperties.getProjectOption(confBook, conf, optionBookId, optionId, defaultVal);
    }
    
    /* Get the device family based on the device's name.
     */
    private TargetFamily getTargetFamily(String targetName) {
        TargetFamily family = TargetFamily.Unknown;
        
        if(targetName.startsWith("ATSAM")) {
            targetName = targetName.substring(2);
        }

        if(targetName.startsWith("PIC32")) {
            targetName = targetName.substring(5);

            if(targetName.startsWith("MZ")) {
                if(targetName.contains("EF"))
                    family = TargetFamily.PIC32MZEF;
                else
                    family = TargetFamily.PIC32MZ;
            }
            else if(targetName.startsWith("MX"))
                family = TargetFamily.PIC32MX;
            else if(targetName.startsWith("MK"))
                family = TargetFamily.PIC32MK;
            else if(targetName.startsWith("WK"))
                family = TargetFamily.PIC32WK;
            else if(targetName.startsWith("MM"))
                family = TargetFamily.PIC32MM;
            else if(targetName.startsWith("CX"))
                family = TargetFamily.PIC32CX;
            else if(targetName.startsWith("CZ"))
                family = TargetFamily.PIC32CZ;
        }
        else if(targetName.startsWith("SAM")) {
            targetName = targetName.substring(3);
            
            if(targetName.startsWith("A5D4"))
                family = TargetFamily.SAMA5D4;
            else if(targetName.startsWith("A5D3"))
                family = TargetFamily.SAMA5D3;
            else if(targetName.startsWith("A5D2"))
                family = TargetFamily.SAMA5D2;
            else if(targetName.startsWith("V7"))
                family = TargetFamily.SAMV7;
            else if(targetName.startsWith("E7"))
                family = TargetFamily.SAME7;
            else if(targetName.startsWith("S7"))
                family = TargetFamily.SAMS7;
            else if(targetName.startsWith("E5"))
                family = TargetFamily.SAME5;
            else if(targetName.startsWith("D5"))
                family = TargetFamily.SAMD5;
            else if(targetName.startsWith("G"))
                family = TargetFamily.SAMG;
            else if(targetName.startsWith("4E"))
                family = TargetFamily.SAM4E;
            else if(targetName.startsWith("4L"))
                family = TargetFamily.SAM4L;
            else if(targetName.startsWith("4S"))
                family = TargetFamily.SAM4S;
            else if(targetName.startsWith("4N"))
                family = TargetFamily.SAM4N;
            else if(targetName.startsWith("C"))
                family = TargetFamily.SAMC;
            else if(targetName.startsWith("L2"))
                family = TargetFamily.SAML2;
            else if(targetName.startsWith("L"))
                family = TargetFamily.SAML;
            else if(targetName.startsWith("D"))
                family = TargetFamily.SAMD;
        }
        else if(targetName.startsWith("MEC")) {
            targetName = targetName.substring(3);

            if(targetName.startsWith("17"))
                family = TargetFamily.MEC17;
            else if(targetName.startsWith("14"))
                family = TargetFamily.MEC14;
        }

        return family;
    }
    
    /* Set default properties depending on the family of the selected device.  These are used for
     * the default option values ("Auto-detect") in the "Target Specific" section of the General
     * Options.
     */
    private void setTargetDefaultProperties(MakeConfiguration conf) {
        String targetName = conf.getDevice().getValue().toUpperCase();
        TargetFamily family = getTargetFamily(targetName);

        String defaultArch = "mipsel-unknown-elf";
        String mipsDefaultIsa = "mips32+mips16e";
        String mipsDefaultCpu = "mips32r2";
        String mipsDefaultFpu = "-msoft-float -mfloat-abi=soft";
        String mipsDefaultDsp = "";
        String armDefaultCpu = "cortex-m0plus";
        String armDefaultIsa = "thumb";
        String armDefaultFpu = "-msoft-float -mfloat-abi=soft";
        boolean isARM = false;
        boolean isMIPS32 = true;

        switch(family) {
            // MIPS32r2, no FPU/DSP, MIPS16e
            case Unknown:
            case PIC32MX:
                break;
            
            // MIPS32r2, DSP, microMIPS, no FPU
            case PIC32MZ:
                mipsDefaultIsa = "mips32+micromips";
                mipsDefaultDsp = "-mdspr2";
                break;

            // MIPS32r5, DSP, microMIPS, FPU
            case PIC32MZEF:
            case PIC32MK:
            case PIC32WK:
                mipsDefaultIsa = "mips32+micromips";
                mipsDefaultCpu = "mips32r5";
                mipsDefaultFpu = "-mhard-float -mfloat-abi=hard";
                mipsDefaultDsp = "-mdspr2";
                break;

            // MIPS32r2, microMIPS only, no FPU/DSP
            case PIC32MM:
                mipsDefaultIsa = "micromips";
                break;

            // Cortex-M0, no FPU
            case SAMD:
            case SAML2:
            case SAMC:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m0plus";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-M23, no FPU
            case SAML:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m23";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-M4, no FPU
            case SAM4N:
            case SAM4S:
            case SAM4L:
            case PIC32CX:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m4";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-M4F, FPv4 D16 single-precision FPU
            case SAM4E:
            case SAMG:
            case SAMD5:
            case SAME5:
            case MEC17:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m4";
                armDefaultFpu = "-mfpu=vfp4-sp-d16 -mfloat-abi=hard";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-M7, FPv5 D16 FPU
            case SAMS7:
            case SAME7:
            case SAMV7:
            case PIC32CZ:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m7";
                armDefaultFpu = "-mfpu=vfp5-dp-d16 -mfloat-abi=hard";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-A5, NEON-FPv4
            case SAMA5D2:
            case SAMA5D4:
                defaultArch = "arm-none-eabi";
                armDefaultIsa = "arm+thumb";
                armDefaultCpu = "cortex-a5";
                armDefaultFpu = "-mfpu=neon-vfpv4 -mfloat-abi=hard";
                isARM = true;
                isMIPS32 = false;
                break;

            // Cortex-A5, FPv4 D16 FPU
            case SAMA5D3:
                defaultArch = "arm-none-eabi";
                armDefaultIsa = "arm+thumb";
                armDefaultCpu = "cortex-a5";
                armDefaultFpu = "-mfpu=vfp4-dp-d16 -mfloat-abi=hard";
                isARM = true;
                isMIPS32 = false;
                break;

            // MIPS32r2, no FPU/DSP, microMIPS
            case MEC14:
                break;
        }

        setProperty("target.arch.default", defaultArch);
        setProperty("target.mips32.isa.default", mipsDefaultIsa);
        setProperty("target.mips32.cpu.default", mipsDefaultCpu);
        setProperty("target.mips32.fpu.default", mipsDefaultFpu);
        setProperty("target.mips32.dsp.default", mipsDefaultDsp);
        setProperty("target.arm.isa.default", mipsDefaultIsa);
        setProperty("target.arm.cpu.default", armDefaultCpu);
        setProperty("target.arm.fpu.default", armDefaultFpu);
        setProperty("target.arch.isARM.default", Boolean.toString(isARM));
        setProperty("target.arch.isMIPS32.default", Boolean.toString(isMIPS32));
    }

    /* Set options and a few properties depending on the architecture selected in the "Target Specific"
     * section of the General Options page.  Mainly this figures out if the selected arch is MIPS32
     * or ARM and then sets up MIPS16e and microMIPS availability (or ARM/Thumb for ARM devices).
     */
    private void setArchSpecificBehavior(MakeConfigurationBook desc, MakeConfiguration conf) {
        boolean isMips32 = false;
        boolean isArm = false;
        boolean grayMips16 = false;
        boolean grayMicromips = false;
        boolean grayThumb = false;
        boolean setMips16 = false;
        boolean setMicromips = false;
        boolean setThumb = false;
        boolean valueMips16 = false;
        boolean valueMicromips = false;
        boolean valueThumb = false;
        
        String arch = getProjectOption(desc, conf, "C32Global", "target.arch", "");

        if(!arch.isEmpty()) {
            if(arch.equals("mipsel-unknown-elf")) {
                isMips32 = true;
                isArm = false;

                String mipsIsa = getProjectOption(desc, conf, "C32Global", "target.mips32.isa",
                                                  "mips32+mips16e");

                if(mipsIsa.equals("mips32+mips16e")) {
                    grayMicromips = true;
                    setMicromips = true;
                    valueMicromips = false;
                }
                else if(mipsIsa.equals("mips32+micromips")) {
                    grayMips16 = true;
                    setMips16 = true;
                    valueMips16 = false;
                }
                else if(mipsIsa.equals("micromips")) {
                    // Here microMIPS must be forced on because only it is supported.
                    grayMips16 = true;
                    grayMicromips = true;
                    setMips16 = true;
                    setMicromips = true;
                    valueMips16 = false;
                    valueMicromips = true;
                }

                // No need to gray Thumb options because they'll be suppressed by ${target.arch.isMIPS}.
            }
            else if(arch.equals("arm-none-eabi")) {
                isMips32 = false;
                isArm = true;

                String armIsa = getProjectOption(desc, conf, "C32Global", "target.arm.isa", "thumb");

                if(armIsa.equals("thumb")) {
                    grayThumb = true;
                    setThumb = true;
                    valueThumb = true;
                }

                // No need to gray MIPS options because they'll be suppressed by ${target.arch.isARM}.
            }
        }

        // Notice that these are not the default arch settings like in setTargetDefaultProperties().
        // This is based on the actual value of the option (see above) whereas the default is based
        // on the name of the device.
        setProperty("target.arch.isARM", Boolean.toString(isArm));
        setProperty("target.arch.isMIPS32", Boolean.toString(isMips32));

        setProperty("mips16.gray", Boolean.toString(grayMips16));
        setProperty("micromips.gray", Boolean.toString(grayMicromips));
        setProperty("thumb.gray", Boolean.toString(grayThumb));

        try {
            Project project = desc.getProject();
            
            if(setMips16) {
                conf.setGenericOption(project, "C32-AS", "generate-16-bit-code", Boolean.toString(valueMips16));
                conf.setGenericOption(project, "C32", "generate-16-bit-code", Boolean.toString(valueMips16));
                conf.setGenericOption(project, "C32CPP", "generate-16-bit-code", Boolean.toString(valueMips16));
                conf.setGenericOption(project, "C32-LD", "generate-16-bit-code", Boolean.toString(valueMips16));
            }

            if(setMicromips) {
                conf.setGenericOption(project, "C32-AS", "generate-micro-compressed-code", Boolean.toString(valueMicromips));
                conf.setGenericOption(project, "C32", "generate-micro-compressed-code", Boolean.toString(valueMicromips));
                conf.setGenericOption(project, "C32CPP", "generate-micro-compressed-code", Boolean.toString(valueMicromips));
                conf.setGenericOption(project, "C32-LD", "generate-micro-compressed-code", Boolean.toString(valueMicromips));            
            }

            if(setThumb) {
                conf.setGenericOption(project, "C32-AS", "generate-thumb-code", Boolean.toString(valueThumb));
                conf.setGenericOption(project, "C32", "generate-thumb-code", Boolean.toString(valueThumb));
                conf.setGenericOption(project, "C32CPP", "generate-thumb-code", Boolean.toString(valueThumb));
                conf.setGenericOption(project, "C32-LD", "generate-thumb-code", Boolean.toString(valueThumb));            
            }
        }
        catch(Exception e)
        {
            // do nothing for now
        }
    }
}
