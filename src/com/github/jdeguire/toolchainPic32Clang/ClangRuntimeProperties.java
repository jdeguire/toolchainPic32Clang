/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
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

    /* TODO:  Do I want to move this target-detection stuff to the ClangAbstractMipsRuntimeProperties base class?
     *        That would make all of this accessible by all of the options, but I don't know if that actually helps.
     */
    private enum TargetFamily {
      PIC32MX,          // MIPS32r2, no FPU/DSP, MIPS16e
      PIC32MZ,          // MIPS32r2, DSP, microMIPS, no FPU
      PIC32MZEF,        // MIPS32r5, DSP, microMIPS, FPU -- EF is different from other MZ
      PIC32MK,          // MIPS32r5, DSP, microMIPS, FPU
      PIC32WK,          // MIPS32r5, DSP, microMIPS, FPU
      PIC32MM,          // MIPS32r2, microMIPS only, no FPU/DSP
      PIC32CX,          // Cortex-M4, no FPU
      PIC32CZ,          // Cortex-M7, FPv5 D16 FPU
      SAMD,             // Cortex-M0, no FPU
      SAML,             // Cortex-M0, no FPU
      SAMC,             // Cortex-M0, no FPU
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
    
    public ClangRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);
        supressResponseFileOption();
        setLegacyLibcDefaultState(conf);
        setImola2Properties(desc);
        setTargetDefaultProperties(conf);
    }

    private void setLegacyLibcDefaultState(MakeConfiguration conf) {
        Boolean legacyLibcEnabled = !pic32CSelected && LTUtils.toolchainVersionGreaterOrEqualTo("1.41", conf);
        setProperty("legacy-libc.default", legacyLibcEnabled.toString());
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

    private TargetFamily getTargetFamily(String targetName) {
        TargetFamily family = TargetFamily.PIC32MX;
        
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
            else
                family = TargetFamily.PIC32MX;
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
            else if(targetName.startsWith("L"))
                family = TargetFamily.SAML;
            else
                family = TargetFamily.SAMD;
        }
        else if(targetName.startsWith("MEC")) {
            targetName = targetName.substring(3);

            if(targetName.startsWith("17"))
                family = TargetFamily.MEC17;
            else
                family = TargetFamily.MEC14;
        }

        return family;
    }
    
    private void supressResponseFileOption() {
        String value = "true";
        if (Utilities.isWindows()) {
            value = "false";
        }
        setProperty("opt-Clang-linker-response-files.suppress", value);
    }

    private void setTargetDefaultProperties(MakeConfiguration conf) {
        String targetName = conf.getDevice().getValue().toUpperCase();
        TargetFamily family = getTargetFamily(targetName);

        String defaultArch = "mipsel-unknown-elf";
        String mipsDefaultCpu = "mips32r2";
        String mipsDefaultFpu = "-msoft-float -mfloat-abi=soft";
        String mipsDefaultDsp = "";
        String armDefaultCpu = "cortex-m0";
        String armDefaultFpu = "-msoft-float -mfloat-abi=soft";
        String isARM = Boolean.FALSE.toString();
        String isMIPS32 = Boolean.TRUE.toString();

        switch(family) {
            // MIPS32r2, no FPU/DSP, MIPS16e
            case PIC32MX:
                break;
            
            // MIPS32r2, DSP, microMIPS, no FPU
            case PIC32MZ:
                mipsDefaultDsp = "-mdspr2";
                break;

            // MIPS32r5, DSP, microMIPS, FPU
            case PIC32MZEF:
            case PIC32MK:
            case PIC32WK:
                mipsDefaultCpu = "mips32r5";
                mipsDefaultFpu = "-mhard-float -mfloat-abi=hard";
                mipsDefaultDsp = "-mdspr2";
                break;

            // MIPS32r2, microMIPS only, no FPU/DSP
            case PIC32MM:
                break;

            // Cortex-M0, no FPU
            case SAMD:
            case SAML:
            case SAMC:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m0";
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
                break;

            // Cortex-M4, no FPU
            case SAM4N:
            case SAM4S:
            case SAM4L:
            case PIC32CX:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m4";
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
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
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
                break;

            // Cortex-M7, FPv5 D16 FPU
            case SAMS7:
            case SAME7:
            case SAMV7:
            case PIC32CZ:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-m7";
                armDefaultFpu = "-mfpu=vfp5-dp-d16 -mfloat-abi=hard";
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
                break;

            // Cortex-A5, NEON-FPv4
            case SAMA5D2:
            case SAMA5D4:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-a5";
                armDefaultFpu = "-mfpu=neon-vfpv4 -mfloat-abi=hard";
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
                break;

            // Cortex-A5, FPv4 D16 FPU
            case SAMA5D3:
                defaultArch = "arm-none-eabi";
                armDefaultCpu = "cortex-a5";
                armDefaultFpu = "-mfpu=vfp4-dp-d16 -mfloat-abi=hard";
                isARM = Boolean.TRUE.toString();
                isMIPS32 = Boolean.FALSE.toString();
                break;

            // MIPS32r2, no FPU/DSP, microMIPS
            case MEC14:
                break;
        }

        setProperty("target.arch.default", defaultArch);
        setProperty("target.mips32.cpu.default", mipsDefaultCpu);
        setProperty("target.mips32.fpu.default", mipsDefaultFpu);
        setProperty("target.mips32.dsp.default", mipsDefaultDsp);
        setProperty("target.arm.cpu.default", armDefaultCpu);
        setProperty("target.arm.fpu.default", armDefaultFpu);
        setProperty("target.arch.isARM", isARM);
        setProperty("target.arch.isMIPS32", isMIPS32);
    }

    /* TODO:  We will probably need to handle multilib stuff ourselves using LTUtils and 
     *        whatever linker options are set.
     *
     *   - Optimization level (0, 1, 2, 3, fast, s, z)
     *   - Arch (mips32r2, mips32r5, cortex m0/m4/m7/a5)
     *   - FPU (soft, mips, vfp4-sp-d16, vfp4-dp-d16, vfp5-dp-d16, neon-vfpv4)
     *   - fast-math enabled
     *   - mips32/mips16e/micromips
     *   - mips DSP
     *
     * We can use the EmbeddedProjectSupport.getSynthesizedOption(Project project, ProjectConfiguration projectConf, String optionBookID, String optionID, String itemPath)
     * method to get option values.  See ClangGlobalMakeRuntimeProperties.java.  This can be used to
     * determine multilibs and support for MIPS16e/microMIPS.
     */
}
