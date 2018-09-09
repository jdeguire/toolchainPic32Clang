/* @author jdeguire
 * Created: 9 Sep 2018
 *
 * This class repesents a target device supported by the toolchain and allows one to query the
 * device for features such as a floating-point unit or DSP extensions.  This uses the name of the 
 * device to determine its features.
 */

package com.github.jdeguire.toolchainPic32Clang;

/**
 */
public class TargetDevice {
    public enum TargetFamily {
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
      MEC14,            // MIPS32r2, microMIPS only, no FPU/DSP
      MEC17,            // Cortex-M4F, FPv4 D16 single-precision FPU
    };

    private String name_;
    final private TargetFamily family_;
    private String cpuName_;
    private String targetTripleName_;
    private boolean isMips32_;
    private boolean isArm_;
    private boolean hasFpu_;
    private boolean supportsMips32Isa_;
    private boolean supportsMips16Isa_;
    private boolean supportsMicroMipsIsa_;
    private boolean supportsDspR2Ase_;
    private boolean supportsArmIsa_;
    private boolean supportsThumbIsa_;
    private String armFpuName_;        // needed only for ARM because MIPS has only one FPU type.

    /* Create a new TargetDevice based on the given name.  Throws an exception if the given name is
     * not recognized by this class.  Note that this class parses the name just enough to determine
     * the device's family, so a lack of an exception does not necessarily mean that the device is 
     * fully supported.
     */
    TargetDevice(String devname) throws IllegalArgumentException {
        name_ = devname.toUpperCase();
        family_ = getTargetFamily();

        populateDeviceInformation();
    }

    /* Get the name of the device provided to the constructor of this class, but in all uppercase.
     */
    public String getDeviceName() {
        return name_;
    }

    /* Get the device family of the target, which is used to determine its features.
     */
    public TargetFamily getFamily() {
        return family_;
    }

    /* Get the name of the CPU used in the target, such as "mips32r5" or "cortex-m4".
     */
    public String getCpuName() {
        return cpuName_;
    }

    /* Get the target triple name used by the toolchain to determine the overall architecture
     * in use.  This is used with the "-target" compiler option.
     */
    public String getTargetTripleName() {
        return targetTripleName_;
    }

    /* Return True if this is a MIPS32 device.
     */
    public boolean isMips32() {
        return isMips32_;
    }

    /* Return True if this is an ARM device.
     */
    public boolean isArm() {
        return isArm_;
    }

    /* Return True if the target has an FPU.
     */
    public boolean hasFpu() {
        return hasFpu_;
    }

    /* Return True if the target supports the MIPS32 instruction set.
     */
    public boolean supportsMips32Isa() {
        return supportsMips32Isa_;
    }

    /* Return True if the target supports the MIPS16e instruction set.
     */
    public boolean supportsMips16Isa() {
        return supportsMips16Isa_;
    }

    /* Return True if the target supports the microMIPS instruction set.
     */
    public boolean supportsMicroMipsIsa() {
        return supportsMicroMipsIsa_;
    }

    /* Return True if the target supports the MIPS DSPr2 application specific extension.
     */
    public boolean supportsDspR2Ase() {
        return supportsDspR2Ase_;
    }

    /* Return True if the target supports the ARM instruction set.
     */
    public boolean supportsArmIsa() {
        return supportsArmIsa_;
    }

    /* Return True if the target supports the Thumb instruction set.
     */
    public boolean supportsThumbIsa() {
        return supportsThumbIsa_;
    }

    /* Get the name of the FPU for ARM devices.  ARM devices have different FPU variants that can be
     * supported that determine whether it is single-precision only or also supports double-precision
     * as well as how many FPU registers are available and whether NEON SIMD extensions are supported.
     *
     * MIPS devices have only only FPU, so this will return an empty string for MIPS.
     */
    public String getArmFpuName() {
        return armFpuName_;
    }


    /* Get the device family based on the device's name.  Throws an exception if the device name is
     * not recognized by this class.
     */
    private TargetFamily getTargetFamily() throws IllegalArgumentException {
        TargetFamily family = TargetFamily.PIC32MX;
        String devname = name_;
        
        if(devname.startsWith("ATSAM")) {
            devname = devname.substring(2);
        }

        if(devname.startsWith("PIC32")) {
            devname = devname.substring(5);

            if(devname.startsWith("MZ")) {
                if(devname.contains("EF"))
                    family = TargetFamily.PIC32MZEF;
                else
                    family = TargetFamily.PIC32MZ;
            }
            else if(devname.startsWith("MX"))
                family = TargetFamily.PIC32MX;
            else if(devname.startsWith("MK"))
                family = TargetFamily.PIC32MK;
            else if(devname.startsWith("WK"))
                family = TargetFamily.PIC32WK;
            else if(devname.startsWith("MM"))
                family = TargetFamily.PIC32MM;
            else if(devname.startsWith("CX"))
                family = TargetFamily.PIC32CX;
            else if(devname.startsWith("CZ"))
                family = TargetFamily.PIC32CZ;
        }
        else if(devname.startsWith("SAM")) {
            devname = devname.substring(3);
            
            if(devname.startsWith("A5D4"))
                family = TargetFamily.SAMA5D4;
            else if(devname.startsWith("A5D3"))
                family = TargetFamily.SAMA5D3;
            else if(devname.startsWith("A5D2"))
                family = TargetFamily.SAMA5D2;
            else if(devname.startsWith("V7"))
                family = TargetFamily.SAMV7;
            else if(devname.startsWith("E7"))
                family = TargetFamily.SAME7;
            else if(devname.startsWith("S7"))
                family = TargetFamily.SAMS7;
            else if(devname.startsWith("E5"))
                family = TargetFamily.SAME5;
            else if(devname.startsWith("D5"))
                family = TargetFamily.SAMD5;
            else if(devname.startsWith("G"))
                family = TargetFamily.SAMG;
            else if(devname.startsWith("4E"))
                family = TargetFamily.SAM4E;
            else if(devname.startsWith("4L"))
                family = TargetFamily.SAM4L;
            else if(devname.startsWith("4S"))
                family = TargetFamily.SAM4S;
            else if(devname.startsWith("4N"))
                family = TargetFamily.SAM4N;
            else if(devname.startsWith("C"))
                family = TargetFamily.SAMC;
            else if(devname.startsWith("L2"))
                family = TargetFamily.SAML2;
            else if(devname.startsWith("L"))
                family = TargetFamily.SAML;
            else if(devname.startsWith("D"))
                family = TargetFamily.SAMD;
        }
        else if(devname.startsWith("MEC")) {
            devname = devname.substring(3);

            if(devname.startsWith("17"))
                family = TargetFamily.MEC17;
            else if(devname.startsWith("14"))
                family = TargetFamily.MEC14;
        }
        else {
            String what = "Device " + devname + " not recognized by this version of the plugin.";
            throw new IllegalArgumentException(what);
        }

        return family;
    }

    /* Get information about the target device based on its family, populating this object's fields
     * based on that.
     */
    private void populateDeviceInformation() {
        switch(family_) {
            // MIPS32r2, no FPU/DSP, MIPS16e
            case PIC32MX:
                cpuName_ = "mips32r2";
                targetTripleName_ = "mipsel-unknown-elf";
                isMips32_ = true;
                isArm_ = false;
                hasFpu_ = false;
                supportsMips32Isa_ = true;
                supportsMips16Isa_ = true;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = false;
                armFpuName_ = "";
                break;
            
            // MIPS32r2, DSP, microMIPS, no FPU
            case PIC32MZ:
                cpuName_ = "mips32r2";
                targetTripleName_ = "mipsel-unknown-elf";
                isMips32_ = true;
                isArm_ = false;
                hasFpu_ = false;
                supportsMips32Isa_ = true;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = true;
                supportsDspR2Ase_ = true;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = false;
                armFpuName_ = "";
                break;

            // MIPS32r5, DSP, microMIPS, FPU
            case PIC32MZEF:
            case PIC32MK:
            case PIC32WK:
                cpuName_ = "mips32r5";
                targetTripleName_ = "mipsel-unknown-elf";
                isMips32_ = true;
                isArm_ = false;
                hasFpu_ = true;
                supportsMips32Isa_ = true;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = true;
                supportsDspR2Ase_ = true;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = false;
                armFpuName_ = "";
                break;

            // MIPS32r2, microMIPS only, no FPU/DSP
            case PIC32MM:
            case MEC14:
                cpuName_ = "mips32r2";
                targetTripleName_ = "mipsel-unknown-elf";
                isMips32_ = true;
                isArm_ = false;
                hasFpu_ = false;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = true;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = false;
                armFpuName_ = "";
                break;

            // Cortex-M0, no FPU
            case SAMD:
            case SAML2:
            case SAMC:
                cpuName_ = "cortex-m0plus";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = false;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = true;
                armFpuName_ = "";
                break;

            // Cortex-M23, no FPU
            case SAML:
                cpuName_ = "cortex-m23";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = false;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = true;
                armFpuName_ = "";
                break;

            // Cortex-M4, no FPU
            case SAM4N:
            case SAM4S:
            case SAM4L:
            case PIC32CX:
                cpuName_ = "cortex-m4";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = false;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = true;
                armFpuName_ = "";
                break;

            // Cortex-M4F, FPv4 D16 single-precision FPU
            case SAM4E:
            case SAMG:
            case SAMD5:
            case SAME5:
            case MEC17:
                cpuName_ = "cortex-m4";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = true;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = true;
                armFpuName_ = "vfp4-sp-d16";
                break;

            // Cortex-M7, FPv5 D16 FPU
            case SAMS7:
            case SAME7:
            case SAMV7:
            case PIC32CZ:
                cpuName_ = "cortex-m7";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = true;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = false;
                supportsThumbIsa_ = true;
                armFpuName_ = "vfp5-dp-d16";
                break;

            // Cortex-A5, NEON-FPv4
            case SAMA5D2:
            case SAMA5D4:
                cpuName_ = "cortex-a5";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = true;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = true;
                supportsThumbIsa_ = true;
                armFpuName_ = "neon-vfpv4";
                break;

            // Cortex-A5, FPv4 D16 FPU
            case SAMA5D3:
                cpuName_ = "cortex-a5";
                targetTripleName_ = "arm-none-eabi";
                isMips32_ = false;
                isArm_ = true;
                hasFpu_ = true;
                supportsMips32Isa_ = false;
                supportsMips16Isa_ = false;
                supportsMicroMipsIsa_ = false;
                supportsDspR2Ase_ = false;
                supportsArmIsa_ = true;
                supportsThumbIsa_ = true;
                armFpuName_ = "vfp4-dp-d16";
                break;
        }
    }
}
