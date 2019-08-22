/* @author jdeguire
 * Created: 9 Sep 2018
 *
 * This class repesents a target device supported by the toolchain and allows one to query the
 * device for features such as a floating-point unit or DSP extensions.  This uses the device
 * database built into MPLAB X (accessed via the xPIC object) to get information.  Much of the
 * xPIC-related stuff was adapted from example code provided by George Pauley, the MPLAB X 
 * Simulator team lead at Microchip.
 */

package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions.Family;
import com.microchip.crownking.mplabinfo.FamilyDefinitions.SubFamily;
import com.microchip.mplab.crownkingx.xPICFactory;
import com.microchip.mplab.crownkingx.xPIC;
import java.util.ArrayList;


/**
 */
public class TargetDevice {

	public enum TargetArch {
		MIPS32R2,
		MIPS32R5,
		ARMV6M,
		ARMV7A,
		ARMV7M,
		ARMV7EM,
		ARMV8A,
		ARMV8M_BASE,
		ARMV8M_MAIN
	};

    final private String name_;
    final private Family family_;
    final private SubFamily subfamily_;
    final private boolean hasfpu_;
    final private boolean hasMips16_;
    final private boolean hasMicroMips_;    
    final private String cpuName_;
	private ArrayList<String> instructionSets_;

    /* Create a new TargetDevice based on the given name.  Throws an exception if the given name is
     * not recognized by this class.  Note that this class parses the name just enough to determine
     * the device's family, so a lack of an exception does not necessarily mean that the device is 
     * fully supported.
     */
    TargetDevice(String devname) throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        name_ = devname.toUpperCase();

        xPIC pic = (xPIC)xPICFactory.getInstance().get(name_);

        family_ = pic.getFamily();

		if(Family.PIC32 == family_  ||  Family.ARM32BIT == family_) {
            subfamily_ = pic.getSubFamily();
            hasfpu_ = pic.hasFPU();
            hasMips16_ = pic.has16Mips();
            hasMicroMips_ = pic.hasMicroMips();
            cpuName_ = pic.getArchitecture();
 
   			instructionSets_ = new ArrayList<>(pic.getInstructionSet().getSubsetIDs());

			String setId = pic.getInstructionSet().getID();
			if(setId != null  &&  !setId.isEmpty())
				instructionSets_.add(setId);
        }
		else {
            xPICFactory.getInstance().release(pic);
            String what = "Device " + devname + " is not a recognized MIPS32 or ARM device.";
            throw new IllegalArgumentException(what);
        }

   	    xPICFactory.getInstance().release(pic);
    }

    /* Get the name of the device provided to the constructor of this class, but in all uppercase.
     */
    public String getDeviceName() {
        return name_;
    }

    /* Get the device family of the target, which is used to determine its features.
     */
	public Family getFamily() {
		return family_;
	}

	/* Get the subfamily of the target, which is a bit more fine-grained than the family.
	 */
	public SubFamily getSubFamily() {
		return subfamily_;
	}

	/* Get the CPU architecture for the device.
	 */
	public TargetArch getArch() {
		TargetArch arch;

		if(isMips32()) {
			// The device database does not seem to distinguish between the two, but looking at the
			// datasheets indicates that devices with an FPU are MIPS32r5.
			if(hasFpu())
				arch = TargetArch.MIPS32R5;
			else
				arch = TargetArch.MIPS32R2;
		}
		else   // ARM32
		{
			arch = TargetArch.ARMV6M;   // lowest common denominator

			boolean found = false;
			for(int i = 0; !found  &&  i < instructionSets_.size(); ++i) {
				switch(instructionSets_.get(i).toLowerCase()) {
					case "armv6m":                               // Cortex M0, M0+, M1
						arch = TargetArch.ARMV6M;
						found = true;
						break;
					case "armv7a":                               // Cortex A5-A9, A1x
						arch = TargetArch.ARMV7A;
						found = true;
						break;
					case "armv7m":                               // Cortex M3
					case "armv7em":                              // Cortex M4, M7
                        // NOTE:  Microchip's EDC files do not actually distinguish between ARMv7-M
                        //        and ARMV7-EM, so we'll do it here.
                        if(getCpuName().equals("cortex-m3"))
    						arch = TargetArch.ARMV7M;
                        else
                            arch = TargetArch.ARMV7EM;

                        found = true;
						break;
					case "armv8a":                               // Cortex A3x, A5x, A7x
						arch = TargetArch.ARMV8A;
						found = true;
						break;
                    case "armv8m":
                    case "armv8m.base":                          // Cortex M23
						arch = TargetArch.ARMV8M_BASE;
						found = true;
						break;
					case "armv8m.main":                          // Cortex M33, M35P
						arch = TargetArch.ARMV8M_MAIN;
						found = true;
						break;
					default:
						found = false;
						break;
				}
			}
		}

		return arch;
	}

	/* Get the name of the architecture as a string suitable for passing to Clang's "-march="
	 * option.  This will probably also work for GCC, though it has not been tried.
	 */
	public String getArchNameForCompiler() {
		return getArch().name().toLowerCase().replace('_', '.');
	}

    /* Get the target triple name used by the toolchain to determine the overall architecture
     * in use.  This is used with the "-target" compiler option.
     */
    public String getTargetTripleName() {
		if(isMips32())
			return "mipsel-unknown-elf";
		else
			return "arm-none-eabi";
    }

    /* Get the CPU name to be used with Clang's "-mtune=" option, such as "cortex-m7" or "mips32r2".
     */
    public String getCpuName() {
        if(isMips32())
            return getArchNameForCompiler();
        else
            return cpuName_.toLowerCase();
    }

    /* Return True if this is a MIPS32 device.
     */
    public boolean isMips32() {
        return getFamily() == Family.PIC32;
    }

    /* Return True if this is an ARM device.
     */
    public boolean isArm() {
        return getFamily() == Family.ARM32BIT;
    }

    /* Return True if the target has an FPU.
     */
    public boolean hasFpu() {
        return hasfpu_;
    }

    /* Return True if the target supports the MIPS32 instruction set.
     */
    public boolean supportsMips32Isa() {
        return (isMips32()  &&  getSubFamily() != SubFamily.PIC32MM);
    }

    /* Return True if the target supports the MIPS16e instruction set.
     */
    public boolean supportsMips16Isa() {
        return (isMips32()  &&  hasMips16_);
    }

    /* Return True if the target supports the microMIPS instruction set.
     */
    public boolean supportsMicroMipsIsa() {
        return (isMips32()  &&  hasMicroMips_);
    }

    /* Return True if the target supports the MIPS DSPr2 application specific extension.
     */
    public boolean supportsDspR2Ase() {
		boolean hasDsp = false;

		if(isMips32()) {
			for(String id : instructionSets_) {
				if(id.equalsIgnoreCase("dspr2")) {
					hasDsp = true;
					break;
				}
			}
		}

		return hasDsp;
    }

    /* Return True if the target supports the ARM instruction set.
     */
    public boolean supportsArmIsa() {
		TargetArch arch = getArch();

        return (TargetArch.ARMV7A == arch  ||  TargetArch.ARMV8A == arch);
    }

    /* Return True if the target supports the Thumb instruction set.
     */
    public boolean supportsThumbIsa() {
        return isArm();
    }

    /* Get the name of the FPU for ARM devices.  ARM devices have different FPU variants that can be
     * supported that determine whether it is single-precision only or also supports double-precision
     * as well as how many FPU registers are available and whether NEON SIMD extensions are supported.
     *
     * MIPS devices have only only FPU, so this will return an empty string for MIPS.
     */
    public String getArmFpuName() {
		String fpuName = "";

		if(isArm()  &&  hasFpu()) {
			switch (getArch()) {
                case ARMV7M:
                case ARMV7EM:
                    if(getCpuName().equals("cortex-m7"))
                        fpuName = "vfp5-dp-d16";
                    else
                        fpuName = "vfp4-sp-d16";
                    break;
                case ARMV7A:
                    // There does not yet seem to be a way to check for NEON other than name.
                    String name = getDeviceName();
                    if(name.startsWith("SAMA5D3")  ||  name.startsWith("ATSAMA5D3"))
                        fpuName = "vfp4-dp-d16";
                    else
                        fpuName = "neon-vfpv4";
                    break;
                default:
                    fpuName = "vfp4-dp-d16";
                    break;
            }
		}

        return fpuName;
    }
}
