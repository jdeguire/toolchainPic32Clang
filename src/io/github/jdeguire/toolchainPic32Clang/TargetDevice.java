/* @author Jese DeGuire
 * Created: 9 Sep 2018
 *
 * This class repesents a target device supported by the toolchain and allows one to query the
 * device for features such as a floating-point unit or DSP extensions.  This uses the device
 * database built into MPLAB X (accessed via the xPIC object) to get information.  Much of the
 * xPIC-related stuff was adapted from example code provided by George Pauley, the MPLAB X 
 * Simulator team lead at Microchip.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions.Family;
import com.microchip.crownking.mplabinfo.FamilyDefinitions.SubFamily;
import com.microchip.mplab.crownkingx.xPICFactory;
import com.microchip.mplab.crownkingx.xPIC;
import java.util.ArrayList;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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

    final private xPIC pic_;
    final private String name_;
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

        pic_ = (xPIC) xPICFactory.getInstance().get(devname);
        name_ = normalizeDeviceName(devname);

        if(Family.PIC32 == pic_.getFamily() || Family.ARM32BIT == pic_.getFamily()) {
            instructionSets_ = new ArrayList<>(pic_.getInstructionSet().getSubsetIDs());

            String setId = pic_.getInstructionSet().getID();
            if(setId != null && !setId.isEmpty()) {
                instructionSets_.add(setId);
            }
        } else {
            String what = "Device " + devname + " is not a recognized MIPS32 or Arm device.";
            throw new IllegalArgumentException(what);
        }
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            xPICFactory.getInstance().release(pic_);
        } finally {
            super.finalize();
        }
    }

    /* Get the xPIC object used by this class to access the MPLAB X device database.
     */
    public xPIC getPic() {
        return pic_;
    }

    /* Get the name of the device this class represents.  It will be in uppercase and normalized
     * such that PIC32 devices will start with "PIC32" and SAM devices will start with "ATSAM".
     */
    public String getDeviceName() {
        return name_;
    }

    /* Return the name of the device modified such that it can be used for C macros.  How the name 
     * is modified depends on the device, but an example is that "PIC32" devices will have the 
     * "PIC" portion removed and "ATSAM" devices will have the "AT" removed.
     */
    public String getDeviceNameForMacro() {
        String name = getDeviceName();

        if(name.startsWith("PIC32")) {
            return name.substring(3);                 // "32MX795F512L"
        } else if(name.startsWith("ATSAM")) {
            return name.substring(2);                 // "SAME70Q21"
        } else {
            return name;
        }
    }

    /* Return a string that can be used as a device series, such as "PIC32MX", "SAMD", and so on.
     * For MIPS devices, this uses the device name to determine the series and will handle non-PIC32
     * devices as well.  For Arm devices, this is equivalent to calling getAtdfFamily().
     */
    public String getDeviceSeriesName() {
        if(isMips32()) {
            String devname = getDeviceName();

            if(devname.startsWith("M")) {
                return devname.substring(0, 3);
            } else if(devname.startsWith("USB")) {
                return devname.substring(0, 5);
            } else {
                return devname.substring(0, 7);
            }
        } else {
            return getAtdfFamily();
        }
    }

    /* Get the device family of the target, which is used to determine its features.
     */
    public Family getFamily() {
        return pic_.getFamily();
    }

    /* Get the subfamily of the target, which is a bit more fine-grained than the family.
     */
    public SubFamily getSubFamily() {
        return pic_.getSubFamily();
    }

    /* Return the name of the ATDF device family for this device, such as "SAME" or "PIC32CX", that 
     * applies to Arm devices.  This will return an empty string if a family is not provided.  MIPS
     * device will generally not have an ATDF family, at least not at the time of this writing 
     * (2 Feb 2020).
     */
    public String getAtdfFamily() {
        String atdfFamily = pic_.getATDFFamily();

        if(null == atdfFamily) {
            atdfFamily = "";
        }

        return atdfFamily;
    }

    /* Get the CPU architecture for the device.
     */
    public TargetArch getArch() {
        TargetArch arch;

        if(isMips32()) {
            // The device database does not seem to distinguish between the two, but looking at the
            // datasheets indicates that devices with an FPU are MIPS32r5.
            if(hasFpu()) {
                arch = TargetArch.MIPS32R5;
            } else {
                arch = TargetArch.MIPS32R2;
            }
        } else // ARM32
        {
            arch = TargetArch.ARMV6M;   // lowest common denominator

            boolean found = false;
            for(int i = 0; !found && i < instructionSets_.size(); ++i) {
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
                        //        and ARMV7E-M, so we'll do it here.
                        if(getCpuName().equals("cortex-m3")) {
                            arch = TargetArch.ARMV7M;
                        } else {
                            arch = TargetArch.ARMV7EM;
                        }

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
        if(isMips32()) {
            return "mipsel-linux-gnu";
        } else {
            return "arm-none-eabi";
        }
    }

    /* Get the CPU name to be used with Clang's "-mtune=" option, such as "cortex-m7" or "mips32r2".
     */
    public String getCpuName() {
        if(isMips32()) {
            return getArchNameForCompiler();
        } else {
            return pic_.getArchitecture().toLowerCase();
        }
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
        boolean hasfpu = false;

        if(isMips32()) {
            hasfpu = pic_.hasFPU();
        } else {
            // The .PIC files don't encode this the same way for Arm devices, so we have to dig for
            // it ourselves.

            // We don't need the "edc:" prefix here since we're using the MPLAB X API.
            Node peripheralListNode = pic_.first("PeripheralList");

            if(null != peripheralListNode && peripheralListNode.hasChildNodes()) {
                NodeList children = peripheralListNode.getChildNodes();

                for(int i = 0; i < children.getLength(); ++i) {
                    Node child = children.item(i);

                    // We do need the "edc:" prefix here because these are not a part of the
                    // MPLAB X API.
                    if(!child.getNodeName().equals("edc:Peripheral") || !child.hasAttributes()) {
                        continue;
                    }

                    Node attributeNode = child.getAttributes().getNamedItem("edc:cname");

                    if(null == attributeNode || !attributeNode.getNodeValue().equals("FPU")) {
                        continue;
                    }

                    hasfpu = true;
                }
            }
        }

        return hasfpu;
    }

    /* Return True if the target has a 64-bit FPU.
     */
    public boolean hasFpu64() {
        // So far, only the Cortex-M4 devices have a single-precision FPU.
        return hasFpu() && !getCpuName().equals("cortex-m4");
    }

    /* Return True if the device has an L1 cache.  This is actually just a guess for now based on
     * the device's family or architecture.
     */
    public boolean hasL1Cache() {
        boolean result = false;

        if(getSubFamily() == SubFamily.PIC32MZ) {
            result = true;
        } else if(getCpuName().equals("cortex-m7")) {
            result = true;
        } else if(getCpuName().startsWith("cortex-a")) {
            result = true;
        }

        return result;
    }

    /* Return True if the target supports the MIPS32 instruction set.
     */
    public boolean supportsMips32Isa() {
        return (isMips32() && getSubFamily() != SubFamily.PIC32MM);
    }

    /* Return True if the target supports the MIPS16e instruction set.
     */
    public boolean supportsMips16Isa() {
        return (isMips32() && pic_.has16Mips());
    }

    /* Return True if the target supports the microMIPS instruction set.
     */
    public boolean supportsMicroMipsIsa() {
        return (isMips32() && pic_.hasMicroMips());
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

    /* Return True if the target supports the MIPS MCU application specific extension.
     */
    public boolean supportsMcuAse() {
        // There's no way to tell from the MPLAB X API, but looking at datasheets for different PIC32
        // series suggests that devices that support microMIPS also support the MCU ASE.
        return supportsMicroMipsIsa();
    }

    /* Return True if the target supports the ARM instruction set.
     */
    public boolean supportsArmIsa() {
        TargetArch arch = getArch();

        return (TargetArch.ARMV7A == arch || TargetArch.ARMV8A == arch);
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
     * MIPS devices have only one FPU, so this will return an empty string for MIPS.
     */
    public String getArmFpuName() {
        String fpuName = "";

        if(isArm() && hasFpu()) {
            switch(getArch()) {
                case ARMV7M:
                case ARMV7EM:
                    if(getCpuName().equals("cortex-m7")) {
                        fpuName = "vfp5-dp-d16";
                    } else {
                        fpuName = "vfp4-sp-d16";
                    }
                    break;
                case ARMV7A:
                    // There does not yet seem to be a way to check for NEON other than name.
                    String name = getDeviceName();
                    if(name.startsWith("ATSAMA5D3")) {
                        fpuName = "vfp4-dp-d16";
                    } else {
                        fpuName = "neon-vfpv4";
                    }
                    break;
                case ARMV8A:
                    fpuName = "fp-armv8";
                    break;
                default:
                    fpuName = "vfp4-sp-d16";
                    break;
            }
        }

        return fpuName;
    }

    /* Ensure the device name is in a predictable format for use by users of this class.  We don't 
     * control what device names we get from the MPLAB X Device objects, so do this to ensure that 
     * we stay consistent even if they change.
     */
    private String normalizeDeviceName(String devname) {
        devname = devname.toUpperCase();

        if(devname.startsWith("SAM")) {
            devname = "AT" + devname;
        } else if(devname.startsWith("32")) {
            devname = "PIC" + devname;
        } else if(devname.startsWith("P32")) {
            devname = "PIC" + devname.substring(1);
        }

        return devname;
    }
}
