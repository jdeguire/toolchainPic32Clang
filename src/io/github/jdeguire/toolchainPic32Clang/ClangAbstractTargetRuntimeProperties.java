/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;
import com.microchip.mplab.nbide.toolchainCommon.properties.CommonToolchainPropertiesAccessor;

/**
 * Handle the common behavior related to mips16 and micromips instruction 
 * sets.
 * This functionality is applicable to both GCC and GPP compilers. 
 * 
 * @author Marian Golea <marian.golea@microchip.com>
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */ 
public abstract class ClangAbstractTargetRuntimeProperties extends CommonToolchainPropertiesAccessor {

    final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();
    final protected ProjectOptionAccessor optAccessor;
    final protected TargetDevice target;

    protected ClangAbstractTargetRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) 
        throws com.microchip.crownking.Anomaly, 
        org.xml.sax.SAXException,
        java.io.IOException, 
        javax.xml.parsers.ParserConfigurationException, 
        IllegalArgumentException,
        MakeConfigurationException {

        super(desc, conf);

        optAccessor = new ProjectOptionAccessor(desc.getProject(), conf);
        target = new TargetDevice(conf.getDevice().getValue());

        setArchSpecificBehavior();
    }

    /* Suppress options that are used to set the target's instruction set so that only the options
     * supported by the target device are allowed to be selected.  For example, the PIC32MM series
     * only supports microMIPS, so all of these options would be suppressed (the target config file
     * that comes with the toolchain will already have the -mmicromips option set).  The Cortex-A
     * devices support both Arm and Thumb, so the "Use Thumb?" option would be enabled and the others
     * disabled.
     */
    private void setArchSpecificBehavior() {
        boolean suppressMips16 = false;
        boolean suppressMicromips = false;
        boolean suppressThumb = false;

        if(target.supportsMips32Isa()) {
            suppressThumb = true;

            if(!target.supportsMips16Isa()) {
                suppressMips16 = true;
            }
            if(!target.supportsMicroMipsIsa()) {
                suppressMicromips = true;
            }
        } else if(target.supportsMicroMipsIsa()) {
            // PIC32MM, so microMIPS will be the default and only option.
            suppressThumb = true;
            suppressMips16 = true;
            suppressMicromips = true;
        } else {
            // Else assume Arm and suppress MIPS-specific options.
            suppressMips16 = true;
            suppressMicromips = true;
            
            if(!target.supportsArmIsa()) {
                // Cortex-M, so Thumb will be the default and only option.
                suppressThumb = true;
            }
        }

        setProperty("target.isARM", Boolean.toString(target.isArm()));
        setProperty("target.isMIPS32", Boolean.toString(target.isMips32()));
        setProperty("mips16.suppress", Boolean.toString(suppressMips16));
        setProperty("micromips.suppress", Boolean.toString(suppressMicromips));
        setProperty("thumb.suppress", Boolean.toString(suppressThumb));
    }
}
