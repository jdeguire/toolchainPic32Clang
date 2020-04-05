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
 * Modified by jdeguire for toolchainPic32Clang.
 */ 
public abstract class ClangAbstractTargetRuntimeProperties extends CommonToolchainPropertiesAccessor {

    final CommonPropertiesCalculator calc = new CommonPropertiesCalculator();
    final private ProjectOptionAccessor optAccessor;
    final protected TargetDevice target;

    private static boolean settingArchOpts = false;
    
    protected ClangAbstractTargetRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException,
		MakeConfigurationException {

		super(desc, conf);

        optAccessor = new ProjectOptionAccessor(desc, conf);
        target = new TargetDevice(conf.getDevice().getValue());

        // It turns out that setting options might end up trying to construct a new instance of this
        // class, which will set options, which will construct YET another new instance of this class,
        // which will set options, and so on.  Doing this breaks the recursion chain in a cheesy
        // manner because whatever.
        if(!settingArchOpts)
        {
            settingArchOpts = true;
            setArchSpecificBehavior();      // This one may throw because it sets options
            settingArchOpts = false;
        }
    }

    /* Set options and a few properties depending on the architecture selected in the "Target Specific"
     * section of the General Options page.  Mainly this figures out if the selected arch is MIPS32
     * or ARM and then sets up MIPS16e and microMIPS availability (or ARM/Thumb for ARM devices).
     */
// TODO: We might be able to clean this function up since some info comes from the target config files.
    private void setArchSpecificBehavior() throws IllegalArgumentException, MakeConfigurationException {
        boolean grayMips16 = false;
        boolean grayMicromips = false;
        boolean grayThumb = false;
        boolean setMips16 = false;
        boolean setMicromips = false;
        boolean setThumb = false;
        boolean valueMips16 = false;
        boolean valueMicromips = false;
        boolean valueThumb = false;

        if(target.supportsMips32Isa()) {
            if(target.supportsMips16Isa()) {
                grayMicromips = true;
                setMicromips = true;
                valueMicromips = false;
            }
            else if(target.supportsMicroMipsIsa()) {
                grayMips16 = true;
                setMips16 = true;
                valueMips16 = false;
            }
            else {
                grayMips16 = true;
                setMips16 = true;
                valueMips16 = false;
                grayMicromips = true;
                setMicromips = true;
                valueMicromips = false;
            }
        }
        else if(target.supportsMicroMipsIsa()) {
            // Here microMIPS must be forced on because only it is supported.
            grayMips16 = true;
            grayMicromips = true;
            setMips16 = true;
            setMicromips = true;
            valueMips16 = false;
            valueMicromips = true;
        }

        if(!target.supportsArmIsa()  &&  target.supportsThumbIsa()) {
            grayThumb = true;
            setThumb = true;
            valueThumb = true;
        }

        setProperty("target.isARM", Boolean.toString(target.isArm()));
        setProperty("target.isMIPS32", Boolean.toString(target.isMips32()));

        setProperty("mips16.gray", Boolean.toString(grayMips16));
        setProperty("micromips.gray", Boolean.toString(grayMicromips));
        setProperty("thumb.gray", Boolean.toString(grayThumb));

        // TODO:  Micromips-only devices will have the option in the target config, so do not duplicate it here
        // TODO:  Thumb-only devices will use Thumb already based on the "-march=" option, so do not duplicate it here.
        if(setMips16) {
            optAccessor.setBooleanProjectOption("C32-AS", "generate-16-bit-code", valueMips16);
            optAccessor.setBooleanProjectOption("C32", "generate-16-bit-code", valueMips16);
            optAccessor.setBooleanProjectOption("C32CPP", "generate-16-bit-code", valueMips16);
            optAccessor.setBooleanProjectOption("C32-LD", "generate-16-bit-code", valueMips16);
        }

        if(setMicromips) {
            optAccessor.setBooleanProjectOption("C32-AS", "generate-micro-compressed-code", valueMicromips);
            optAccessor.setBooleanProjectOption("C32", "generate-micro-compressed-code", valueMicromips);
            optAccessor.setBooleanProjectOption("C32CPP", "generate-micro-compressed-code", valueMicromips);
            optAccessor.setBooleanProjectOption("C32-LD", "generate-micro-compressed-code", valueMicromips);
        }

        if(setThumb) {
            optAccessor.setBooleanProjectOption("C32-AS", "generate-thumb-code", valueThumb);
            optAccessor.setBooleanProjectOption("C32", "generate-thumb-code", valueThumb);
            optAccessor.setBooleanProjectOption("C32CPP", "generate-thumb-code", valueThumb);
            optAccessor.setBooleanProjectOption("C32-LD", "generate-thumb-code", valueThumb);            
        }
    }
}
