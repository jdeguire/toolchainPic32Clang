/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.mplabinfo.FamilyDefinitions;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.LanguageToolRuntimePropertiesAccessor;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;


public abstract class ClangAbstractMipsRuntimeProperties extends LanguageToolRuntimePropertiesAccessor {
    public static final String PIC32C_SELECTED_PROPERTY = "pic32C.selected";
    final Boolean pic32CSelected;
    
    protected ClangAbstractMipsRuntimeProperties(MakeConfigurationBook desc, MakeConfiguration conf) {
        super(desc, conf);

        pic32CSelected = ClangLanguageToolchain.isPIC32C(getPic());
        setProperty(PIC32C_SELECTED_PROPERTY, pic32CSelected.toString());
        setMipsBehavior();
    }

    private void setMipsBehavior() {
        final xPIC pic = getAssembly().GetDevice();

        boolean isMips16Disabled = !pic.has16Mips();
        boolean isMicroMipsDisabled = !pic.hasMicroMips();
        
        if (FamilyDefinitions.Family.PIC32 == pic.getFamily()
                && FamilyDefinitions.SubFamily.PIC32MM == pic.getSubFamily()) {
            //for PIC32MM subfamily, micromips is supported but the user is not allowed to change the option in any way.
            //Since the compiler will default to its usage, the option itself can remain unselected.
            isMicroMipsDisabled = Boolean.TRUE;
        }
        
        final String mips16Disabled = pic32CSelected ? Boolean.TRUE.toString() : String.valueOf(isMips16Disabled);
        final String microMipsDisabled = pic32CSelected ? Boolean.TRUE.toString() : String.valueOf(isMicroMipsDisabled);

        setProperty("mips16.suppress", mips16Disabled);
        setProperty("micromips.suppress", microMipsDisabled);
    }
}
