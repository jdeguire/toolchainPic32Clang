

/*
 * Microchip Software Notice
 *
 * Subject to your compliance with the terms of this license, Microchip
 * Technology Inc. (“Microchip”) hereby grants you a non-exclusive license,
 * free of charge license to the accompanying software (“Software”) solely for
 * use with Microchip microcontrollers and/or Microchip digital signal
 * controller products.  Subject to the foregoing, you (either directly or
 * through third party contractors working on your behalf) may: (a) use and
 * make copies of the Software; (b) modify and prepare derivative works of the
 * Software provided in source code form, subject to Microchip’s rights in and
 * to the unmodified code delivered to you; (c) distribute the Software and
 * copies thereof and derived code; and (d) demonstrate, sell, offer to sell,
 * and distribute products (incorporating or bundled with your software and
 * third party software) to end user customers and OEM customers that include
 * the Software (including derived code) or otherwise distribute the Software
 * as a component of an software development kit to ODM and OEM customers,
 * provided that, in all cases (described in clauses (a) through (d) herein),
 * the Software and derived code are accompanied by the header file that
 * accompanies the Software.
 *
 * Further, Microchip is not responsible for any and all modifications made to
 * the Software by your or other authorized users.  You and your authorized
 * users will use commercially reasonable efforts to note any such
 * modifications at the end of the applicable header file notice.
 *
 * The Software are owned by Microchip or its licensors, and protected under
 * applicable copyright laws.  All rights reserved.  This Software and any
 * accompanying information are for information purposes only, and do not
 * modify Microchip’s standard warranty for its microcontroller or digital
 * signal controller products.  It is your responsibility to ensure that the
 * Software meet your requirements.
 *
 * EXCEPT AS EXPRESSLY STATED ABOVE BY MICROCHIP, THE SOFTWARE IS PROVIDED “AS
 * IS.” MICROCHIP EXPRESSLY DISCLAIMS ANY WARRANTY OF ANY KIND, WHETHER EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT.  IN
 * NO EVENT WILL MICROCHIP BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR
 * EQUIPMENT, COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES,
 * ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE
 * THEREOF), ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
 *
 * TO THE FULLEST EXTENT ALLOWED BY LAW, MICROCHIP’S LIABILITY FOR USE OF THE
 * SOFTWARE OR DERIVED CODE WILL NOT EXCEED $1,000 USD. MICROCHIP PROVIDES THE
 * SOFTWARE CONDITIONALLY UPON ACCEPTANCE OF THESE TERMS.
 *
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.spi.FilenameSuffixProvider;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectConfiguration;

/**
 *
 * @author drmc
 */
public class XC32OutputFilenameSuffixProvider implements FilenameSuffixProvider {

    public String getFilenameSuffix(Project project, ProjectConfiguration projectConf, boolean isDebug) {
        if (!isDebug) {
            return "hex"; // NOI18N
        }
        return "elf";
    }

}
