/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.mdbcore.common.streamingdata.StreamingDataEnums;
import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.TraceSetupInformationInterface;
import com.microchip.mplab.mdbcore.streamingdataprocessing.TraceOptionsConstants;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;
import java.util.logging.Level;

/**
 *
 * @author jose Modified: 2011.03.29 PRJ Add instrumented trace support.
 * Modified by Jesse DeGuire for toolchainPic32Clang.
 */
public final class CompilerMaketimeProperties extends CommonMaketimeProperties {

    public CompilerMaketimeProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) 
        throws com.microchip.crownking.Anomaly, 
        org.xml.sax.SAXException,
        java.io.IOException, 
        javax.xml.parsers.ParserConfigurationException, 
        IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);

        addDebuggerNameOptions();
        commandLineProperties.setProperty("INSTRUMENTED_TRACE_OPTIONS", getTraceOptions());
        commandLineProperties.setProperty("FUNCTION_LEVEL_PROFILING_OPTIONS", getFunctionLevelProfilingOptions()); // waiting on compiler 2013.06.04
        commandLineProperties.setProperty("project_cpp", shouldBuildWithCPP(ClangLanguageToolchain.CPP_SUPPORT_FIRST_VERSION).toString());
        commandLineProperties.setProperty("c_includes_in_cpp", shouldUseCIncludesInCPP());
    }

    // TODO:  We might not be able to use these instrumented trace options, so they might need removing.
    // TODO:  Maybe in the future we can somehow use Clang's built-in instrumented trace.
    final String getTraceMediumMacro(boolean isDisabled, int traceMedium, int tracePort, int traceSPI) {
        if (isDisabled) {
            return "off";
        } else if (traceMedium == TraceOptionsConstants.SDC_MEDIUM_NATIVE) {
            return "dc";
        } else if (traceMedium == TraceOptionsConstants.SDC_MEDIUM_PORT) {
            switch (tracePort) {
                case TraceOptionsConstants.SDC_PORTSELECT_PORTA:
                    return "porta";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTBHI:
                    return "portbhi";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTC:
                    return "portc";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTD:
                    return "portd";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTDHI:
                    return "portdhi";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTE:
                    return "porte";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTF:
                    return "portf";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTG:
                    return "portg";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTGHI:
                    return "portghi";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTH:
                    return "porth";
                case TraceOptionsConstants.SDC_PORTSELECT_PORTJ:
                    return "portj";
                default:
                    break;
            }
        } else if (traceMedium == TraceOptionsConstants.SDC_MEDIUM_SPI) {
            if (traceSPI == TraceOptionsConstants.SDC_SPISELECT_SPI1) {
                return "spi1";
            } else if (traceSPI == TraceOptionsConstants.SDC_SPISELECT_SPI2) {
                return "spi2";
            }
        }

        return "";
    }

    // TODO:  We might not be able to use these instrumented trace options, so they might need removing.
    final String getTraceOptions() {
        if (assembly == null) {
            // log error?
        } else {
            TraceSetupInformationInterface tsi = assembly.getLookup().lookup(TraceSetupInformationInterface.class);
            if (tsi == null) {
                // log error?
            } else {
                if (tsi.isTraceEnabled() && (tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {  // StreamingDataController.SDC_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {
                    return String.format(
                            "-include InstrumentedTrace.h -mit=%s ",
                            getTraceMediumMacro(tsi.areTraceMacrosDisabled(), tsi.getTraceMediumSelection().getValue(), tsi.getTracePortSelection().getValue(), tsi.getTraceSPISelection().getValue()));
                }
            }
        }

        return "";
    }

    // TODO:  We might not be able to use these instrumented trace options, so they might need removing.
    final String getFunctionLevelProfilingOptions() {
        if (assembly == null) {
            MPLABLogger.mplog.log(Level.SEVERE, "CompilerProperties::getFunctionLevelProfilingOptions, could not get the Assembly.");
        } else {
            TraceSetupInformationInterface tsi = assembly.getLookup().lookup(TraceSetupInformationInterface.class);
            if (tsi == null) {
                MPLABLogger.mplog.log(Level.SEVERE, "CompilerProperties::getFunctionLevelProfilingOptions, could not get the TraceSetupInformationInterface.");
            } else {
                // PRJ 2014.11.07 Added check for iFlowtrace resource.
                if (tsi.isTraceEnabled() && (tsi.getTraceResource() != StreamingDataEnums.eSDResource.SD_RESOURCE_IFLOWTRACE)
                        && ((tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_FUNCTION_PROFILE)
                        || ((tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_POWER_MODULE)
                        && (tsi.getTraceStream() == StreamingDataEnums.eSDStream.SD_STREAM_FUNCTION_PROFILE)))) {
                    return getFunctionLevelProfilingOptions(tsi, desc.getBaseDir());
                }
            }
        }

        return "".substring(0);
    }

    // TODO:  We might not be able to use these instrumented trace options, so they might need removing.
    public String getFunctionLevelProfilingOptions(final TraceSetupInformationInterface tsi, final String projBaseDir) {
        return "-mit=profile".substring(0);
    }

    private String shouldUseCIncludesInCPP() {
        return optAccessor.getProjectOption("C32CPP", "c-includes-in-cpp", "false");
    }
}
