/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.mdbcore.assemblies.Assembly;
import com.microchip.mplab.mdbcore.assemblies.AssemblyProvider;
import com.microchip.mplab.mdbcore.common.streamingdata.StreamingDataEnums;
import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.TraceSetupInformationInterface;
import com.microchip.mplab.mdbcore.streamingdataprocessing.TraceOptionsConstants;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ConfigurationBookProvider;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.Item;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.ItemConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;
import java.util.logging.Level;
import org.netbeans.api.project.Project;

/**
 *
 * @author jose Modified: 2011.03.29 PRJ Add instrumented trace support.
 */
public class CompilerProperties extends CommonProperties{

    public CompilerProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
        
        commandLineProperties.put("PROCESSOR_NAME_FOR_COMPILER", getProcessorNameForCompiler(conf.getDevice().getValue()));
        LinkerProperties.addDebuggerNameOptions(conf.getPlatformTool().getValue(), getPic(), commandLineProperties);
        commandLineProperties.put("INSTRUMENTED_TRACE_OPTIONS", getTraceOptions(projectDescriptor, conf));
        commandLineProperties.put("FUNCTION_LEVEL_PROFILING_OPTIONS", getFunctionLevelProfilingOptions(projectDescriptor)); // waiting on compiler 2013.06.04
        commandLineProperties.put("project_cpp", CompilerProperties.buildWithGPP(projectDescriptor, conf));
        
        /* TODO:  Add properties to emulate XC32 using command-line macros (-D option).
        */
    }

    public static boolean buildWithGPP(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        return xcHasCPPSupport(conf) && projectIsCPP(projectDescriptor, conf);
    }

    public static boolean xcHasCPPSupport(MakeConfiguration conf) {
        try {
            String baseDir = conf.getLanguageToolchain().getDir().getValue();
            return ClangLanguageToolchain.xcHasCPPSupport(conf,baseDir);
        } catch (NullPointerException npe) {
            MPLABLogger.mplog.log(Level.SEVERE, "[Clang]CompilerProperties::xcHasCPPSupport.", npe);
            return true;
        }
    }

    /**
     * A project is a C++ project if it has any C++ content (currently, that
     * means containing .cpp sources).
     *
     * @param projectDescriptor
     * @param conf
     * @return true <=> the projects is a C++ project
     */
    public static boolean projectIsCPP(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        if (null == projectDescriptor || null == conf) {
            MPLABLogger.mplog.log(Level.SEVERE, "[Clang]CompilerProperties::projectIsCPP, unexpected null args.");
            return false;
        }
        if (conf.isCompileConfiguration()) {
            final Item[] items = projectDescriptor.getProjectItems();
            if (null == items) {
                MPLABLogger.mplog.log(Level.SEVERE, "[Clang]CompilerProperties::projectIsCPP, cannot reach the project items.");
                return false;
            }
            for (int x = 0; x < items.length; x++) {
                final ItemConfiguration itemConfiguration = items[x].getItemConfiguration(conf);
                if (itemConfiguration == null) {
                    // item my exist but may not be configured yet
                    continue;
                }
                if (itemConfiguration.getExcluded().getValue()) {
                    continue;
                }
                if (!itemConfiguration.isCompilerToolConfiguration()) {
                    continue;
                }
                if (items[x].isCPPSource()) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * See return
     *
     * @param deviceName
     * @return value of processor to be fed to assembler. Override as needed
     */
    public static String getProcessorNameForCompiler(String deviceName) {
        if (deviceName.toUpperCase().startsWith("PIC32"))
            return deviceName.substring(3, deviceName.length());
        else 
            return deviceName;
    }

    public static String getTraceMediumMacro(boolean isDisabled, int traceMedium, int tracePort, int traceSPI) {
        if (isDisabled) {
            return "off";
        } else if (traceMedium == TraceOptionsConstants.SDC_MEDIUM_NATIVE) {
            return "dc";
        } else if (traceMedium == TraceOptionsConstants.SDC_MEDIUM_PORT) {
            if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTA) {
                return "porta";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTBHI) {
                return "portbhi";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTC) {
                return "portc";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTD) {
                return "portd";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTDHI) {
                return "portdhi";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTE) {
                return "porte";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTF) {
                return "portf";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTG) {
                return "portg";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTGHI) {
                return "portghi";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTH) {
                return "porth";
            } else if (tracePort == TraceOptionsConstants.SDC_PORTSELECT_PORTJ) {
                return "portj";
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

    public String getTraceOptions(MakeConfigurationBook confBook, MakeConfiguration conf) {
        if (confBook != null) {
            Project project = confBook.getProject();
            AssemblyProvider provider = project.getLookup().lookup(AssemblyProvider.class);
            if (provider == null) {
                // log error?
            } else {
                Assembly assembly = provider.getAssembly();
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
            }
        }

        return "";
    }
    //=======================================================================

    private String getFunctionLevelProfilingOptions(MakeConfigurationBook confBook)
    {
     if (confBook != null)
      {
       Project project = confBook.getProject();
       AssemblyProvider provider = project.getLookup().lookup(AssemblyProvider.class);
       if (provider == null)
        {
         MPLABLogger.mplog.log(Level.SEVERE, "ClangSelectedProperties::getFunctionLevelProfilingOptions, could not get the AssemblyProvider.");
        }
       else
        {
         Assembly assembly = provider.getAssembly();
         if (assembly == null)
          {
           MPLABLogger.mplog.log(Level.SEVERE, "ClangSelectedProperties::getFunctionLevelProfilingOptions, could not get the Assembly.");
          }
         else
          {
           TraceSetupInformationInterface tsi = assembly.getLookup().lookup(TraceSetupInformationInterface.class);
           if (tsi == null)
            {
             MPLABLogger.mplog.log(Level.SEVERE, "ClangSelectedProperties::getFunctionLevelProfilingOptions, could not get the TraceSetupInformationInterface.");
            }
           else
            {
             // PRJ 2014.11.07 Added check for iFlowtrace resource.
             if (tsi.isTraceEnabled() && (tsi.getTraceResource() != StreamingDataEnums.eSDResource.SD_RESOURCE_IFLOWTRACE) &&
                 ((tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_FUNCTION_PROFILE) ||
                  ((tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_POWER_MODULE) &&
                   (tsi.getTraceStream() == StreamingDataEnums.eSDStream.SD_STREAM_FUNCTION_PROFILE))))
              {
               return getFunctionLevelProfilingOptions(tsi, confBook.getBaseDir());
              }
            }
          }
        }
      }

     return "".substring(0);
    } // getFunctionLevelProfilingOptions

    //-----------------------------------------------------------------------

    public String getFunctionLevelProfilingOptions(final TraceSetupInformationInterface tsi, String projBaseDir)
    {
     return "-mit=profile".substring(0);
    } // getFunctionLevelProfilingOptions
}
