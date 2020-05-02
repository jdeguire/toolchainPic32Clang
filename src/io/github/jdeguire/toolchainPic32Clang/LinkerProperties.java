/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package io.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.mdbcore.common.streamingdata.StreamingDataEnums;
import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.TraceSetupInformationInterface;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import java.util.Properties;
import java.util.logging.Level;

/**
 *
 * @author jose
 * Modified by jdeguire for toolchainPic32Clang.
 */
public final class LinkerProperties extends CommonProperties {

    public LinkerProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) 
		throws com.microchip.crownking.Anomaly, 
		org.xml.sax.SAXException,
		java.io.IOException, 
		javax.xml.parsers.ParserConfigurationException, 
		IllegalArgumentException {

        super(projectDescriptor, conf, commandLineProperties);

        addDebuggerNameOptions();
        commandLineProperties.put("INSTRUMENTED_TRACE_OPTIONS", getTraceOptions());
        commandLineProperties.put("FUNCTION_LEVEL_PROFILING_OPTIONS", getFunctionLevelProfilingOptions()); // waiting on compiler 2013.06.04
        commandLineProperties.put("project_cpp", shouldBuildWithCPP(ClangLanguageToolchain.CPP_SUPPORT_FIRST_VERSION));

        commandLineProperties.put("show_mem_usage", shouldShowMemUsage());
        commandLineProperties.put("thinlto_threads_opt", getThinLtoThreadsOpt());
        commandLineProperties.put("multilib_dir_opt", getMultilibDirectoryOpt());
    }

    // TODO:  Can we use this or do we remove it?
    final String getTraceOptions() {
        if (assembly == null) {
            // log error?
        } else {
            TraceSetupInformationInterface tsi = assembly.getLookup().lookup(TraceSetupInformationInterface.class);
            if (tsi == null) {
                // log error?
            } else {
                if (tsi.isTraceEnabled() && (tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {  //StreamingDataController.SDC_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {
                    return " -lsol";
                }
            }
        }

        return "";
    }

    // TODO:  Can we use this or do we remove it?
    final String getFunctionLevelProfilingOptions() {
        if (assembly == null) {
            MPLABLogger.mplog.log(Level.SEVERE, "ClangLinkerProperties::getFunctionLevelProfilingOptions, could not get the Assembly.");
        } else {
            TraceSetupInformationInterface tsi = assembly.getLookup().lookup(TraceSetupInformationInterface.class);
            if (tsi == null) {
                MPLABLogger.mplog.log(Level.SEVERE, "ClangLinkerProperties::getFunctionLevelProfilingOptions, could not get the TraceSetupInformationInterface.");
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

    // TODO:  Can we use this or do we remove it?
    public String getFunctionLevelProfilingOptions(final TraceSetupInformationInterface tsi, final String projBaseDir) {
        return "-lcppcfl ".substring(0);
    }

    private Boolean shouldShowMemUsage() {
        if(optAccessor.getBooleanProjectOption("C32-LD", "report-memory-usage", false)) {
            return Boolean.TRUE;
        } else {
            return Boolean.FALSE;
        }
    }

    /* Get the option value for the number of threads to use for Clang's ThinLTO mode and build the
     * string to be passed to the linker.  An option value of 0 or less will tell this to use half
     * of the available logical processors on the machine.
     */
    private String getThinLtoThreadsOpt() {
        String opt = "";

        if(optAccessor.getBooleanProjectOption("C32Global", "wpo-lto", false)  &&
           optAccessor.getBooleanProjectOption("C32Global", "lto.enable-thin", false))
        {
            int maxthreads = Runtime.getRuntime().availableProcessors();
            int defaultthreads = (maxthreads + 1) / 2;

            int ltothreads = optAccessor.getIntProjectOption("C32Global", "lto.link.threads", 0);

            if(ltothreads > maxthreads)
                ltothreads = maxthreads;
            else if(ltothreads <= 0)
                ltothreads = defaultthreads;

            opt = ",--thinlto-jobs=" + Integer.toString(ltothreads);
        }

        return opt;
    }

    /* Return the "-L<dir>" option string that will point to the particular multilib variant that we 
     * need given our options and architecture.
     *
     * The directory order will be cpu/isa/dsp/fpu/fastmath/optimization_level.  Note that 'dsp'
     * applies only to MIPS devices.
     */
    private String getMultilibDirectoryOpt() {
        String multilibOpt = "-L\"=/";

        if(target.isMips32()) {
            multilibOpt += getMips32Multilib();
        } else if(target.isArm()) {
            if(target.supportsArmIsa()) {
                multilibOpt += getCortexAMultilib();
            } else {
                multilibOpt += getCortexMMultilib();
            }
        } else
            return "";

        multilibOpt += getCommonMultilibs();
        return multilibOpt + "\"";
    }

    private String getMips32Multilib() {
        String libdir = ClangLanguageToolchain.MIPS32_LIB_DIR + "/";
        libdir += target.getCpuName();

        // ISA is user-selectable via an option, so check that option.
        if(target.supportsMips16Isa()  &&  
           optAccessor.getBooleanProjectOption("C32-LD", "generate-16-bit-code", false)) {
            libdir += "/mips16";
        }
        else if(target.supportsMicroMipsIsa()  &&  target.supportsMips32Isa()) {
            if(optAccessor.getBooleanProjectOption("C32-LD", "generate-micro-compressed-code", false)) {
                libdir += "/micromips";
            }
        }
        // else will be MIPS32 or microMIPS for devices that support only the latter.

        if(target.supportsDspR2Ase()) {
            libdir += "/dspr2";
        }

        if(target.hasFpu()) {
            libdir += "/fpu64";
        }

        return libdir;
    }

    private String getCortexMMultilib() {
        String libdir = ClangLanguageToolchain.CORTEX_M_LIB_DIR + "/";
        libdir += target.getCpuName().replace('-', '_');

        // ISA is always Thumb on Cortex-M, so that's always the default.

        /* Get FPU.
         */
        if(target.hasFpu()) {
            libdir += "/" + target.getArmFpuName().toLowerCase();
        }
        
        return libdir;
    }

    private String getCortexAMultilib() {
        String libdir = ClangLanguageToolchain.CORTEX_A_LIB_DIR + "/";
        libdir += target.getCpuName().replace('-', '_');

        // ISA is user-selectable via an option, so check that option.
        if(optAccessor.getBooleanProjectOption("C32-LD", "generate-thumb-code", false)) {
            libdir += "/thumb";
        }

        /* Get FPU.
         */
        if(target.hasFpu()) {
            libdir += "/" + target.getArmFpuName().toLowerCase();
        }
        
        return libdir;
    }


    /* Output the multilib dirctories for options common to all architectures we support.
     */
    private String getCommonMultilibs() {
        String libdir = "";

        /* Get fast-math usage.
         */
        if(optAccessor.getBooleanProjectOption("C32-LD", "use-fast-math-libs", false)) {
            libdir += "/fast-math";
        }

        /* Get optimization level.
         */
        String opt = optAccessor.getProjectOption("C32-LD", "optimization-level", "");

        if(!opt.isEmpty()) {
            libdir += "/" + (opt.substring(1).toLowerCase());
        }

        return libdir;
    }
}
