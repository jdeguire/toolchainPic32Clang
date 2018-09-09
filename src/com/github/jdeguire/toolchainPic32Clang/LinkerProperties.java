/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.mdbcore.common.streamingdata.StreamingDataEnums;
import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.TraceSetupInformationInterface;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.MemRegionType;
import com.microchip.mplab.nbide.toolchainCommon.ReservedMemoryRangesCalculator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

/**
 *
 * @author jose
 * Modified by jdeguire for toolchainPic32Clang.
 */
public final class LinkerProperties extends CommonProperties {

    private final ReservedMemoryRangesCalculator memCalc;

    public LinkerProperties(final MakeConfigurationBook projectDescriptor,
            final MakeConfiguration conf,
            final Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);
	memCalc = new ReservedMemoryRangesCalculator(conf, assembly, getPic());

        addDebuggerNameOptions();
        commandLineProperties.put("INSTRUMENTED_TRACE_OPTIONS", getTraceOptions());
        commandLineProperties.put("FUNCTION_LEVEL_PROFILING_OPTIONS", getFunctionLevelProfilingOptions()); // waiting on compiler 2013.06.04
        commandLineProperties.put("project_cpp", shouldBuildWithCPP(ClangLanguageToolchain.CPP_SUPPORT_FIRST_VERSION));
        Boolean hasMemReservation = hasMemReservation();
        commandLineProperties.put("HAS_DEBUG_RANGES", hasMemReservation.toString());
        commandLineProperties.put("IDE_DASHBOARD", getIdeDashboardProperty());

        String memRanges = hasMemReservation ? getDebugMemRanges() : "";
        commandLineProperties.put("DEBUG_MEMORY_RANGES", memRanges);
        commandLineProperties.put("CHIPKIT_DEBUG_SYMBOL", getChipKITDebugSymbol());

        commandLineProperties.put("THINLTO_THREADS_OPT", getThinLtoThreadsOpt());
        commandLineProperties.put("MULTILIB_DIR_OPT", getMultilibDirectoryOpt());
    }

    private String getChipKITDebugSymbol() {
        boolean applicable = toolchainVersionGreaterOrEqualTo(ClangLanguageToolchain.CHIPKIT_SUPPORT_FIRST_VERSION);
        return applicable ? ",-D=__DEBUG_D" : "";
    }

    private String getIdeDashboardProperty() {
        boolean applicable = calc.supportsMemorySummary(conf);
        return applicable ? ",--memorysummary," + LTUtils.MEMORY_FILE_ADDRESS : "";
    }

    private boolean hasMemReservation() {
        boolean isPIC32C = isPIC32C();
        if (isPIC32C) {
            return false;
        }

        return toolchainVersionGreaterOrEqualTo(ClangLanguageToolchain.MEM_RESERVATION_SUPPORT_FIRST_VERSION);
    }

    private String getDebugMemRanges() {
        final Map<MemRegionType, List<Pair<Long, Long>>> ranges = memCalc.getReservedMemoryRanges();
        final List<Pair<Long, Long>> prgRanges = ranges.get(MemRegionType.PROGRAM);
        final List<Pair<Long, Long>> dataRanges = ranges.get(MemRegionType.DATA);
        final List<Pair<Long, Long>> bootRanges = ranges.get(MemRegionType.BOOT);
        final String prgStr = rangesToString(prgRanges, "program");
        final String dataStr = rangesToString(dataRanges, "data");
        final String bootStr = rangesToString(bootRanges, "boot");
        return prgStr + " " + dataStr + " " + bootStr;
    }

    private String rangesToString(final List<Pair<Long, Long>> rs, final String region) {
        final String tmpl = String.format("-mreserve=%s@0x%s:0x%s", region, "%s", "%s");
        final StringBuilder sb = new StringBuilder("");
        boolean first = true;
        for (Pair<Long, Long> r : rs) {
            if (!first) {
                sb.append(" ");
            }
            first = false;
//            r = new Pair<Long, Long>(LTUtils.ensureVirtualAddress(vatt, r.first), LTUtils.ensureVirtualAddress(vatt, r.second));
            final String begin = Long.toString(r.first, 16).toUpperCase();
            final String end = Long.toString(r.second - 1, 16).toUpperCase();
            sb.append(String.format(tmpl, begin, end));
        }
        return sb.toString();
    }


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

    public String getFunctionLevelProfilingOptions(final TraceSetupInformationInterface tsi, final String projBaseDir) {
        return "-lcppcfl ".substring(0);
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
        String multilibOpt = "-L" + getToolchainBasePath();

        if(target.isMips32())
            multilibOpt += getMips32Multilib();
        else if(target.isArm())
            multilibOpt += getArmMultilib();
        else
            return "";

        multilibOpt += getCommonMultilibs();
        return multilibOpt;
    }

    private String getMips32Multilib() {
        String libdir = "";

        libdir += target.getCpuName();

        // ISA is user-selectable via an option, so check that option.
        if(optAccessor.getBooleanProjectOption("C32-LD", "generate-16-bit-code", false)) {
            libdir += "/mips16e";
        }
        else if(optAccessor.getBooleanProjectOption("C32-LD", "generate-micro-compressed-code", false)) {
            libdir += "/micromips";
        }
        // else will be MIPS32

        if(target.supportsDspR2Ase()) {
            libdir += "/dspr2";
        }

        if(target.hasFpu()) {
            libdir += "/fpu64";
        }

        return libdir;
    }

    private String getArmMultilib() {
        String libdir = "";

        libdir += target.getCpuName().replace('-', '_');

        // ISA is user-selectable via an option, so check that option.
        if(optAccessor.getBooleanProjectOption("C32-LD", "generate-thumb-code", false)) {
            libdir += "/thumb";
        }
        // Else will be ARM

        /* Get FPU.
         */
        if(target.hasFpu()) {
            libdir += "/" + target.getArmFpuName();
        }
        
        return libdir;        
    }

    /* Output the multilib dirctories for options common to all architectures we support.
     */
    private String getCommonMultilibs() {
        String libdir = "";

        /* Get fast math.
         */
        if(optAccessor.getBooleanProjectOption("C32Global", "relaxed-math", false)) {
            libdir += "/fastmath";
        }        

        /* Get optimization level.
         */
        String opt = optAccessor.getProjectOption("C32-LD", "optimization-level", "");

        if(!opt.isEmpty()) {
            libdir += "/" + (opt.substring(1));
        }

        return libdir;
    }    
}
