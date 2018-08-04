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

        commandLineProperties.put("MULTILIB_DIR_OPT", getMultilibDirectoryOpt(projectDescriptor, conf));
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

    /* Return the "-L<dir>" option string that will point to the particular multilib variant that we 
     * need given our options and architecture.
     *
     * The directory order will be cpu/isa/dsp/fpu/fastmath/optimization_level.  Note that 'dsp'
     * applies only to MIPS devices.
     */
// TODO:  Remove 'confBook' and 'conf' arguments from signature.
    private String getMultilibDirectoryOpt(MakeConfigurationBook confBook, MakeConfiguration conf) {
        String arch = getProjectOption(confBook, conf, "C32Global", "target.arch", "");
        String multilibOpt = "-L" + getToolchainBasePath(conf);

        if(arch.equals("mipsel-unknown-elf"))
            multilibOpt += getMips32Multilib(confBook, conf);
        else if(arch.equals("arm-none-eabi"))
            multilibOpt += getArmMultilib(confBook, conf);
        else
            return "";

        multilibOpt += getCommonMultilibs(confBook, conf);
        return multilibOpt;
    }

// TODO:  Remove 'confBook' and 'conf' arguments from signature.
    private String getMips32Multilib(MakeConfigurationBook confBook, MakeConfiguration conf) {
        String libdir = "";
        String opt;

        /* Get CPU type.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "target.mips32.cpu", "mips32r2");
        libdir += opt;

        /* Get ISA.
         */
        opt = getProjectOption(confBook, conf, "C32-LD", "generate-16-bit-code", "false");

        if(opt.equalsIgnoreCase("true")) {
            libdir += "/mips16e";
        }
        else {
            opt = getProjectOption(confBook, conf, "C32-LD", "generate-micro-compressed-code", "false");

            if(opt.equalsIgnoreCase("true")) {
                libdir += "/micromips";
            }
        }

        /* Get DSP.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "target.mips32.dsp", "");

        if(opt.equals("-mdspr2")) {
            libdir += "/dspr2";
        }

        /* Get FPU.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "target.mips32.fpu", "");

        if(opt.contains("hard")) {
            libdir += "/fpu64";
        }

        return libdir;
    }

// TODO:  Remove 'confBook' and 'conf' arguments from signature.
    private String getArmMultilib(MakeConfigurationBook confBook, MakeConfiguration conf) {
        String libdir = "";
        String opt;

        /* Get CPU type.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "target.arm.cpu", "cortex-m0plus");
        libdir += opt.replace('-', '_');

        /* Get ISA.
         */
        opt = getProjectOption(confBook, conf, "C32-LD", "generate-thumb-code", "false");

        if(opt.equalsIgnoreCase("true")) {
            libdir += "/thumb";
        }

        /* Get FPU.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "target.arm.fpu", "");

        if(opt.contains("vfp4-sp-d16")) {
            libdir += "/vfp4_sp_d16";
        }
        else if(opt.contains("vfp4-dp-d16")) {
            libdir += "/vfp4_dp_d16";
        }
        else if(opt.contains("vfp5-dp-d16")) {
            libdir += "/vfp5_dp_d16";
        }
        else if(opt.contains("neon-vfpv4")) {
            libdir += "/neon_vfpv4";
        }
        
        return libdir;        
    }

    /* Output the multilib dirctories for options common to all architectures we support.
     */
// TODO:  Remove 'confBook' and 'conf' arguments from signature.
    private String getCommonMultilibs(MakeConfigurationBook confBook, MakeConfiguration conf) {
        String libdir = "";
        String opt;

        /* Get fast math.
         */
        opt = getProjectOption(confBook, conf, "C32Global", "relaxed-math", "false");

        if(opt.equalsIgnoreCase("true")) {
            libdir += "/fastmath";
        }        

        /* Get optimization level.
         */
        opt = getProjectOption(confBook, conf, "C32-LD", "optimization-level", "");

        if(!opt.isEmpty()) {
            libdir += "/" + (opt.substring(1));
        }

        return libdir;
    }    
}
