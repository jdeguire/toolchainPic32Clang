/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.crownking.Pair;
import com.microchip.crownking.opt.Version;
import com.microchip.mplab.crownkingx.xPIC;
import com.microchip.mplab.logger.MPLABLogger;
import com.microchip.mplab.mdbcore.assemblies.Assembly;
import com.microchip.mplab.mdbcore.assemblies.AssemblyProvider;
import com.microchip.mplab.mdbcore.common.streamingdata.StreamingDataEnums;
import com.microchip.mplab.mdbcore.common.streamingdata.interfaces.TraceSetupInformationInterface;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.XMLBaseMakefileWriter;
import com.microchip.mplab.nbide.toolchainCommon.LTUtils;
import com.microchip.mplab.nbide.toolchainCommon.LTUtilsConfiguration;
import com.microchip.mplab.nbide.toolchainCommon.MPLinkUtils;
import com.microchip.mplab.nbide.toolchainCommon.MemRegionType;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import org.netbeans.api.project.Project;

/**
 *
 * @author jose
 * Modified by jdeguire for toolchainPic32Clang.
 */
public class LinkerProperties extends CommonProperties {

    public LinkerProperties(MakeConfigurationBook projectDescriptor,
            MakeConfiguration conf,
            Properties commandLineProperties) {
        super(projectDescriptor, conf, commandLineProperties);

        addDebuggerNameOptions(conf.getPlatformTool().getValue(), getPic(),commandLineProperties);
        commandLineProperties.put("INSTRUMENTED_TRACE_OPTIONS", getTraceOptions(projectDescriptor, conf));
        commandLineProperties.put("FUNCTION_LEVEL_PROFILING_OPTIONS", getFunctionLevelProfilingOptions(projectDescriptor)); // waiting on compiler 2013.06.04
        commandLineProperties.put("project_cpp", CompilerProperties.buildWithGPP(projectDescriptor, conf));
        Boolean hasMemReservation = hasMemReservation(projectDescriptor, conf);
        commandLineProperties.put("HAS_DEBUG_RANGES", hasMemReservation.toString());
        commandLineProperties.put("IDE_DASHBOARD", getIdeDashboardProperty(conf));

        LTUtilsConfiguration cfg = LTUtils.getConfiguration(projectDescriptor, conf);
        String memRanges = hasMemReservation ? getDebugMemRanges(cfg) : "";
        commandLineProperties.put("DEBUG_MEMORY_RANGES", memRanges);
        commandLineProperties.put("CHIPKIT_DEBUG_SYMBOL", getChipKITDebugSymbol(cfg));

        commandLineProperties.put("XC32_COMPAT_MACROS", CompilerProperties.getXC32CompatibilityMacros(projectDescriptor, conf));
    }

    private static String getChipKITDebugSymbol(final LTUtilsConfiguration cfg) {
        boolean applicable = LTUtils.toolchainVersionGreaterOrEqualTo("1.34", cfg.conf);
        return applicable ? ",-D=__DEBUG_D" : "";
    }

    private static String getIdeDashboardProperty(MakeConfiguration conf) {
        boolean applicable = ClangLanguageToolchain.supportsMemorySummary(conf);
        return applicable ? ",--memorysummary," + LTUtils.MEMORY_FILE_ADDRESS : "";
    }

    private boolean hasMemReservation(final MakeConfigurationBook projectDescriptor, final MakeConfiguration conf) {
        boolean isPIC32C = ClangLanguageToolchain.isPIC32C(getPic());
        if (isPIC32C){
            return false;
        }
        
        final String version = LTUtils.getVersion(conf);
        if (null == version || version.isEmpty()) {
            MPLABLogger.mplog.log(Level.SEVERE, "XC16LinkSelectedProperties::hasMemReservation, could not determine the toolchain version.");
            return false;
        }
        final Version cVersionActual = new Version(version);
        final Version cVersionBase = new Version("1.30");
        return cVersionActual.compareTo(cVersionBase) >= 0;
    }

    private String getDebugMemRanges(final LTUtilsConfiguration cfg) {
        final Map<MemRegionType, List<Pair<Long, Long>>> ranges = MPLinkUtils.getReservedMemoryRanges(cfg);
        final List<Pair<Long, Long>> prgRanges = ranges.get(MemRegionType.PROGRAM);
        final List<Pair<Long, Long>> dataRanges = ranges.get(MemRegionType.DATA);
        final List<Pair<Long, Long>> bootRanges = ranges.get(MemRegionType.BOOT);
        final String prgStr = rangesToString(prgRanges, "program");
        final String dataStr = rangesToString(dataRanges, "data");
        final String bootStr = rangesToString(bootRanges, "boot");
        return prgStr + " " + dataStr + " " + bootStr;
    }

    private String rangesToString(List<Pair<Long, Long>> rs, String region) {
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

    public static String getLinkerGldFileName(MakeConfigurationBook projectDescriptor, MakeConfiguration conf) {
        List<String> filesInGldDir = projectDescriptor.getFilesFromLinkerFolder(conf);
        String res = null;

        if (filesInGldDir != null && filesInGldDir.size() > 0) {
            res = filesInGldDir.get(0);
            // File has unix style escape sequence.
            res = XMLBaseMakefileWriter.getFileNameQuoted(res);
            // TODO add logging if size() > 1
        }

        return res; // NOI18N
    }

    public static void addDebuggerNameOptions(final String debuggerName, final xPIC pic,
            Properties commandLineProperties) {

        String debuggerOptionsAsSymbol = "";
        String debuggerOptionsAsMacro = "";

        if (debuggerName != null && debuggerName.length() > 0) {
            debuggerOptionsAsSymbol = "--defsym=" + debuggerName + "=1";
            debuggerOptionsAsMacro = "-D" + debuggerName + "=1";
        }
        if (debuggerOptionsAsSymbol.length() > 0) {
            commandLineProperties.put("COMMA_BEFORE_DEBUGGER_NAME", ",");
            boolean isPIC32C = ClangLanguageToolchain.isPIC32C(pic);
            String debuggerOptionString = isPIC32C ? "" : "-mdebugger";
            commandLineProperties.put("DEBUGGER_OPTION_TO_LINKER", debuggerOptionString);
        } else {
            commandLineProperties.put("COMMA_BEFORE_DEBUGGER_NAME", "");
            commandLineProperties.put("DEBUGGER_OPTION_TO_LINKER", "");
        }

        commandLineProperties.put("DEBUGGER_NAME_AS_SYMBOL", debuggerOptionsAsSymbol);
        commandLineProperties.put("DEBUGGER_NAME_AS_MACRO", debuggerOptionsAsMacro);
    }

    public static String getDebuggerName(String debugger) {
        String res = "";

        if (debugger.equals("RealICEPlatformTool")) {
            res = "__MPLAB_DEBUGGER_REAL_ICE";
        } else if (debugger.equals("ICD3PlatformTool")) {
            res = "__MPLAB_DEBUGGER_ICD3";
        } else if (debugger.equals("PICkit3PlatformTool") || debugger.equals("PK3OBPlatformTool") || debugger.equals("PKOBSKDEPlatformTool")) {
            res = "__MPLAB_DEBUGGER_PK3";
        } else if (debugger.equals("PICkit2PlatformTool")) {
            res = "__MPLAB_DEBUGGER_PICKIT2";
        } else if (debugger.equals("ICD2PlatformTool")) {
            res = "__MPLAB_DEBUGGER_ICD2";
        } else if (debugger.equals("SKDEPIC32PlatformTool")) {
            res = "__MPLAB_DEBUGGER_PIC32MXSK";
        } else if (debugger.equals("Simulator")) {
            res = "__MPLAB_DEBUGGER_SIMULATOR";
        } else if (debugger.equals("ICD4Tool")) {
            res = "__MPLAB_DEBUGGER_ICD4";
        }

        return res;
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
                        if (tsi.isTraceEnabled() && (tsi.getTraceSelect() == StreamingDataEnums.eSDTraceSelect.SD_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {  //StreamingDataController.SDC_TRACE_SELECT_USER_INSTRUMENTED_TRACE)) {
                            return " -lsol";
                        }
                    }
                }
            }
        }

        return "";
    }

    private String getFunctionLevelProfilingOptions(MakeConfigurationBook confBook) {
        if (confBook != null) {
            Project project = confBook.getProject();
            AssemblyProvider provider = project.getLookup().lookup(AssemblyProvider.class);
            if (provider == null) {
                MPLABLogger.mplog.log(Level.SEVERE, "ClangLinkerProperties::getFunctionLevelProfilingOptions, could not get the AssemblyProvider.");
            } else {
                Assembly assembly = provider.getAssembly();
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
                            return getFunctionLevelProfilingOptions(tsi, confBook.getBaseDir());
                        }
                    }
                }
            }
        }

        return "".substring(0);
    } // getFunctionLevelProfilingOptions

    //-----------------------------------------------------------------------
    public String getFunctionLevelProfilingOptions(final TraceSetupInformationInterface tsi, String projBaseDir) {
        return "-lcppcfl ".substring(0);
//   return "-lcdpf ".substring(0);
    } // getFunctionLevelProfilingOptions
}
