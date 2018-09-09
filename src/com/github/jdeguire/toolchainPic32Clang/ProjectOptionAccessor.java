/* @author jdeguire
 * Created: 1 Sep 2018
 *
 * This class provides a bit more convenient way to access the option of a project, which is needed
 * for determining what certain command-line options need to be emitted, such as using use options
 * to determine the multilib variant needed for the current target.
 */

package com.github.jdeguire.toolchainPic32Clang;

import com.microchip.mplab.nbide.embedded.makeproject.EmbeddedProjectSupport;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfiguration;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationBook;
import com.microchip.mplab.nbide.embedded.makeproject.api.configurations.MakeConfigurationException;
import org.netbeans.api.project.Project;

/**
 */
public class ProjectOptionAccessor
{
    private MakeConfigurationBook book_;
    private MakeConfiguration conf_;
    private Project project_;
    
    /* Create a new accessor object with the given configuration and book.
     */
    ProjectOptionAccessor(MakeConfigurationBook book, MakeConfiguration conf) {
        book_ = book;
        conf_ = conf;
        project_ = book_.getProject();
    }

    /* Get the current value of the given option as a string.  This will work for any option type
     * because all options are internally stored as strings.
     *
     * The optionBookId is the name given to the mp:configurationObject in the Clang.languageToolchain.xml
     * file, such as "C32Global", "C32", "C32CPP", etc.  The optionId is the name of the option itself 
     * assigned to the opt:id attribute.  This will return the given default value if the option could 
     * not be read for some reason.
     */
    public String getProjectOption(String optionBookId, String optionId, String defaultVal) {
        String ret = defaultVal;

        if(null != project_)
        {
            String val = EmbeddedProjectSupport.getSynthesizedOption(project_, conf_, optionBookId, 
                                                                     optionId, null); // NOI18N

            if(null != val)
                ret = val;
        }
        
        return ret;
    }
    
    /* Like above, but used for boolean options such as the <opt:booleanOption>.  This returns True
     * if the option is "true" (ignoring case) or returns False otherwise.  This returns the given
     * default if the option could not be read for some reason.
     */
    public boolean getBooleanProjectOption(String optionBookId, String optionId, boolean defaultVal) {
        return Boolean.parseBoolean(getProjectOption(optionBookId, optionId, Boolean.toString(defaultVal)));
    }

    /* Like above, but used for integer options such as the <opt:rangeOption>.  This returns the
     * integer value of the option if it was able to be read; otherwise, it returns the default
     * value. If you have a <opt:stringOption> that takes a number, be sure to add the attribute
     *
     * opt:validatorclass="com.microchip.mplab.nbide.toolchainCommon.opt.validation.DecimalNumberOptionValidator"
     *
     * to have the option validated as the user enters the value.
     */
    public int getIntProjectOption(String optionBookId, String optionId, int defaultVal) {
        return Integer.getInteger(getProjectOption(optionBookId, optionId, Integer.toString(defaultVal)));
    }


    /* Set the value of the option to the given string if able.  This works for any option because 
     * option values are stored as strings internally; however, it is up to you to ensure that the
     * string is correct for the option (eg. represents an integer for options that expect it).
     *
     * This will throw an exception if the option could not be set.  This would happen if the option
     * could not be accessed for some reason (does not exist or an internal error) or if the string
     * is somehow not valid for the option (eg. the option expects an integer string but did not
     * get it).
    */
    public void setProjectOption(String optionBookId, String optionId, String val)
                          throws IllegalArgumentException, MakeConfigurationException {
        conf_.setGenericOption(project_, optionBookId, optionId, val);
    }

    /* Like above, but sets a boolean value for boolean options such as <opt:booleanOption>.
     */
    public void setBooleanProjectOption(String optionBookId, String optionId, boolean val)
                                 throws IllegalArgumentException, MakeConfigurationException {
        setProjectOption(optionBookId, optionId, Boolean.toString(val));
    }

    /* Like above, but sets an integer value for numeric options such as <opt:rangeOption>.
     */
    public void setIntProjectOption(String optionBookId, String optionId, int val)
                             throws IllegalArgumentException, MakeConfigurationException {
        setProjectOption(optionBookId, optionId, Integer.toString(val));
    }
}
