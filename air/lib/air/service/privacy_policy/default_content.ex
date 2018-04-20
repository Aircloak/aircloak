defmodule Air.Service.PrivacyPolicy.DefaultContent do
  @moduledoc "Helper module containing the default privacy policy that is suggested for new installations."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the default privacy content"
  @spec get() :: String.t()
  def get(),
    do: """
    This privacy policy outlines what data is being collected about you when you are using Aircloak Insights.
    It also outlines your rights and who you should reach out to if you have questions or concerns.

    Please note that you can review this privacy policy and withdraw your consent at any time.

    # Contact information

    This Aircloak Insights instance is run and operated by __[FILL IN COMPANY NAME]__.
    As such __[FILL IN COMPANY NAME]__ is the Data Controller when it comes to the
    personal information collected by this Aircloak Insights instance.

    If you have questions or concerns you can contact the Data Controller or the Data Controller's representative at:

    __Data Controller__
    Address
    Contact information
    Phone number and or email address

    or the Data Controller's Data Protection Officer:

    __Data Protection Officer__
    Name of data protection officer
    Building and office number
    Phone number and or email address

    # Collection of information

    ## Information collected for the Data Controller

    The following information is recorded by Aircloak Insights:

    ### Personal information
    Your name and email address as part of your user profile as well as other Aircloak Insights
    specific system settings.

    ### Query history
    The queries you execute in Aircloak Insights along with related metadata and the query
    results are recorded. Related metadata includes such things as:

    - the data source the query was run against
    - how long the query took to execute
    - how many columns were requested and how many columns had to be loaded out of the database
    - what query features and capabilities were used (such as function names)

    ### Audit log
    Aircloak Insights provides audit logging capabilities. In addition to the queries being
    executed the audit log records actions such as changing your profile information or logging in to the system.

    For users with administrative privileges, additional information is recorded, namely such actions as
    the creation of user accounts or groups, altering data sources, or changing the general system settings.

    ## Information collected for Aircloak GmbH

    Aircloak GmbH collects pseudonymised statistics about the queries being executed.
    Each analyst is identified by a pseudonym. The pseudonym is an abstract identifier that
    only gives Aircloak GmbH the ability to correlate multiple actions taken by a single individual.
    It does not provide Aircloak GmbH with knowledge of the users name or other personal details.
    For each query the same metadata recorded by Aircloak Insights is also sent to Aircloak GmbH.
    This includes information such as:

    - the data source the query was run against
    - how long the query took to execute
    - how many columns were requested and how many columns had to be loaded out of the database
    - what query features and capabilities were used (such as function names)

    Aircloak GmbH does not record the queries themselves nor the query results.

    The information is analysed through an instance of Aircloak Insights.
    The fully anonymised statistical information of such analyses is subsequently used
    to assess how well different aspects of the system perform and help prioritise future improvements.

    ## Storage period

    ### Data collected by Aircloak Insights

    The data collected by an Aircloak Insights installation is stored indefinitely or until a user account is deleted

    ### Data collected by Aircloak GmbH

    The information stored by Aircloak GmbH is stored in pseudonymised form for a period of up to 6 months.
    The anonymous usage statistics generated based on the pseudonymised information is stored indefinitely.

    ## Who has access to your personal information

    ### Aircloak Insights

    In addition to the Data Controller, all Aircloak Insights users with administrative privileges
    have full insight into the personal information stored about a user.

    ### Aircloak GmbH

    The pseudonymous information collected by Aircloak Insights on behalf of Aircloak GmbH might be accessed
    by Aircloak GmbH engineers, administrators, and affiliated personnel for debugging purposes.

    # Rights

    If you are an EU citizen or an individual residing within the European Union,
    the General Data Protection Regulation (GDPR) grants you a set of rights, namely the right to:

    - access the data stored about you in a portable format
    - rectify the information stored about you
    - erase the information stored about you
    - restrict or object to the processing of your data
    - the ability to withdraw your consent to data collection and processing

    ## Right to access

    When logged in to Aircloak Insights you can at any time download an archive of the information
    the system has stored about you. You system administrator, controller or data protection officer
    might be able to provide you with auxiliary server logs for the actions performed by you through
    auxiliary systems, such a web server logs.

    ## Right to rectify the information

    When logged in to Aircloak Insights you can alter your profile information.

    ## Right to erase the information

    You can at any time request that your data protection officer, controller or administrator delete your account.
    This will in turn delete all the information Aircloak Insights stores about you.

    If you want Aircloak GmbH to erase the pseudonymised information it stores about you, please contact
    compliance@aircloak.com, providing your Aircloak Identifier. You can find this identifier in the profile
    section of the Aircloak Insights user interface.

    ## Right to restrict or object to the processing
    The collection of the personal information outlined above is a requirement to using Aircloak Insights.
    You are however free to not accept this privacy policy. Not accepting the privacy policy will prevent
    you from accessing, using and querying Aircloak Insights.

    ## Right to withdraw your consent
    You can review this privacy policy at any point while logged into the Aircloak Insights web interface.
    On the same page where you can review the privacy policy you also have the ability to withdraw your consent.
    Doing so will prevent you from using the system, but will not remove any information already stored about you
    by Aircloak Insights. Your right to erase the information afterwards remains untouched – please contact your administrator.
    """
end
