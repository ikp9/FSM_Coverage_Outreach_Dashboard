# FSM_Coverage_Outreach_Dashboard
FSM child vaccination coverage analysis code, along with outreach planning from DSU project - dashboard with vaccination coverage and outreach planning tools

#Changes made July 29, 2026 - AT
Add analytic script
Updated directory path
Updated file directories with new data
Updated as of date to July 1
Updated analytic code output directory
Updated output files so today's date is automatically added, so we don't have to manually update

#Changes made July 30, 2026 - AT
Update tables code from multiple chunks with multiple Excel exports to creates one file named FSM_Child_Coverage_Outreach_Tables_MMDDYY.xlsx
Adds the 60–71 months and 72–83 months age groups
Adds FSM, Chuuk, Kosrae, Pohnpei, and Yap breakdowns to:
IIS denominator by age
Dose-specific coverage
Adds:
State and County before Village
State before County in the three county-level tables
Automatically detects the state variable from state.x, state, state_join, or state.y.
Checks for missing required variables and returns a clear error listing them.
Preserves the Chuuk and Yap regional breakdowns in the 19–35-month tables.
Cleans and standardizes state, county, and village names.
Delete old code chunks

Added app files - install packages, app, prepare data, and route functions
Updated paths.

#Changes made August 4, 2026 - AT
Changed all paths to the FSM_Coverage_Outreach_Dashboard; moved all data here so no longer pulling from old folder

#changes made August 5, 2026 - EB
Made changes to directory pathways and import pathways

