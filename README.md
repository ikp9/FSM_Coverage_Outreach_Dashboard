#Raw data files can be found at:
\\cdc.gov\locker\NCIRD_ISD_AB_DATA2\Island_Surveys\Data Collection\Federated States of Micronesia (FSM)\2026\Dashboard\Raw Patient Data

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

#Changes made August 5, 2026 - AT
Updated all paths to relative so any user with same folder setup can run without editing user names 
Fixed issue with regions - e.g., Chuuk outer being too low
Updated prepare_data to match path updates in analytic code

#Updates August 6, 2026 - AT
Added output of patients needing vax file
Updated app so cards on coverage page dynamically update with age group
Added 2 - 83 months to vaccination coverage page

#Updates 8/19/2026 - AT
Added telephone and cell phone to reminder/recall output

#Updates 8/24/26 - AT
Switched from Markdown to Quarto

#Updates 8/28/26 - AT
Brought in de-id adolescent dataset to prepare data and app. App now inlcudes adolescent coverage and planning page. 

#Updates 9/29/26 - AT
Updated patients needing vaccinations table to include mmr1utd, mmr2utd, needmmr1, needmmr2


# Priority score update — October 30, 2026

Individual risk is 20 points when non-MMR vaccinations are not up to date,
55 points when MMR is not up to date, plus up to 25 points for time since the
last vaccination (days / 30.4375, capped at 36 months). Children not yet eligible
for MMR receive no MMR contribution. Missing vaccination intervals retain the
existing maximum time contribution. The preparation script recalculates the
individual score so older analytic exports also use the corrected definition.

Community patient risk remains 60% median individual score, 20% 75th percentile,
and 20% percentage scoring at least 70. Priority now combines patient risk,
density-based transmission potential, and high-risk child burden with weights
of 90%, 5%, and 5%, respectively. Access/remoteness and low-density
access terms, including the remoteness-based Moderate floor, are removed from
priority. Coverage-based High overrides and scenario-specific score tertiles
remain unchanged. Operational access metadata remains available in prepared
data but is not used in the score or component chart.

After changing analytic inputs, run `source(here::here("R", "prepare_data.R"))`
before starting the app. Priority levels are recalculated from the current data and score tertiles,
then the program review rules below are applied.


# Community priority Excel export — November 2, 2026

Every completed run of the child analysis now refreshes dashboard preparation
using that run's exact deidentified patient export and creates
`data/Analytic Code Output/Community Priority Score_MMDDYY.xlsx`. Running
`R/prepare_data.R` directly also creates the workbook. The existing `openxlsx`
dependency writes a filterable table sorted by Priority Rank, with frozen
headers, typed numeric values, and the requested coverage and score columns.
Priority Rank, Number of Children, Children Not UTD, and Data Quality Flag follow
the nine requested columns. A Definitions sheet records units, denominators,
scoring rules, missing-value handling and the source patient filename.

Population Density is people per square kilometre. Median time since vaccination
matches the dashboard: median among children who are not UTD, capped at 36
months. Missing densities and unavailable medians remain blank. Coverage cells
store numeric proportions and display as percentages with one decimal place.
Rerunning on the same day replaces that day's workbook; earlier dated files
are retained. If the workbook is open in Excel, close it before rerunning.


# Priority review overrides — November 2, 2026

Unknown county records are classified using the total number of children with
unknown county in each state: fewer than 15 is Low, 15 through 30 inclusive is
Moderate, and more than 30 is High for program data-quality review. This applies
to the state-specific Zz-Unknown records and other Unknown/Missing county labels.
Known counties with 10 or fewer children are changed from High to Moderate.
All counties with 10 or fewer children receive a Small denominator flag.

Every applicable data-quality flag is retained, joined with semicolons. Unknown
county records also require geography resolution, and those with more than 30
children receive a Data quality review required flag. Missing coordinates,
Census matches, land area and density are reported independently. Ready means
no flags apply. Unresolved or unknown geography is not ready for routing.

These rules change Priority Level, not the numerical Priority Score or its
rank. The original classification is retained as priority_group_before_review
in the prepared community CSV. The dashboard and automatic Excel export both
use the reviewed level and full set of flags. Excel wraps compound flag text.


# Dashboard data-as-of labels — November 2, 2026

A shared header shows Data as of [date] on every dashboard page. Child pages,
community profiles, planning and methods use the child analysis age_as_of_date.
The adolescent page uses coverage_as_of_date associated with the adolescent
extract. Export dates, file modification dates and the current date are not
used as data cutoff dates.

The child analysis saves its cutoff in the deidentified workbook's new
Analysis_Metadata sheet and passes it to dashboard preparation. Existing
adolescent extracts record their coverage date in the hpv_count definition on
Data_Dictionary. Preparation writes both dates to data/dashboard_metadata.csv,
which is loaded with the prepared dashboard data. A previous date is reused
only for the same source extract; unavailable dates are explicitly labelled
not recorded. The loaded legacy data are as of September 29, 2026 (child) and
August 1, 2026 (adolescent), rather than the dates of newer analysis settings.


# Seasonal flu page — November 2, 2026

The Seasonal Flu page reads the newest Dataset 4_Seasonal Flu_DeID_MMDDYY.xlsx
from data/Analytic Code Output and the FSM sheet of data/Census_Flu_2026.xlsx.
Geography supports selecting FSM and any combination of Chuuk, Kosrae, Pohnpei
and Yap. Age group supports the five analytic age bands. Each selected geography
has a cumulative monthly coverage chart, including a Total for the selected
age groups. Summary cards show Census population and distinct seasonal doses.
When FSM is selected, cards count the national total once rather than adding
states again. Otherwise they sum the selected states. No COVID page or indicators
are added.

The analysis cutoff is September 30, 2026, with a July 1, 2026 season start,
recorded explicitly for the supplied extract in data/seasonal_flu_settings.csv.
The shared page header uses that analysis cutoff. File dates are used only to
select the latest extract, not to infer its cutoff. Add a settings row for a new
extract before loading it. An Analysis_Metadata sheet can supply its actual
coverage_as_of_date or analysis_as_of_date. The season start remains explicit.

Coverage uses one qualifying person from their first dose through each completed
month divided by the matching geography/age Census population. The dose card
counts distinct person/date doses and is not a count of fully vaccinated people.
Source rows are already patient-level and deidentified; no identity-based
deduplication across rows is possible or attempted. Repeat dose columns on one
row are deduplicated by date. Vaccinated records without a state contribute to
FSM only. The existing child/adolescent data and priority calculations are
unchanged. The y-axis scale control and Plotly zoom help inspect low coverage.
