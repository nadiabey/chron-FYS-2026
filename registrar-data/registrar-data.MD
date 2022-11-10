This year, we attempted to use ethnicity data from the University Registrar to show the diversity of ethnic backgrounds at Duke. The registrar publishes this data in a dashboard [here.](https://public.tableau.com/app/profile/office.of.the.university.registrar.duke.university/viz/EthnicityTrendsatDuke2012onward/EthnicGroupMakeup)

The underlying dataset includes data from 2012 to 2023. We used the "Ethnic Group Makeup" data. The initial view can be filtered by Ethnic Group to show breakdowns within Ethnic Groups.

![alt text](https://github.com/nadiabey/chron-FYS-2026/blob/main/registrar-data/Screenshot%202022-11-10%20at%201.06.10%20PM.png "A circle graph showing Ethnic Group Makeup.")

We exported the Tableau workbook from the dashboard and extracted the [source data](https://github.com/nadiabey/chron-FYS-2026/blob/main/registrar-data/final%20data%20(DU_UR_ETHNIC_DETAILS)_final%20data.csv). This data included numbers of how many people selected a given race or ethnicity by academic year.

Further examination of the data showed that the percentages shown in the Tableau dashboard are out of ALL responses from 2012 to 2023. 

For example, there were 109,054 responses for the ethnic group "White" from 2012 to 2023. From 2012 to 2023, there were 93 responses for "Seljuk Turk." When the dashboard is filtered to "White," it says that 0.09% of students identify as "Seljuk Turk." Dividing 93 by 109,054 gives 0.085%, which rounds up to 0.09%.

![alt text](https://github.com/nadiabey/chron-FYS-2026/blob/main/registrar-data/Screenshot%202022-11-10%20at%201.05.51%20PM.png "A green circle graph showing the ethnic breakdown of White respondents. Nearly 97% of responses are just 'White.' A popup window above a tiny wedge says that 0.09% of responses were 'Seljuk Turk.'")

With this knowledge, we filtered the data to only include responses from the 2023 academic year. This is how we identified the ethnicity breakdown for this year.
