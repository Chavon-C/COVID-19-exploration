## Report on COVID-19 trends in 5 countries: Portugal, Greece, Czechia, Sweden and Jordan. 

**Link to data set used**: [*COVID-19 data set on
GitHub*](https://github.com/owid/covid-19-data/blob/master/public/data/cases_deaths/full_data.csv)

This report looks at the monthly cases and deaths as well as the total
cases and deaths. You can use this data set to explore trends by data
visualisation with ggplot2 or using R base functions to get the summary
of each country. It will also help to investigate any trends between
COVID-19 cases over the span of 4 years, for example whether there are
seasonal trends in spikes of cases and deaths.

### script.R

This contains all the code used to create this report

Below is a table of the variables that were included in my final data
frame, which I used to explore the trends.

| Variable     | Description                                        |
|--------------|----------------------------------------------------|
| country      | Shows which country the observations were recorded |
| month        | Shows which month the observations were recorded   |
| new cases    | Number of new cases were recorded                  |
| new deaths   | Number of new deaths were recorded                 |
| total cases  | Cumulative number of new cases                     |
| total deaths | Cumulative number of new deaths                    |

Hopefully this data set will allow you to engage in meaningful
discussion about the comparison of the 5 countries: why there is such a
difference in the trends of observations and inspire you to conduct your
own further research to explore the causes of this.
