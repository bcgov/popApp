[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

popApp
============================

A dashboard for presenting B.C. sub-provincial population estimates and projections by age and gender and a variety of region types (e.g., community health service areas, school districts, etc.)

Code is also provided for the data cleaning process.

### Data

Estimates:
BC Stats annually releases total population estimates for a variety of sub-provincial boundary types. These estimates are consistent in aggregate with the July 1st provincial level estimates produced by Statistics Canada.

Projections:
BC Stats applies the Component/Cohort-Survival method to project the population. This method "grows" the population from the latest base year estimate by forecasting births, deaths and migration by age. These forecasts are based on past trends modified to account for possible future changes and, consequently, should be viewed as only one possible scenario of future population. Projections are also released annually and are as of July 1st.

### Usage

The app.R code is used to create a shiny dashboard which is hosted on [shinyapps.io](https://bcstats.shinyapps.io/popApp). Data are sourced and tidied in the get_data.R script. 

### Project Status

Stable

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/popApp/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---

