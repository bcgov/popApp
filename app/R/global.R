# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#Sourcing weighted median function
median_pop <- function(a, p, t){
  #Consumes the vectors of the same length that represent the age, the 
  #population by age, and the total population. It returns the median as 
  #computed by the ministry of health
  
  #Computing percentiles
  perc<-cumsum({{p}})/{{t}}
  #Computing share of the population in an age bracket
  share <- {{p}}/{{t}}
  #Computing percentile up to the previous age bracket
  previous<- perc-share
  #Computing the median
  median <- case_when(
    perc == 0.5 ~ {{a}}+1,
    previous < 0.5 & perc > 0.5 ~ {{a}}+(0.5-previous)/share,
    TRUE ~ 0
  )
  #Since the result above may return more than one median different from zero 
  #due to populations of size zero, we pick the median with the first non zero
  #size population (the max).
  return(janitor::round_half_up(max(median),1))
}
