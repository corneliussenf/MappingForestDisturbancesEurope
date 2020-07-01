# Metadata disturbances.csv #

Disturbance reference data collected via TimeSync for 19,996 plots across 35 countries in continental Europe.

A NOTE OF CAUTION: While we recorded an agent label (i.e., the cause of disturbance, including harvest, biotic, breakage, fire), the agent attribution is highly unreliable and depends strongly on the availability of high resolution imagery during interpretation.

Columns:

- country: the country the plots are located in
- plotid: a country-specific id (i.e., starting with 1 for each country)
- disturbance_n: number of disturbances detected during the period 1986-2018 (maximum of 3; NA if not disturbance detected)
- year_disturbance_1: Year of the first disturbance detected (NA if no disturbance detected)
- year_disturbance_2: Year of the second disturbance detected (NA if only one disturbance detected)
- year_disturbance_3: Year of the third disturbance detected (NA if only one or two disturbance(s) detected)
- agent_disturbance_1: Agent of the first disturbance (NA if no disturbance detected; SEE NOTE OF CAUTION!)
- agent_disturbance_2: Agent of the second disturbance detected (NA if only one disturbance detected; SEE NOTE OF CAUTION!)
- agent_disturbance_3: Agent of the third disturbance detected (NA if only one or two disturbance(s) detected; SEE NOTE OF CAUTION!)
- severity_disturbance_1: Severity of the first disturbance, expressed as stand-replacing (SR; all canopy removed during disturbance event leading to non-treed land cover) or non-stand-replacing (NSR; residual canopy cover after disturbances leading to treed land cover). NA if not disturbance detected.
- severity_disturbance_2: Severity of the second disturbance (see description above; NA if only one disturbance detected)
- severity_disturbance_3: Severity of the first disturbance (see description above; NA if only one or two disturbance(s) detected)
