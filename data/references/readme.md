# Metadata disturbances.csv

Disturbance reference data collected via TimeSync (Cohen et al. 2010) for 19,996 plots across 35 countries in continental Europe. The data is from Senf et al. 2020 and follows a response design described in Senf et al. 2018.

*A NOTE OF CAUTION:* While we recorded an agent label (i.e., the cause of disturbance, including harvest, biotic, breakage, fire), the agent attribution is highly unreliable and depends strongly on the availability of high resolution imagery during interpretation. We strongly suggest to NOT use the agent label.

## Columns:

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

## References

Senf, C., Pflugmacher, D., Zhiqiang, Y., Sebald, J., Knorrn, J., Neumann, M., Hostert, P., Seidl, R. (2018) Canopy mortality has doubled across Europe’s temperate forests in the last three decades. Nature Communications, 9, 4978. https://doi.org/10.1038/s41467-018-07539-6

Senf, C., Sebald, J., Seidl. R. (2020) Increases in canopy mortality and their impact on the demographic structure of Europe’s forests. bioRxiv preprint. https://doi.org/10.1101/2020.03.30.015818

Cohen, W. B., Yang, Z., & Kennedy, R. (2010). Detecting trends in forest disturbance and recovery using yearly Landsat time series: 2. TimeSync - Tools for calibration and validation. Remote Sensing of Environment, 114(12), 2911–2924. https://doi.org/10.1016/j.rse.2010.07.010