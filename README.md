# Geocomputation Project

This report employs a Maximum Entropy (MaxEnt) model to conduct a multi-criteria spatial analysis of data centre site selection across England. By integrating presence-only data from existing data centres with a range of predictors, the study identifies suitable areas for data centre building development. 

## Suitability Map
![image 1](./Data/SuitibilityMap/data_center_suitability.png)

## Files Explanation
`WebCrawler`: Get data centres' information from web pages, need google api for geocoding <br/>
`fullpreprocess`: Preprocess points, lines, and polygons into the same projection, resolution, and extent <br/>
`MaxEnt`: Run MaxEnt model for the suitability map, need to prepare raster data and data centres  <br/>

### Performance
`Permutation`: Calculate permutation importance for each predictor <br/>
`Kfold`: Perform k-fold cross-validation to evaluate model robustness <br/>
`model_performance`: Summarise model performance metrics such as AUC <br/>
`Risidual`: Analyze model residuals and test for spatial autocorrelation (Moran's I) <br/>

### Datasource

| Variable Name | Spatial Resolution/Scale - Data type |
| --- | --- |
| **[Existing Data Centres](https://www.datacentermap.com/united-kingdom/)** (Oct. 2025)| Not Specified - Vector |
| **[Major Roads](https://digimap.edina.ac.uk/)** (Ordnance Survey, Oct. 2025) | 1:25,000 - Vector |
| **[Time to Large Employers](https://www.gov.uk/government/statistical-data-sets/journey-time-statistics-data-tables-jts)**  (Dept. of Transport, Nov. 2021) | Not Specified - Vector |
| **[Solar Irradiation](https://globalsolaratlas.info/map?s=52.531021,-1.264906&m=site&c=52.531021,-1.264906,11)** (Oct. 2019) | 30km - Raster |
| **[Soil Suitability](https://www.bgs.ac.uk/datasets/bgs-geology-625k/)** (British Geological Survey, n.d.) | 1:625,000 - Vector |
| **[Flood Risk Areas](https://environment.data.gov.uk/dataset/f3d63ec5-a21a-49fb-803a-0fa0fb7238b6)** (Environment Agency, Dec. 2018) | Not Specified - Vector |
| **[Brownfields](https://www.planning.data.gov.uk/dataset/brownfield-land)** (Gov.UK, Jan. 2026) | Not Specified - Vector |
| **[Drought Severity Index](https://climate-themetoffice.hub.arcgis.com/datasets/b9e6f84d2ee943d0be17d93366bca8dc_0/explore?location=54.832243%2C-3.273213%2C6.19)** (Mar. 2025) | 12km - Raster |
| **[Annual Median Air Temperature](https://climate-themetoffice.hub.arcgis.com/datasets/TheMetOffice::annual-average-temperature-change-projections-local-authority-v1/about)** (Jul. 2024) | Not Specified - Vector |
| **[Substations](https://ukpowernetworks.opendatasoft.com/explore/?disjunctive.theme&disjunctive.dublin-core.subject&sort=explore.popularity_score)** (2025) | Not Specified - Vector |
| **[Underground Cables](https://ukpowernetworks.opendatasoft.com/explore/?disjunctive.theme&disjunctive.dublin-core.subject&sort=explore.popularity_score)** (Nov 2025) | Not Specified - Vector |
| **[Overhead Lines]((https://ukpowernetworks.opendatasoft.com/explore/?disjunctive.theme&disjunctive.dublin-core.subject&sort=explore.popularity_score))** (Nov 2025) | Not Specified - Vector |
| **[Northeast and Yorkshire Areas Headroom Capacity](https://northernpowergrid.opendatasoft.com/explore/dataset/heatmapsubstationareas/information/?disjunctive.name&disjunctive.type&disjunctive.pvoltage&disjunctive.local_authority&disjunctive.gen_voltage_constraint&disjunctive.dem_voltage_constraint&disjunctive.gsp_hm_constraint_manual&disjunctive.genconstraint&disjunctive.demconstraint&disjunctive.worst_case_constraint_gen_colour&disjunctive.worst_case_constraint_dem_colour&disjunctive.upstreamname)** (Northern Powergrid, Jan 2026) | Not Specified - Vector |
| **[Headroom Capacity](https://ukpowernetworks.opendatasoft.com/explore/dataset/dfes-network-headroom-report/information/)** (UK Power Networks, Jan 2026) | Not Specified - Vector |
| **[Headroom Capacity](https://www.ssen.co.uk/about-ssen/dso/publications-and-reports/)** (SSEN, Nov 2025) | Not Specified - Vector |
| **[Headroom Capacity](https://electricitynorthwest.opendatasoft.com/explore/dataset/ndp-pry-bsp-headroom/information/)** (Electricity North West, Jan 2025) | Not Specified - Vector |
