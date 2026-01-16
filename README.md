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

| Variable Name | Dataset (Provider, Last Updated) | Spatial Resolution/Scale - Data type |
| --- | --- | --- |
| **[Existing Data Centres](https://www.datacentermap.com/united-kingdom/)** | Data Centre Map (n.d.) | Not Specified - Vector |
| **[Major Roads](https://digimap.edina.ac.uk/)** | OS Open Roads (Ordnance Survey, Oct. 2025) | 1:25,000 - Vector |
| **[Time to Large Employers](https://www.gov.uk/government/statistical-data-sets/journey-time-statistics-data-tables-jts)** | Journey Time Statistics database (Dept. of Transport, Nov. 2021) | Not Specified - Vector |
| **[Solar Irradiation](https://globalsolaratlas.info/map?s=52.531021,-1.264906&m=site&c=52.531021,-1.264906,11)** | Solargis (Oct. 2019) | 30km - Raster |
| **Soil Suitability** | BGS Geology 625K (British Geological Survey, n.d.) | 1:625,000 - Vector |
| **[Flood Risk Areas](https://environment.data.gov.uk/dataset/f3d63ec5-a21a-49fb-803a-0fa0fb7238b6)** | Flood Risk Areas (Environment Agency, Dec. 2018) | Not Specified - Vector |
| **[Brownfields](https://www.planning.data.gov.uk/dataset/brownfield-land)** | Brownfield land dataset (Gov.UK, Jan. 2026) | Not Specified - Vector |
| **[Drought Severity Index](https://climate-themetoffice.hub.arcgis.com/datasets/b9e6f84d2ee943d0be17d93366bca8dc_0/explore?location=54.832243%2C-3.273213%2C6.19)** | Met Office Climate Data Portal (Mar. 2025) | 12km - Raster |
| **[Annual Median Air Temperature](https://climate-themetoffice.hub.arcgis.com/datasets/TheMetOffice::annual-average-temperature-change-projections-local-authority-v1/about)** | Met Office Climate Data Portal (Jul. 2024) | Not Specified - Vector |
| **Substations** | UK Power Networks (2025) | Not Specified - Vector |
| **Underground Cables** | UK Power Networks (Nov 2025) | Not Specified - Vector |
| **Overhead Lines** | UK Power Networks (Nov 2025) | Not Specified - Vector |
| **Supplementary Data** | **Specific to Power Infrastructure Analysis** |  |
| **Northeast and Yorkshire Areas Headroom Capacity** | Heat Map Data - Substation Areas (Northern Powergrid, Jan 2026) | Not Specified - Vector |
| **Headroom Capacity** | DFES NSHR (UK Power Networks, Jan 2026) | Not Specified - Vector |
| **Headroom Capacity** | Network Scenario Headroom Report (SSEN, Nov 2025) | Not Specified - Vector |
| **Headroom Capacity** | NDP PRY BSP Headroom (Electricity North West, Jan 2025) | Not Specified - Vector |