library(tmap)
library(ggplot2)
library(sf)

roads=st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/strategic_rds.shp")
emp=st_read("~/STA_MSc/Term_1/SAGc/Group assignment/Data/emp_5000.gpkg")

# plot the

# plot the distribution of how long it takes to get to a large employer
mean(emp$X5000EmpAvgt, na.rm = T) # the median average time - 24.09 minutes
ggplot(data=emp, aes(X5000EmpAvgt)) + geom_histogram() + labs(x="Average time to large Employers (In minutes)")

