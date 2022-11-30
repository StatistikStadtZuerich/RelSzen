# RelSzen

*Scenario model of the future reformed population of the city of Zurich*

## Model

This [R](https://www.r-project.org/) code calculates population scenarios for the future reformed population of the city of Zurich. For data protection reasons the input and output data are not available here. The results of the calculations are population sizes by the ten church districts, years, and age classes (10 year classes).

## Renv

### To users
We use an [`renv`](https://rstudio.github.io/renv/articles/renv.html) enabled RStudio project to manage the local environment with libraries in order to ensure reproducible results, correct working directories etc. You can use the included one or [create your own](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects).  
When you pull this repository, make sure to restart your R session (this is done automatically if you open this as an RStudio project). R will read the project specific `.Rprofile` file and thereby activate `renv`.  
You can also activate it manually by using `renv::activate()`. This might lead to the download of many (potentially older) packages.  
Keep in mind that you will be working in this specific environment as long as you're not switching to another project or manually deactivate renv (`renv::deactivate()`).

### To contributors
If a new package is required, use `renv::install("new package")`. For updates, use `renv::update("update_package")`.  
Then add this to the lock file by running `renv::snapshot()` and commit the changes in renv.lock. 
A good overview can be found [here](https://rstudio.github.io/renv/articles/renv.html)

## Author

[Statistik Stadt Zürich](mailto:statistik@zuerich.ch)

