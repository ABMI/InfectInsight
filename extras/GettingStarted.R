# # Following two packages are recommended to install before infection
# install.packages("devtools")
# devtools::install_github("OHDSI/DatabaseConnector")
# devtools::install_github("OHDSI/SqlRender")

library(infection)

library(devtools)
library(DatabaseConnector)
library(SqlRender)
library(lubridate)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(plotly)
library(quantmod)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(summaryBox)
library(DT)
library(ggrepel)
library(gridExtra)
library(shinyWidgets)
library(shinyalert)
library(stringr)
library(rgdal)
library(ggiraphExtra)
library(kormaps2014)
library(shinycssloaders)
library(purrr)
library(XML)


# 1.DB connect
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms= "dbms",
                                                                server='server',
                                                                user='user',
                                                                password='password',
                                                                port='port',
                                                                pathToDriver = 'pathToDriver')
oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdmDatabaseSchema"
cohortDatabaseSchema <- "cohortDatabaseSchema"

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

# 2.sample cohort table create
createCohort <- T # Create sample cohort table for test
generateCohort <- T
cohortTable <- "cohortTable"
targetCohortId <- 1234
DiganosisConceptID <- '46273463' # Influenza ConceptID

CreateCohort(createCohort,
             generateCohort,
             connection,
             cohortDatabaseSchema,
             cohortTable,
             DiganosisConceptID)

# Load Cohort table
Cohort_dg <- loadCohortTable_dg(connection,
                                cohortDatabaseSchema,
                                cohortTable)

Cohort_v <- loadCohortTable_v(connection,
                              cohortDatabaseSchema,
                              cohortTable)

Cohort_c <- loadCohortTable_c(connection,
                              cohortDatabaseSchema,
                              cohortTable)

Cohort_p <- loadCohortTable_p(connection,
                              cohortDatabaseSchema,
                              cohortTable)


# # 3. Run APP
runShinyApp()

