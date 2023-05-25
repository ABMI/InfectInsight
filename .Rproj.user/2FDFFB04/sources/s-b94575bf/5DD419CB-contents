shinyApp(
  ui <- navbarPage(
    theme = shinythemes::shinytheme('flatly'),
    title = "Infection Dashboard",

    # Cohort Level
    tabPanel("Cohort Level",
             column(6,
                    h3(strong("Demographic information")),
                    column(4,
                           box(plotlyOutput("Sex"),
                               width = NULL,
                               height = "100%")
                           ),
                    column(8,
                           box(plotlyOutput("Age"),
                               width = NULL,
                               height = "100%")
                           ),
                    box(withSpinner(plotlyOutput("PCR")),
                        title = tags$b("PCR"),
                        width = NULL,
                        height = "100%")
                    ),
             column(6,
                    fluidRow(column(3, uiOutput("Confirmed")),
                             column(3, uiOutput("Hospitalisation")),
                             column(3, uiOutput("Discharge")),
                             column(3, uiOutput("Death"))),
                    tags$div(style = "height: 50px;"),
                    fluidRow(tabBox(width = 12,
                      tabPanel(title = tags$b("Region"),
                               radioButtons(inputId = "mapGraph",
                                           label = "",
                                           choices = c("Map", "Regional Graph"),
                                           selected = "Map",
                                           inline = T),
                               withSpinner(uiOutput("tabRegion"))
                               ),
                      tabPanel(title = tags$b("Severity"),
                               fluidRow(box(width = 12,
                                               title = tags$b("Non-invasive ventilation"),
                                            withSpinner(plotlyOutput("NIV",
                                                            height = "300px")))),
                               fluidRow(box(width = 12,
                                               title = tags$b("Invasive mechanical ventilation"),
                                            withSpinner(plotlyOutput("MIV",
                                                            height = "300px")))),
                               fluidRow(box(width = 12,
                                               title = tags$b("Extracorporeal membrane oxygenation"),
                                            withSpinner(plotlyOutput("ECMO",
                                                            height = "300px"))))
                               )
                      )
                      )
           )
    ),

    tabPanel("Individual Level",
             fluidRow(column(width = 2,
                             box(width = 12,
                                 pickerInput("searchSubject",
                                             label = "",
                                             choices = c("select the patient", sort(unique(Cohort_dg$SUBJECT_ID))),
                                             selected = "select the patient",
                                             multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
                                             )
                                 )
                             ),
                      column(width = 1,
                             h4(strong("SEX")),
                             textOutput("isex")),
                      column(width = 1,
                             h4(strong("AGE")),
                             textOutput("iage")),
                      column(width = 2,
                             h4(strong("ADRESS")),
                             textOutput("iadress")),
                      column(width = 4,
                             h4(strong("TRAVEL HISTORY")),
                             textOutput("itravel"))
                      ),
             fluidRow(mainPanel(width = 12,
                                tabsetPanel(type = "tabs", id = "indi_tabs",
                                       tabPanel("Info",
                                                box(title = tags$b("Total Medical Schedule"),
                                                    tags$code("Enter the subject id."),
                                                    withSpinner(plotlyOutput("summary")),
                                                    width = "100%")),
                                       tabPanel("Diagnosis",
                                                box(width = 12,
                                                    title = tags$b("A list of diagnosis history."),
                                                    DT::dataTableOutput("diagnosis"))),
                                       tabPanel("Lab",
                                                column(6,
                                                       box(width = 12,
                                                           title = tags$b("Lab Results"),
                                                           tags$code("Select the wanted Period."),
                                                           tags$h5("Search Period: yyyy-mm-dd to yyyy-mm-dd"),
                                                           fluidRow(
                                                             column(width = 9,
                                                                    dateRangeInput("labDate",
                                                                                   label = NULL,
                                                                                   min = "1994-01-01",
                                                                                   format = "yyyy-mm-dd")),
                                                             column(width = 3,
                                                                    actionButton("labPeriodSearch", "Search"))
                                                             ),
                                                           withSpinner(DT::dataTableOutput("labList"))
                                                           )
                                                       ),
                                                column(6,
                                                       box(width = 12,
                                                           title = tags$b("Trend of Results"),
                                                           tags$code("Enter the concept id."),
                                                           sidebarSearchForm(textId = "labCode",
                                                                             buttonId = "labCodeSearch",
                                                                             icon = shiny::icon("magnifying-glass")),
                                                           withSpinner(plotlyOutput("labGraph"))
                                                           )
                                                       )
                                                ),
                                       tabPanel("Drug",
                                                box(width = 12,
                                                    title = tags$b("Drug list"),
                                                    tags$code("Select the wanted Period."),
                                                    tags$h5("Search Period: yyyy-mm-dd to yyyy-mm-dd"),
                                                    fluidRow(
                                                      column(width = 3,
                                                             dateRangeInput("drugDate",
                                                                            label = NULL,
                                                                            min = "1994-01-01",
                                                                            format = "yyyy-mm-dd")),
                                                      column(width = 9,
                                                             actionButton("drugPeriodSearch",
                                                                          "Search"))
                                                      ),
                                                    withSpinner(DT::dataTableOutput("drugList"))
                                                    )
                                                ),
                                       tabPanel("Report",
                                                box(width = 12,
                                                    title = tags$b("Select the below"),
                                                    tags$code("1) Select the wanted Period."),
                                                    tags$h5("Search Period: yyyy-mm-dd to yyyy-mm-dd"),
                                                    column(width = 9,
                                                           dateRangeInput("reportDate",
                                                                          label = NULL,
                                                                          min = "1994-01-01",
                                                                          format = "yyyy-mm-dd")),
                                                    column(width = 3,
                                                           actionButton("reportPeriodSearch", "Search")),
                                                    withSpinner(DT::dataTableOutput("notelist"))
                                                    ),
                                                box(width = 12,
                                                    tags$code("2) Enter the note id."),
                                                    sidebarSearchForm(textId = "noteCode",
                                                                      buttonId = "noteCodeSearch",
                                                                      icon = shiny::icon("magnifying-glass")),
                                                    withSpinner(uiOutput("reading"))
                                                )
                                                )
                                       )
                              )
                    )
             )

    )


  # Define server logic required to draw a histogram
  ,server <- function(input, output, session) {
    ### reactive ###
    # Cohort Level #
    # Region
    rc_region_G <- reactive({
      filterRegion <- filter(Cohort_dg,
                             Region == input$Region)

      name <- c("total", "case")
      n <- c(length(unique(Cohort_dg$SUBJECT_ID)),
             length(unique(filterRegion$SUBJECT_ID)))
      comparison <- data.frame(name = name, n = n)
      comparison <- comparison %>%
        mutate(pct = n/n[1]*100)
      comparison$pct[1] <- 100-comparison$pct[2]

      colors <- c("#E4E3E4", "#DB4D57")

      ggplot(comparison, aes(x = '', y = pct, fill = name)) +
        geom_bar(stat = 'identity') +
        theme_void() +
        coord_polar(theta = 'y', start = 0, direction = -1, clip = 'off') +
        geom_polygon(aes(x = 0, y = 0, group = 1), size = 5, color = 'white') +
        annotate("text", x = 0, y = 0, label = paste0(round(comparison$pct[2], 1), '%'), size = 10) +
        scale_fill_manual(values = setNames(colors, comparison$name)) +
        theme(legend.position = "none")
      })
    rc_region_T <- reactive({
      filterRegion <- filter(Cohort_dg,
                             Region == input$Region)

      Cumulative_confirmed_cases <- length(unique(filterRegion$SUBJECT_ID))
      Increase_decrease_previousDay <- nrow(filter(filterRegion,
                                                   COHORT_START_DATE == today()))
      Death <- nrow(filter(filterRegion,
                           !is.na(DEATH_DATE) & DEATH_DATE >= COHORT_START_DATE ))
      names <- c("Cumulative confirmed cases",
                 "Increase or decrease from the previous day",
                 "Number of deaths")
      count <- c(Cumulative_confirmed_cases,
                 Increase_decrease_previousDay,
                 Death)

      df_region <- data.frame(Names = names, Count = count)
      colnames(df_region) <- NULL
      print(df_region, row.names = FALSE)
    })


    # Individual Level #
    # side bar
    rc_iage <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)
        subject_information <- filter(Cohort_dg,
                                      SUBJECT_ID == as.numeric(input$searchSubject))
        i_age <- year(Sys.Date())-(subject_information$YEAR_OF_BIRTH)
        return(i_age)
      }
    })
    rc_isex <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)
        subject_information <- filter(Cohort_dg,
                                      SUBJECT_ID == as.numeric(input$searchSubject))
        i_sex <- subject_information$GENDER_SOURCE_VALUE
        return(i_sex)
      }
    })
    rc_iadress <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)
        subject_information <- filter(Cohort_dg,
                                      SUBJECT_ID == as.numeric(input$searchSubject))
        i_adress <- paste0(subject_information$Region, " ", subject_information$CITY)
        return(i_adress)
      }
    })
    # !!!!!!!!!!!!!!여행력 추가되면 변경 필요!!!!!!!!!!
    rc_itravel <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)
        i_travel <- "NA"
        return(i_travel)
      }
    })

    # Info
    rc_smPlotly <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)

        # Visit summary
        subject_v <- filter(Cohort_v,
                          SUBJECT_ID == as.numeric(input$searchSubject))

        subject_v <- subject_v %>%
          select(CONCEPT_NAME, VISIT_START_DATE, VISIT_END_DATE) %>%
          arrange(VISIT_START_DATE) %>%
          mutate(type = "Visit")

      for (i in 1:nrow(subject_v)){
        subject_v$tag[i] <- paste(subject_v$CONCEPT_NAME[i],
                                  paste(subject_v$VISIT_START_DATE[i],
                                        subject_v$VISIT_END_DATE[i],
                                        sep = " ~ "),
                                  sep = ", ")
      }

        subject_v <- subset(subject_v, select = -VISIT_END_DATE)

      setnames(subject_v,
               old = c("VISIT_START_DATE"),
               new = c("Dates")
      )

      # Condition summary
      subject_c <- filter(Cohort_c,
                          SUBJECT_ID == as.numeric(input$searchSubject))

      subject_c <- subject_c %>%
        select(CONCEPT_NAME, CONDITION_START_DATE, CONDITION_END_DATE) %>%
        arrange(CONDITION_START_DATE) %>%
        mutate(type = "Diagnosis")

      t <- subject_c %>%
        distinct(CONDITION_START_DATE)

      for (i in 1:nrow(t)){
        z <- subject_c %>%
          filter(CONDITION_START_DATE == t$CONDITION_START_DATE[i]) %>%
          select(CONCEPT_NAME)

        y <- as.list(z$CONCEPT_NAME)

        if(length(y) <= 5){
          x <- paste(y[1:length(y)], collapse = ", ")
        }else{
          x <- paste(y[1:5], collapse = ", ")
          x <- paste(x, ", lots of condition list omitted..")
        }

        subject_c$CONCEPT_NAME[i] <-  x
      }


      for (i in 1:nrow(subject_c)){
        subject_c$tag[i] <- paste(paste(subject_c$CONDITION_START_DATE[i],
                                        subject_c$CONDITION_END_DATE[i],
                                        sep = " ~ "),
                                  subject_c$CONCEPT_NAME[i],
                                  sep = ", ")
      }

      subject_c <- subset(subject_c, select = -CONDITION_END_DATE)

      setnames(subject_c,
               old = c("CONDITION_START_DATE"),
               new = c("Dates")
      )

      # measurement summary
      sql_m <- "SELECT a.cohort_definition_id, a.subject_id, a.cohort_start_date, b.measurement_concept_id, c.concept_name, b.measurement_date
             FROM @cohort_database_schema.@cohort_table a
             left outer join @cdm_database_schema.measurement b on a.subject_id = b.person_id
             left outer join @cdm_database_schema.CONCEPT c on b.measurement_concept_id = c.concept_id
             where a.subject_id = @subject;"

      sql_m <- SqlRender::render(sql_m,
                                 cohort_database_schema = cohortDatabaseSchema,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 cohort_table = cohortTable,
                                 subject = subject_id)

      subject_m <- as.data.frame(DatabaseConnector::querySql(connection, sql_m))


      subject_m <- subject_m %>%
        select(CONCEPT_NAME, MEASUREMENT_DATE) %>%
        arrange(MEASUREMENT_DATE) %>%
        mutate(type = "Lab")

      t <- subject_m %>%
        distinct(MEASUREMENT_DATE)

      for (i in 1:nrow(t)){
        z <- subject_m %>%
          filter(MEASUREMENT_DATE == t$MEASUREMENT_DATE[i]) %>%
          select(CONCEPT_NAME)

        y <- as.list(z$CONCEPT_NAME)

        if(length(y) <= 5){
          x <- paste(y[1:length(y)], collapse = ", ")
        }else{
          x <- paste(y[1:5], collapse = ", ")
          x <- paste(x, ", lots of lab list omitted..")
        }

        subject_m$CONCEPT_NAME[i] <-  x
      }

      subject_m <- subject_m %>%
        distinct(MEASUREMENT_DATE, .keep_all = TRUE)

      subject_m$tag <- subject_m$CONCEPT_NAME

      setnames(subject_m,
               old = "MEASUREMENT_DATE",
               new = "Dates"
      )

      # drug summary
      sql_d <- "SELECT a.cohort_definition_id, a.subject_id, a.cohort_start_date, b.drug_concept_id, c.concept_name, b.drug_concept_id, b.drug_exposure_start_date, b.drug_exposure_end_date
             FROM @cohort_database_schema.@cohort_table a
             left outer join @cdm_database_schema.drug_exposure b on a.subject_id = b.person_id
             left outer join @cdm_database_schema.CONCEPT c on b.drug_concept_id = c.concept_id
             where a.subject_id = @subject;"

      sql_d <- SqlRender::render(sql_d,
                                 cohort_database_schema = cohortDatabaseSchema,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 cohort_table = cohortTable,
                                 subject = subject_id)

      subject_d <- as.data.frame(DatabaseConnector::querySql(connection, sql_d))


      subject_d <- subject_d %>%
        select(CONCEPT_NAME, DRUG_EXPOSURE_START_DATE) %>%
        arrange(DRUG_EXPOSURE_START_DATE) %>%
        mutate(type = "Drug")

      t <- subject_d %>%
        distinct(DRUG_EXPOSURE_START_DATE)

      for (i in 1:nrow(t)){
        z <- subject_d %>%
          filter(DRUG_EXPOSURE_START_DATE == t$DRUG_EXPOSURE_START_DATE[i]) %>%
          select(CONCEPT_NAME)

        y <- as.list(z$CONCEPT_NAME)

        if(length(y) <= 5){
          x <- paste(y[1:length(y)], collapse = ", ")
        }else{
          x <- paste(y[1:5], collapse = ", ")
          x <- paste(x, ", lots of drug list omitted..")
        }

        subject_d$CONCEPT_NAME[i] <-  x
      }

      subject_d <- subject_d %>%
        distinct(DRUG_EXPOSURE_START_DATE, .keep_all = TRUE)

      subject_d$tag <- subject_d$CONCEPT_NAME

      setnames(subject_d,
               old = "DRUG_EXPOSURE_START_DATE",
               new = "Dates"
      )

      # Observation summary
      sql_o <- "SELECT a.cohort_definition_id, a.subject_id, a.cohort_start_date, b.observation_concept_id, c.concept_name, b.observation_date
             FROM @cohort_database_schema.@cohort_table a
             left outer join @cdm_database_schema.observation b on a.subject_id = b.person_id
             left outer join @cdm_database_schema.CONCEPT c on b.observation_concept_id = c.concept_id
             where a.subject_id = @subject;"

      sql_o <- SqlRender::render(sql_o,
                                 cohort_database_schema = cohortDatabaseSchema,
                                 cdm_database_schema = cdmDatabaseSchema,
                                 cohort_table = cohortTable,
                                 subject = subject_id)

      subject_o <- as.data.frame(DatabaseConnector::querySql(connection, sql_o))


      subject_o <- subject_o %>%
        select(CONCEPT_NAME, OBSERVATION_DATE) %>%
        arrange(OBSERVATION_DATE) %>%
        mutate(type = "Observation")

      t <- subject_o %>%
        distinct(OBSERVATION_DATE)

      for (i in 1:nrow(t)){
        z <- subject_o %>%
          filter(OBSERVATION_DATE == t$OBSERVATION_DATE[i]) %>%
          select(CONCEPT_NAME)

        y <- as.list(z$CONCEPT_NAME)

        if(length(y) <= 5){
          x <- paste(y[1:length(y)], collapse = ", ")
        }else{
          x <- paste(y[1:5], collapse = ", ")
          x <- paste(x, ", lots of observation list omitted..")
          x <- paste(y, collapse = ", ")
        }

        subject_o$CONCEPT_NAME[i] <-  x
      }

      subject_o <- subject_o %>%
        distinct(OBSERVATION_DATE, .keep_all = TRUE)

      subject_o$tag <- subject_o$CONCEPT_NAME

      setnames(subject_o,
               old = "OBSERVATION_DATE",
               new = "Dates"
      )


      # binding summaries
      row <- data.frame(
        CONCEPT_NAME = c(NA, NA, NA, NA, NA),
        Dates = c(NA, NA, NA, NA, NA),
        type = c("Visit", "Diagnosis", "Lab", "Observation", "Drug"),
        tag = c(NA, NA, NA, NA, NA),
        stringsAsFactors = FALSE
      )

      bindsm <- rbind(subject_v, subject_c, subject_m, subject_o, subject_d, row)
      # Define the factor levels in the desired order
      factor_levels <- c("Observation", "Drug", "Lab", "Diagnosis", "Visit")

      # Convert the 'type' column to a factor with the desired levels
      bindsm$type <- factor(bindsm$type, levels = factor_levels)



      # Graph
      fig <- plot_ly(bindsm,
                     x = ~Dates,
                     y = ~type,
                     color = ~type,
                     text = ~tag,
                     type = "scatter",
                     mode = "markers",
                     marker = list(size = 20)) %>%
        layout(xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "date")))
      }
    })

    # Diagnosis
    rc_diagnosis <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)

        sql_cond <- "SELECT distinct a.subject_id, i.condition_concept_id, z.concept_name, i.condition_era_start_date, i.condition_era_end_date
                     FROM @cohort_database_schema.@cohort_table a
                     left outer join @cdm_database_schema.condition_era i on a.subject_id = i.person_id
                     left outer join @cdm_database_schema.CONCEPT z on condition_concept_id = z.concept_id
                     where a.subject_id = @subjectID;"

        sql_cond <- SqlRender::render(sql_cond,
                                     cohort_database_schema = cohortDatabaseSchema,
                                     cdm_database_schema = cdmDatabaseSchema,
                                     cohort_table = cohortTable,
                                     subjectID = subject_id)

        df_cond <- as.data.frame(DatabaseConnector::querySql(connection, sql_cond))

        show_cond <- df_cond %>%
          select(CONDITION_ERA_START_DATE, CONDITION_ERA_END_DATE, CONCEPT_NAME) %>%
          rename(Diagnosis_Start_Date = "CONDITION_ERA_START_DATE",
                 Diagnosis_End_Date = "CONDITION_ERA_END_DATE",
                 Diagnosis_Name = "CONCEPT_NAME") %>%
          arrange(desc(Diagnosis_Start_Date) , Diagnosis_Name)
      }
    })

    # LAB
    rc_labList <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)

      sql_lab <- "SELECT distinct a.subject_id, i.measurement_concept_id, z.concept_name, i.measurement_datetime, i.value_as_number, i.value_as_concept_id, i.unit_concept_id, i.range_low, i.range_high, i.unit_source_value, i.value_source_value
    FROM @cohort_database_schema.@cohort_table a
    left outer join @cdm_database_schema.measurement i on a.subject_id = i.person_id
    left outer join @cdm_database_schema.CONCEPT z on i.measurement_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

      sql_lab <- SqlRender::render(sql_lab,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cdm_database_schema = cdmDatabaseSchema,
                                   cohort_table = cohortTable,
                                   subjectID = subject_id)

      df_measurement <- as.data.frame(DatabaseConnector::querySql(connection, sql_lab))

      labstart <- as.Date(as.character(input$labDate[1]))
      labend <- as.Date(as.character(input$labDate[2]))

      FureTestLab <- df_measurement %>%
        filter(MEASUREMENT_DATETIME >= labstart & MEASUREMENT_DATETIME <= labend) %>%
        ungroup() %>%
        distinct(MEASUREMENT_CONCEPT_ID,
                 VALUE_AS_NUMBER,
                 VALUE_AS_CONCEPT_ID,
                 .keep_all = TRUE) %>%
        filter(MEASUREMENT_CONCEPT_ID != 0)

      FureTestLab <- FureTestLab %>%
        filter(!is.na(VALUE_AS_NUMBER))

      if(is.numeric(FureTestLab$VALUE_AS_NUMBER)){
        FureTestLab$VALUE_AS_NUMBER <- round(FureTestLab$VALUE_AS_NUMBER, digits = 3)
      }

      FureTestLab$Range <- paste(paste(round(FureTestLab$RANGE_LOW, digits = 2),
                                       round(FureTestLab$RANGE_HIGH, digits = 2),
                                       sep = " ~ "),
                                 FureTestLab$UNIT_SOURCE_VALUE,
                                 sep = " ")

      FureTestLab$VALUE_AS_NUMBER <- ifelse(is.na(FureTestLab$VALUE_AS_NUMBER),
                                            FureTestLab$VALUE_SOURCE_VALUE,
                                            FureTestLab$VALUE_AS_NUMBER)

      filteredLabDate <- as.data.frame(FureTestLab %>%
                                         select(MEASUREMENT_CONCEPT_ID,
                                                MEASUREMENT_DATETIME  ,
                                                CONCEPT_NAME,
                                                VALUE_AS_NUMBER,
                                                Range) %>%
                                         arrange(desc(MEASUREMENT_DATETIME), CONCEPT_NAME)
      )
      setnames(filteredLabDate,
               old = c("MEASUREMENT_CONCEPT_ID", "VALUE_AS_NUMBER", "MEASUREMENT_DATETIME"),
               new = c("CONCEPT_ID", "Result", "Date_Time")
      )

      labData <- as.data.frame(filteredLabDate)
      }
    })
    rc_labGraph <- reactive({
      subject_id <- as.numeric(input$searchSubject)
      wantLab <- as.numeric(input$labCode)

      sql_lab <- "SELECT distinct a.subject_id, i.measurement_concept_id, z.concept_name, i.MEASUREMENT_DATETIME, i.value_as_number, i.value_as_concept_id, i.unit_concept_id, i.range_low, i.range_high, i.unit_source_value, i.value_source_value
    FROM @cohort_database_schema.@cohort_table a
    inner join @cdm_database_schema.measurement i on a.subject_id = i.person_id and a.subject_id = @subjectID
    inner join @cdm_database_schema.CONCEPT z on i.measurement_concept_id = z.concept_id and i.measurement_concept_id = @wantLab;"

      sql_lab <- SqlRender::render(sql_lab,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cdm_database_schema = cdmDatabaseSchema,
                                   cohort_table = cohortTable,
                                   subjectID = subject_id,
                                   wantLab = wantLab)

      df_measurement <- as.data.frame(DatabaseConnector::querySql(connection, sql_lab))

      FureTestLab <- df_measurement %>%
        arrange(MEASUREMENT_DATETIME) %>%
        distinct(MEASUREMENT_CONCEPT_ID,
                 MEASUREMENT_DATETIME,
                 VALUE_AS_NUMBER,
                 VALUE_AS_CONCEPT_ID,
                 .keep_all = TRUE)

      FureTestLab$Range <- paste("Normal Range",
                                 paste(paste(round(FureTestLab$RANGE_LOW, digits = 2),
                                             round(FureTestLab$RANGE_HIGH, digits = 2),
                                             sep = " ~ "),
                                       FureTestLab$UNIT_SOURCE_VALUE,
                                       sep = " ")
      )

      FureTestLab$VALUE_AS_NUMBER <- ifelse(is.na(FureTestLab$VALUE_AS_NUMBER),
                                            FureTestLab$VALUE_SOURCE_VALUE,
                                            FureTestLab$VALUE_AS_NUMBER)
      reArrange <- FureTestLab %>%
        distinct(MEASUREMENT_DATETIME, .keep_all = TRUE) %>%
        arrange(MEASUREMENT_DATETIME)

      setnames(reArrange,
               old = c("", "VALUE_AS_NUMBER"),
               new = c("Date", "Result"))

      GraphLab <- plot_ly(reArrange,
                          x = ~Date,
                          y = ~Result,
                          text = ~Range,
                          color = ~Result,
                          type = "scatter",
                          mode = "lines",
                          marker = list(size = 20)) %>%
        layout(xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "date")))

      return(GraphLab)
    })

    # Drug
    rc_druglist <- reactive({
      subject_id <- as.numeric(input$searchSubject)

      sql_drug <- "SELECT distinct a.subject_id, f.drug_concept_id, z.concept_name, f.drug_era_start_date, f.drug_era_end_date
    FROM @cohort_database_schema.@cohort_table a
    inner join @cdm_database_schema.drug_era f on a.subject_id = f.person_id
    inner join @cdm_database_schema.CONCEPT z on f.drug_concept_id = z.concept_id
    where a.subject_id = @subjectID;"

      sql_drug <- SqlRender::render(sql_drug,
                                    cohort_database_schema = cohortDatabaseSchema,
                                    cdm_database_schema = cdmDatabaseSchema,
                                    cohort_table = cohortTable,
                                    subjectID = subject_id)

      df_drug <- as.data.frame(DatabaseConnector::querySql(connection, sql_drug))

      drugstart <- as.Date(as.character(input$drugDate[1]))
      drugend <- as.Date(as.character(input$drugDate[2]))

      filterSubject <- df_drug %>%
        filter(DRUG_ERA_START_DATE >= drugstart & DRUG_ERA_END_DATE <= drugend) %>%
        ungroup() %>%
        select(DRUG_CONCEPT_ID,
               CONCEPT_NAME,
               DRUG_ERA_START_DATE,
               DRUG_ERA_END_DATE) %>%
        arrange(desc(DRUG_ERA_START_DATE), desc(DRUG_ERA_END_DATE), CONCEPT_NAME)

      setnames(filterSubject,
               old = c("DRUG_CONCEPT_ID",
                       "CONCEPT_NAME",
                       "DRUG_ERA_START_DATE",
                       "DRUG_ERA_END_DATE"),
               new = c("CONCEPT_ID",
                       "DrugName",
                       "StartDate",
                       "EndDate")
               )


      drugData <- as.data.frame(filterSubject)
      datatable(
        drugData,
        selection = "single"
      )
    })

    # Observation
    rc_reportList <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)

        sql_note <- "SELECT distinct a.subject_id, i.note_id, i.note_date, i.note_type_concept_id, i.note_title, i.note_text
                     FROM @cdm_database_schema.note i
                     left join @cohort_database_schema.@cohort_table a on a.subject_id = i.person_id
                     where i.person_id = @subjectID;"

        sql_note <- SqlRender::render(sql_note,
                                      cohort_database_schema = cohortDatabaseSchema,
                                      cdm_database_schema = cdmDatabaseSchema,
                                      cohort_table = cohortTable,
                                      subjectID = subject_id)

        df_note <- as.data.frame(DatabaseConnector::querySql(connection, sql_note))


        # filter date
        notestart <- as.Date(as.character(input$reportDate[1]))
        noteend <- as.Date(as.character(input$reportDate[2]))

        df_note <- df_note %>%
          filter(NOTE_DATE >= notestart & NOTE_DATE<= noteend) %>%
          ungroup()

        show_note <- df_note %>%
          select(NOTE_ID, NOTE_DATE, NOTE_TITLE) %>%
          arrange(desc(NOTE_DATE), NOTE_TITLE)

      }
    })
    rc_reportText <- reactive({
      if(input$searchSubject == "select the patient"){
        NULL

      }else{
        subject_id <- as.numeric(input$searchSubject)
        wantNote <- as.numeric(input$noteCode)

        sql_note <- "SELECT distinct i.note_text
                     FROM @cdm_database_schema.note i
                     left join @cohort_database_schema.@cohort_table a on a.subject_id = i.person_id
                     where i.person_id = @subjectID and i.note_id = @note_ID;"

        sql_note <- SqlRender::render(sql_note,
                                      cohort_database_schema = cohortDatabaseSchema,
                                      cdm_database_schema = cdmDatabaseSchema,
                                      cohort_table = cohortTable,
                                      subjectID = subject_id,
                                      note_ID = wantNote)

        df_note <- DatabaseConnector::querySql(connection, sql_note)

        show_note <- as.character(df_note$NOTE_TEXT)
      }
    })


    ### render ###
    # Cohort Level #
    # Demography
    output$Sex <- renderPlotly(sexProportionGraph())
    output$Age <- renderPlotly(ageProportionGraph())

    # PCR
    output$PCR <- renderPlotly({

      PCR_concpet_id <- '3043891, 3045856, 586526'
      # 3043891 Influenza virus A Ag [Presence] in Nose
      # 3045856 Influenza virus B Ag [Presence] in Nose
      # 586526 SARS-CoV-2 (COVID-19) RNA [Presence] in Nasopharynx by NAA with probe detection

      sql_PCR <- "SELECT distinct a.subject_id, b.measurement_concept_id, c.concept_name, b.measurement_date, b.value_as_concept_id
    FROM @cohort_database_schema.@cohort_table a
    left join @cdm_database_schema.measurement b on a.subject_id = b.person_id
    left join @cdm_database_schema.CONCEPT c on b.measurement_concept_id = c.concept_id
    where b.measurement_concept_id in ( @PCR_concpet_id )"

      sql_PCR <- SqlRender::render(sql_PCR,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cdm_database_schema = cdmDatabaseSchema,
                                   cohort_table = cohortTable,
                                   PCR_concpet_id = PCR_concpet_id)

      df_PCR <- as.data.frame(DatabaseConnector::querySql(connection, sql_PCR))
      df_PCR$Result <- ifelse(df_PCR$VALUE_AS_CONCEPT_ID == 9189, "Negative", "Positive")

      FureTestPCR <- df_PCR %>%
        arrange(MEASUREMENT_DATE, MEASUREMENT_CONCEPT_ID) %>%
        distinct(SUBJECT_ID ,
                 MEASUREMENT_CONCEPT_ID,
                 MEASUREMENT_DATE,
                 VALUE_AS_CONCEPT_ID,
                 .keep_all = TRUE)

      setnames(FureTestPCR,
               old = c("MEASUREMENT_DATE", "CONCEPT_NAME"),
               new = c("Date", "Test"))


      # Graph
      GraphPCR <- FureTestPCR %>%
        filter(Date >= Sys.Date() - years(5)) %>%
        count(Date, Test) %>%
        plot_ly(x = ~Date,
                y = ~n,
                color = ~Test,
                type = "bar") %>%
        layout(xaxis = list(
          title = "DATE",
          range = list(as.Date(Sys.time())-months(1), as.Date(Sys.time())),
          rangeselector = list(
            buttons = list(
              list(count = 7, label = "1 week", step = "day", stepmode = "backward"),
              list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
              list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
              list(step = "all")
            )
          ),
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "Number of tests"),
        legend = list(x = 0.5, y = -1.5, xanchor = "center", yanchor = "bottom"))
    })

    # Treatment status
    output$Confirmed <- renderUI({
      text <- paste0(length(unique(Cohort_dg$SUBJECT_ID)),
                     " (+ ",
                     nrow(filter(Cohort_dg,
                                 COHORT_START_DATE == today())),
                     ")"
                     )
      text <- as.vector(text)

      summaryBox2(title = tags$b("Confirmed"),
                  value = text,
                  width = NULL,
                  style = "primary",
                  icon = NULL)
    })
    output$Hospitalisation <- renderUI({
      # careSite <- c(193,691,15,237,349,633,24,827,181,422,196,830,235,689,33,824,191,249,634,765,301,238,177,161,180,819,195,38,178,23,197,766)
      text <- paste0(nrow(filter(Cohort_v,
                                 VISIT_CONCEPT_ID %in% c(262, 9201) & VISIT_START_DATE  >= COHORT_START_DATE)),
                     " (+ ",
                     nrow(filter(Cohort_v,
                                 VISIT_START_DATE == today())),
                     ")"
      )
      text <- as.vector(text)

      summaryBox2(title = tags$b("Hospitalisation"),
                  value = text,
                  width = NULL,
                  style = "warning",
                  icon = NULL)
    })
    output$Discharge <- renderUI({
      # careSite <- c(193,691,15,237,349,633,24,827,181,422,196,830,235,689,33,824,191,249,634,765,301,238,177,161,180,819,195,38,178,23,197,766)
      text <- paste0(nrow(filter(Cohort_v,
                                 VISIT_CONCEPT_ID %in% c(262, 9201) & VISIT_START_DATE  >= COHORT_START_DATE & VISIT_END_DATE <=today())),
                     " (+ ",
                     nrow(filter(Cohort_v,
                                 VISIT_END_DATE == today())),
                     ")"
                     )

      text <- as.vector(text)

      summaryBox2(title = tags$b("Discharge"),
                  value = text,
                  width = NULL,
                  style = "success",
                  icon = NULL)
    })
    output$Death <- renderUI({
      text <- paste0(nrow(filter(Cohort_dg,
                                 !is.na(DEATH_DATE) & DEATH_DATE >= COHORT_START_DATE )),
                     " (+ ",
                     nrow(filter(Cohort_dg,
                                 COHORT_START_DATE == today())),
                     ")"
      )
      text <- as.vector(text)

      summaryBox2(title = tags$b("Death"),
                  value = text,
                  width = NULL,
                  style = "secondary",
                  icon = NULL)
    })


    # TAB 1.Region
    output$Map <- renderUI({
      # patient counts by region
      N_region <- as.data.table(table(Cohort_dg$Region))
      N_region <- rename(N_region, "E_region"="V1")
      N_region$label <- paste0(N_region$Region, " (n = ", N_region$N, ")")

      # map data
      korM <- korpop1 %>% select(행정구역별_읍면동, code)
      korM <- rename(korM, K_region=행정구역별_읍면동)
      korM$E_region <- c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju",
                         "Daejeon", "Ulsan", "Sejong-si", "Gyeonggi-do", "Gangwon-do",
                         "Chungcheongbuk-do", "Chungcheongnam-do", "Jeollabuk-do", "Jellanam-do", "Gyeongsangbuk-do",
                         "Gyeongsangnam-do", "Jeju-do")

      korPop <- left_join(korM, N_region, by = "E_region")
      for(i in 1:nrow(korPop)){
        if(is.na(korPop$N[i])){
          korPop$N[i] <- 0
          korPop$label[i] <- "(n = 0)"}
      }

      # Graph
      K_map <- ggChoropleth(data=korPop,
                            aes(fill= N,
                                map_id = code,
                                tooltip = E_region),
                            map = kormap1,
                            interactive=T)

  })
    output$nationalComparison <- renderPlot({rc_region_G()})
    output$regionStatus <- renderTable({rc_region_T()})
    output$tabRegion <- renderUI({
      if(input$mapGraph == "Map"){
        return(uiOutput("Map"))
      }else if(input$mapGraph == "Regional Graph"){
        return(
          list(pickerInput(inputId = "Region",
                           label = "",
                           choices = sort(unique(Cohort_dg$Region)),
                           selected = "Gyeonggi-do"
                           ),
               column(6,
                      div(style = "text-align:center;",
                          tags$h5(tags$b("Percentage of cases in the region"))
                          ),
                      plotOutput("nationalComparison",
                                 height = "300px")),
               column(6,
                      tags$div(style = "height: 200px;"),
                      tableOutput("regionStatus"))
                      )
          )
        }
      })

    # TAB 2.Severity
    output$NIV <- renderPlotly({
      niv <- filter(Cohort_p,
                    PROCEDURE_DATE >= COHORT_START_DATE & PROCEDURE_CONCEPT_ID  %in% c(4155151, 4239130))
      # 4155151 	Oxygen administration by nasal cannula
      # 4239130 	Oxygen therapy

      # group by date and count N per day
      reArrange <- niv %>%
        group_by(COHORT_START_DATE, PROCEDURE_DATE) %>%
        summarise(N = n()) %>%
        ungroup()

      # plot trend graph
      niv_graph <- plot_ly(reArrange,
                           x = ~COHORT_START_DATE,
                           y = ~N,
                           type = "scatter",
                           mode = "lines") %>%
        layout(xaxis = list(
          title = "DATE",
          range = list(as.Date(Sys.time())-years(3), as.Date(Sys.time())),
          rangeselector = list(
            buttons = list(
              list(count = 7, label = "1 week", step = "day", stepmode = "backward"),
              list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
              list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
              list(step = "all")
            )
          ),
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "Number of people"))

    })
    output$MIV <- renderPlotly({miv <- filter(Cohort_p,
                                              PROCEDURE_DATE >= COHORT_START_DATE & PROCEDURE_CONCEPT_ID  %in% c(4230167, 4262010))
    # 4230167   [Artificial respiration]
    # 4262010   [Tracheostomy care management]

    # group by date and count N per day
    reArrange <- miv %>%
      group_by(COHORT_START_DATE, PROCEDURE_DATE) %>%
      summarise(N = n()) %>%
      ungroup()

    # plot trend graph
    miv_graph <- plot_ly(reArrange,
                         x = ~COHORT_START_DATE,
                         y = ~N,
                         type = "scatter",
                         mode = "lines") %>%
      layout(xaxis = list(
        title = "DATE",
        range = list(as.Date(Sys.time())-years(3), as.Date(Sys.time())),
        rangeselector = list(
          buttons = list(
            list(count = 7, label = "1 week", step = "day", stepmode = "backward"),
            list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
            list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
            list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
            list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
            list(step = "all")
          )
        ),
        rangeslider = list(type = "date")
      ),
      yaxis = list(title = "Number of people"))
    })
    output$ECMO <- renderPlotly({
      ecmo <- filter(Cohort_p,
                     PROCEDURE_DATE >= COHORT_START_DATE & PROCEDURE_CONCEPT_ID  %in% c(4052536))

      # 4052536 Extracorporeal membrane oxygenation

      # group by date and count N per day
      reArrange <- ecmo %>%
        group_by(COHORT_START_DATE, PROCEDURE_DATE) %>%
        summarise(N = n()) %>%
        ungroup()

      # plot trend graph
      ecmo_graph <- plot_ly(reArrange,
                            x = ~COHORT_START_DATE,
                            y = ~N,
                            type = "scatter",
                            mode = "lines") %>%
        layout(xaxis = list(
          title = "DATE",
          range = list(as.Date(Sys.time())-years(3), as.Date(Sys.time())),
          rangeselector = list(
            buttons = list(
              list(count = 7, label = "1 week", step = "day", stepmode = "backward"),
              list(count = 1, label = "1 month", step = "month", stepmode = "backward"),
              list(count = 3, label = "3 months", step = "month", stepmode = "backward"),
              list(count = 6, label = "6 months", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 year", step = "year", stepmode = "backward"),
              list(step = "all")
            )
          ),
          rangeslider = list(type = "date")
        ),
        yaxis = list(title = "Number of people"))

    })

    # Individual Level #
    observeEvent(input$searchSubject, {
      # input 값이 변경될 때마다 새로고침
      updateTabsetPanel(session, "indi_tabs")

      # side bar
      output$iage <- eventReactive(input$searchSubject, {rc_iage()})
      output$isex <- eventReactive(input$searchSubject, {rc_isex()})
      output$iadress <- eventReactive(input$searchSubject, {rc_iadress()})
      output$itravel <- eventReactive(input$searchSubject, {rc_itravel()})

      # Info
      B_summary <- eventReactive(input$searchSubject, {rc_smPlotly()})
      output$summary <- renderPlotly(B_summary())

      # Diagnosis
      output$diagnosis <- DT::renderDataTable({rc_diagnosis()})

      # Lab
      B_lablist <- eventReactive(input$labPeriodSearch, {
        lab <- rc_labList()
        lab$Date_Time <- format(
          as.POSIXct(lab$Date_Time, format = "%Y-%m-%d %H:%M:%S"),
          format = "%Y-%m-%d %H:%M:%S"
        )

        lab
        })
      output$labList <- DT::renderDataTable({B_lablist()})
      B_labGraph <- eventReactive(input$labCodeSearch, {rc_labGraph()})
      output$labGraph <- renderPlotly({B_labGraph()})


      # Drug
      B_drugperiod <- eventReactive(input$drugPeriodSearch, {rc_druglist()})
      output$drugList <- DT::renderDataTable({
      if(is.null(input$drugDate)){
        print("Enter the drug Date")
      }else{
      B_drugperiod()}})

      # Observation
      B_notelist <- eventReactive(input$reportPeriodSearch, {
      data <- rc_reportList()
      datatable(data,
                selection = "single",
                callback = DT::JS(
                  "table.on('click.dt', 'tr', function() {",
                  "  var data = table.row(this).data();",
                  "  Shiny.setInputValue('notelist_rows_selected', data);",
                  "});"
                )
      )
    })
      output$notelist <- DT::renderDataTable({B_notelist()})
      B_noteReading <- eventReactive(input$noteCodeSearch, {rc_reportText()})
      output$reading <- renderUI({
      xml_text <- B_noteReading()
      HTML(xml_text)
      })

    })

  }

)
