cohortDefinitionsModule <- function(id, cohortSubset, dataSource, database) {
  ns <- shiny::NS(id)

  choicesFordatabaseOrVocabularySchema <- list(
    'From site' = database$databaseIdWithVocabularyVersion,
    'Reference Vocabulary' = vocabularyDatabaseSchemas
  )

  cohortDefinitionServer <- function(input, output, session) {

    cohortDefinitionTableData <- shiny::reactive(x = {
      data <- cohortSubset() %>%
        dplyr::select(cohort = .data$shortName, .data$cohortId, .data$cohortName)
      return(data)
    })

    # Cohort Definition ---------------------------------------------------------
    output$cohortDefinitionTable <-
      reactable::renderReactable(expr = {
        data <- cohortDefinitionTableData() %>%
          dplyr::mutate(cohortId = as.character(.data$cohortId))

        validate(need(hasData(data), "There is no data for this cohort."))
        keyColumns <- c("cohort", "cohortId", "cohortName")
        dataColumns <- c()

        displayTable <- getDisplayTableSimple(
          data = data,
          keyColumns = keyColumns,
          dataColumns = dataColumns,
          selection = "single"
        )
        return(displayTable)
      })

    selectedCohortDefinitionRow <- reactive({
      idx <- reactable::getReactableState("cohortDefinitionTable", "selected")
      if (is.null(idx)) {
        return(NULL)
      } else {
        subset <- cohortSubset()
        if (nrow(subset) == 0) {
          return(NULL)
        }
        row <- subset[idx[1],]
        return(row)
      }
    })

    output$cohortDefinitionRowIsSelected <- reactive({
      return(!is.null(selectedCohortDefinitionRow()))
    })

    outputOptions(output,
                  "cohortDefinitionRowIsSelected",
                  suspendWhenHidden = FALSE)

    ## cohortDetailsText ---------------------------------------------------------
    output$cohortDetailsText <- shiny::renderUI({
      row <- selectedCohortDefinitionRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        tags$table(
          style = "margin-top: 5px;",
          tags$tr(
            tags$td(tags$strong("Cohort ID: ")),
            tags$td(HTML("&nbsp;&nbsp;")),
            tags$td(row$cohortId)
          ),
          tags$tr(
            tags$td(tags$strong("Cohort Name: ")),
            tags$td(HTML("&nbsp;&nbsp;")),
            tags$td(row$cohortName)
          )
        )
      }
    })

    ## cohortCountsTableInCohortDefinition ---------------------------------------------------------
    output$cohortCountsTableInCohortDefinition <-
      reactable::renderReactable(expr = {
        if (is.null(selectedCohortDefinitionRow())) {
          return(NULL)
        }
        data <- cohortCount
        if (!hasData(data)) {
          return(NULL)
        }
        data <- data %>%
          dplyr::filter(.data$cohortId == selectedCohortDefinitionRow()$cohortId) %>%
          dplyr::filter(.data$databaseId %in% database$databaseId) %>%
          dplyr::select(.data$databaseId,
                        .data$cohortSubjects,
                        .data$cohortEntries) %>%
          dplyr::rename("persons" = .data$cohortSubjects,
                        "events" = .data$cohortEntries)

        validate(need(hasData(data), "There is no data for this cohort."))

        keyColumns <- c("databaseId")
        dataColumns <- c("persons", "events")

        displayTable <- getDisplayTableSimple(data = data,
                                              keyColumns = keyColumns,
                                              dataColumns = dataColumns)
        return(displayTable)
      })

    ## cohortDefinitionCirceRDetails ---------------------------------------------------------
    cohortDefinitionCirceRDetails <- shiny::reactive(x = {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Rendering human readable cohort description using CirceR (may take time)", value = 0)
      data <- selectedCohortDefinitionRow()
      if (!hasData(data)) {
        return(NULL)
      }
      details <-
        getCirceRenderedExpression(
          cohortDefinition = data$json[1] %>% RJSONIO::fromJSON(digits = 23),
          cohortName = data$cohortName[1],
          includeConceptSets = TRUE
        )
      return(details)
    })

    output$cohortDefinitionText <- shiny::renderUI(expr = {
      cohortDefinitionCirceRDetails()$cohortHtmlExpression %>%
        shiny::HTML()
    })
    ## cohortDefinitionJson ---------------------------------------------------------
    output$cohortDefinitionJson <- shiny::renderText({
      row <- selectedCohortDefinitionRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        row$json
      }
    })

    ## cohortDefinitionSql ---------------------------------------------------------
    output$cohortDefinitionSql <- shiny::renderText({
      row <- selectedCohortDefinitionRow()
      if (is.null(row)) {
        return(NULL)
      } else {
        row$sql
      }
    })

    ## cohortDefinitionConceptSetExpression ---------------------------------------------------------
    cohortDefinitionConceptSetExpression <- shiny::reactive({
      row <- selectedCohortDefinitionRow()
      if (is.null(row)) {
        return(NULL)
      }

      expression <- RJSONIO::fromJSON(row$json, digits = 23)
      if (is.null(expression)) {
        return(NULL)
      }
      expression <-
        getConceptSetDetailsFromCohortDefinition(cohortDefinitionExpression = expression)

      return(expression)
    })

    output$conceptsetExpressionsInCohort <- reactable::renderReactable(expr = {
      data <- cohortDefinitionConceptSetExpression()
      if (is.null(data)) {
        return(NULL)
      }
      if (!is.null(data$conceptSetExpression) &&
        nrow(data$conceptSetExpression) > 0) {
        data <- data$conceptSetExpression %>%
          dplyr::select(.data$id, .data$name)
      } else {
        return(NULL)
      }

      validate(need(
        all(!is.null(data),
            nrow(data) > 0),
        "There is no data for this cohort."
      ))

      keyColumns <- c("id", "name")
      dataColumns <- c()
      getDisplayTableSimple(
        data = data,
        keyColumns = keyColumns,
        dataColumns = dataColumns,
        selection = "single"
      )
    })

    ### cohortDefinitionConceptSetExpressionSelected ---------------------------------------------------------
    cohortDefinitionConceptSetExpressionSelected <- shiny::reactive(x = {
      idx <- reactable::getReactableState("conceptsetExpressionsInCohort", "selected")
      if (length(idx) == 0 || is.null(idx)) {
        return(NULL)
      }
      if (hasData(cohortDefinitionConceptSetExpression()$conceptSetExpression)) {
        data <-
          cohortDefinitionConceptSetExpression()$conceptSetExpression[idx,]
        if (!is.null(data)) {
          return(data)
        } else {
          return(NULL)
        }
      }
    })

    output$cohortDefinitionConceptSetExpressionRowIsSelected <- shiny::reactive(x = {
      return(!is.null(cohortDefinitionConceptSetExpressionSelected()))
    })

    shiny::outputOptions(x = output,
                         name = "cohortDefinitionConceptSetExpressionRowIsSelected",
                         suspendWhenHidden = FALSE)

    output$isDataSourceEnvironment <- shiny::reactive(x = {
      return(is(dataSource, "environment"))
    })
    shiny::outputOptions(x = output,
                         name = "isDataSourceEnvironment",
                         suspendWhenHidden = FALSE)

    ### cohortDefinitionConceptSetDetails ---------------------------------------------------------
    cohortDefinitionConceptSetDetails <- shiny::reactive(x = {
      if (is.null(cohortDefinitionConceptSetExpressionSelected())) {
        return(NULL)
      }
      data <-
        cohortDefinitionConceptSetExpression()$conceptSetExpressionDetails
      if (!hasData(data)) {
        return(NULL)
      }
      data <- data %>%
        dplyr::filter(.data$id == cohortDefinitionConceptSetExpressionSelected()$id)
      if (!hasData(data)) {
        return(NULL)
      }
      data <- data %>%
        dplyr::select(
          .data$conceptId,
          .data$conceptName,
          .data$isExcluded,
          .data$includeDescendants,
          .data$includeMapped,
          .data$standardConcept,
          .data$invalidReason,
          .data$conceptCode,
          .data$domainId,
          .data$vocabularyId,
          .data$conceptClassId
        )
      return(data)
    })

    output$cohortDefinitionConceptSetDetailsTable <-
      reactable::renderReactable(expr = {
        data <- cohortDefinitionConceptSetDetails()
        validate(need(
          all(!is.null(data),
              nrow(data) > 0),
          "There is no data for this cohort."
        ))
        if (is.null(cohortDefinitionConceptSetDetails())) {
          return(NULL)
        }

        data <- data %>%
          dplyr::rename(exclude = .data$isExcluded,
                        descendants = .data$includeDescendants,
                        mapped = .data$includeMapped,
                        invalid = .data$invalidReason)
        validate(need(
          all(!is.null(data),
              nrow(data) > 0),
          "There is no data for this cohort."
        ))

        keyColumns <- c(
          "conceptId",
          "conceptName",
          "exclude",
          "descendants",
          "mapped",
          "standardConcept",
          "invalid",
          "conceptCode",
          "domainId",
          "vocabularyId",
          "conceptClassId"
        )

        dataColumns <- c()
        getDisplayTableSimple(data = data,
                              keyColumns = keyColumns,
                              dataColumns = dataColumns)

      })

    getDatabaseIdInCohortConceptSet <- shiny::reactive({
      return(database$databaseId[database$databaseIdWithVocabularyVersion == input$databaseOrVocabularySchema])
    })

    ## Cohort Concept Set
    ### getSubjectAndRecordCountForCohortConceptSet ---------------------------------------------------------
    getSubjectAndRecordCountForCohortConceptSet <- shiny::reactive(x = {
      row <- selectedCohortDefinitionRow()

      if (is.null(row) || length(getDatabaseIdInCohortConceptSet()) == 0) {
        return(NULL)
      } else {
        data <- cohortCount %>%
          dplyr::filter(.data$cohortId == row$cohortId) %>%
          dplyr::filter(.data$databaseId == getDatabaseIdInCohortConceptSet()) %>%
          dplyr::select(.data$cohortSubjects, .data$cohortEntries)

        if (nrow(data) == 0) {
          return(NULL)
        } else {
          return(data)
        }
      }
    })

    ### subjectCountInCohortConceptSet ---------------------------------------------------------
    output$subjectCountInCohortConceptSet <- shiny::renderUI({
      row <- getSubjectAndRecordCountForCohortConceptSet()
      if (is.null(row)) {
        return(NULL)
      } else {
        tags$table(
          tags$tr(
            tags$td("Subjects: "),
            tags$td(scales::comma(row$cohortSubjects, accuracy = 1))
          )
        )
      }
    })

    ### recordCountInCohortConceptSet ---------------------------------------------------------
    output$recordCountInCohortConceptSet <- shiny::renderUI({
      row <- getSubjectAndRecordCountForCohortConceptSet()
      if (is.null(row)) {
        return(NULL)
      } else {
        tags$table(
          tags$tr(
            tags$td("Records: "),
            tags$td(scales::comma(row$cohortEntries, accuracy = 1))
          )
        )
      }
    })

    ### getCohortDefinitionResolvedConceptsReactive ---------------------------------------------------------
    getCohortDefinitionResolvedConceptsReactive <-
      shiny::reactive(x = {
        row <- selectedCohortDefinitionRow()
        if (is.null(row)) {
          return(NULL)
        }
        output <-
          resolvedConceptSet(
            dataSource = dataSource,
            databaseIds = database$databaseId,
            cohortId = row$cohortId
          )
        if (!hasData(output)) {
          return(NULL)
        }
        conceptCount <- getCountForConceptIdInCohortReactive()
        output <- output %>%
          dplyr::left_join(conceptCount,
                           by = c("databaseId", "conceptId"))
        return(output)
      })

    output$cohortDefinitionResolvedConceptsTable <-
      reactable::renderReactable(expr = {
        if (input$conceptSetsType != 'Resolved') {
          return(NULL)
        }

        databaseIdToFilter <- database %>%
          dplyr::filter(.data$databaseIdWithVocabularyVersion == input$databaseOrVocabularySchema) %>%
          dplyr::pull(.data$databaseId)
        if (!hasData(databaseIdToFilter)) {
          return(NULL)
        }

        validate(need(
          length(cohortDefinitionConceptSetExpressionSelected()$id) > 0,
          "Please select concept set"
        ))

        data <- getCohortDefinitionResolvedConceptsReactive()
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))
        data <- data %>%
          dplyr::filter(.data$conceptSetId == cohortDefinitionConceptSetExpressionSelected()$id) %>%
          dplyr::filter(.data$databaseId == databaseIdToFilter) %>%
          dplyr::rename("subjects" = .data$conceptSubjects,
                        "count" = .data$conceptCount)
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))
        keyColumns <- c(
          "conceptId",
          "conceptName",
          "domainId",
          "vocabularyId",
          "conceptClassId",
          "standardConcept",
          "conceptCode"
        )
        dataColumns <- c("subjects",
                         "count")
        displayTable <- getDisplayTableSimple(data = data,
                                              keyColumns = keyColumns,
                                              dataColumns = dataColumns)
        return(displayTable)
      })


    ### getCountForConceptIdInCohortReactive ---------------------------------------------------------
    getCountForConceptIdInCohortReactive <-
      shiny::reactive(x = {
        row <- selectedCohortDefinitionRow()
        if (is.null(row)) {
          return(NULL)
        }
        data <- getCountForConceptIdInCohort(
          dataSource = dataSource,
          databaseIds = database$databaseId,
          cohortId = row$cohortId
        )
        return(data)
      })

    ## cohortConceptsetExpressionJson ---------------------------------------------------------
    output$cohortConceptsetExpressionJson <- shiny::renderText({
      if (is.null(cohortDefinitionConceptSetExpressionSelected())) {
        return(NULL)
      }
      json <- cohortDefinitionConceptSetExpressionSelected()$json
      return(json)
    })

    ## Other ---------------------------------------------------------
    ### getConceptSetIds ---------------------------------------------------------
    getConceptSetIds <- shiny::reactive(x = {
      return(conceptSets$conceptSetId[conceptSets$conceptSetName %in% input$conceptSetsSelected])
    })

    ### getCohortDefinitionOrphanConceptsReactive ---------------------------------------------------------
    getCohortDefinitionOrphanConceptsReactive <- shiny::reactive(x = {
      validate(need(
        all(
          !is.null(getDatabaseIdInCohortConceptSet()),
          length(getDatabaseIdInCohortConceptSet()) > 0
        ),
        "Orphan codes are not available for reference vocabulary in this version."
      ))
      if (is.null(row) ||
        length(cohortDefinitionConceptSetExpressionSelected()$name) == 0) {
        return(NULL)
      }
      validate(need(
        length(input$databaseOrVocabularySchema) > 0,
        "No data sources chosen"
      ))
      row <- selectedCohortDefinitionRow()
      if (is.null(row)) {
        return(NULL)
      }
      output <- getOrphanConceptResult(
        dataSource = dataSource,
        databaseIds = database$databaseId,
        cohortId = row$cohortId
      )
      if (!hasData(output)) {
        return(NULL)
      }
      output <- output %>%
        dplyr::anti_join(getCohortDefinitionResolvedConceptsReactive() %>%
                           dplyr::select(.data$conceptId) %>%
                           dplyr::distinct(),
                         by = "conceptId")
      if (!hasData(output)) {
        return(NULL)
      }
      output <- output %>%
        dplyr::anti_join(getCohortDefinitionMappedConceptsReactive() %>%
                           dplyr::select(.data$conceptId) %>%
                           dplyr::distinct(),
                         by = "conceptId")
      if (!hasData(output)) {
        return(NULL)
      }
      output <- output %>%
        dplyr::rename("persons" = .data$conceptSubjects,
                      "records" = .data$conceptCount)
      return(output)
    })

    output$cohortDefinitionOrphanConceptTable <-
      reactable::renderReactable(expr = {
        if (input$conceptSetsType != 'Orphan concepts') {
          return(NULL)
        }
        databaseIdToFilter <- database %>%
          dplyr::filter(.data$databaseIdWithVocabularyVersion == input$databaseOrVocabularySchema) %>%
          dplyr::pull(.data$databaseId)
        if (!hasData(databaseIdToFilter)) {
          return(NULL)
        }
        data <- getCohortDefinitionOrphanConceptsReactive()
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))
        data <- data %>%
          dplyr::filter(.data$conceptSetId == cohortDefinitionConceptSetExpressionSelected()$id) %>%
          dplyr::filter(.data$databaseId == databaseIdToFilter) %>%
          dplyr::rename(
            "subjects" = .data$persons,
            "count" = .data$records,
            "standard" = .data$standardConcept
          )
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))
        keyColumns <-
          c("conceptId",
            "conceptName",
            "vocabularyId",
            "conceptCode",
            "standard")
        dataColumns <- c("subjects",
                         "count")

        displayTable <- getDisplayTableSimple(data = data,
                                              keyColumns = keyColumns,
                                              dataColumns = dataColumns)
        return(displayTable)
      })

    ### getCohortDefinitionMappedConceptsReactive ---------------------------------------------------------
    getCohortDefinitionMappedConceptsReactive <-
      shiny::reactive(x = {
        row <- selectedCohortDefinitionRow()
        if (is.null(row)) {
          return(NULL)
        }
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Getting concepts mapped to concept ids resolved by concept set expression (may take time)", value = 0)
        output <-
          mappedConceptSet(
            dataSource = dataSource,
            databaseIds = database$databaseId,
            cohortId = row$cohortId
          )
        if (!hasData(output)) {
          return(NULL)
        }
        conceptCount <- getCountForConceptIdInCohortReactive()
        output <- output %>%
          dplyr::left_join(conceptCount,
                           by = c("databaseId", "conceptId"))
        return(output)
      })

    output$cohortDefinitionMappedConceptsTable <-
      reactable::renderReactable(expr = {
        if (input$conceptSetsType != 'Mapped') {
          return(NULL)
        }

        databaseIdToFilter <- database %>%
          dplyr::filter(.data$databaseIdWithVocabularyVersion == input$databaseOrVocabularySchema) %>%
          dplyr::pull(.data$databaseId)
        if (!hasData(databaseIdToFilter)) {
          return(NULL)
        }

        validate(need(
          length(cohortDefinitionConceptSetExpressionSelected()$id) > 0,
          "Please select concept set"
        ))

        data <- getCohortDefinitionMappedConceptsReactive()
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))

        data <- data %>%
          dplyr::filter(.data$conceptSetId == cohortDefinitionConceptSetExpressionSelected()$id) %>%
          dplyr::filter(.data$databaseId == databaseIdToFilter) %>%
          dplyr::rename("subjects" = .data$conceptSubjects,
                        "count" = .data$conceptCount)
        validate(need(
          hasData(data),
          paste0("No data for database id ", input$databaseOrVocabularySchema)
        ))

        keyColumns <- c(
          "resolvedConceptId",
          "conceptId",
          "conceptName",
          "domainId",
          "vocabularyId",
          "conceptClassId",
          "standardConcept",
          "conceptCode"
        )
        dataColumns <- c("subjects",
                         "count")

        getDisplayTableSimple(data = data,
                              keyColumns = keyColumns,
                              dataColumns = dataColumns)

      })

    output$databasePicker <- shiny::renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("databaseOrVocabularySchema"),
        label = "Vocabulary version choices:",
        choices = choicesFordatabaseOrVocabularySchema,
        multiple = FALSE,
        width = 200,
        inline = TRUE,
        choicesOpt = list(style = rep_len("color: black;", 999)),
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          liveSearchStyle = "contains",
          liveSearchPlaceholder = "Type here to search",
          virtualScroll = 50
        )
      )
    })

  }

  shiny::moduleServer(id, cohortDefinitionServer)
}

cohortDefinitionsUi <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    shinydashboard::box(
      width = NULL,
      status = "primary",
      htmltools::withTags(
        table(width = "100%",
              tr(
                td(align = "left",
                   h4("Cohort Definition")
                ),
                td(align = "right",
                   button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortDefinitionTable')}')"))
                )
              )
        )
      ),
      shiny::column(12,
                    reactable::reactableOutput(outputId = ns("cohortDefinitionTable"))),
      shiny::column(
        12,
        conditionalPanel(
          "output.cohortDefinitionRowIsSelected == true",
          shiny::tabsetPanel(
            type = "tab",
            shiny::tabPanel(title = "Details",
                            shiny::htmlOutput(ns("cohortDetailsText"))),
            shiny::tabPanel(title = "Cohort Count",
                            tags$br(),
                            htmltools::withTags(
                              table(width = "100%",
                                    tr(
                                      td(align = "right",
                                         button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortCountsTableInCohortDefinition')}')"))
                                      )
                                    )
                              )
                            ),
                            reactable::reactableOutput(outputId = ns("cohortCountsTableInCohortDefinition"))),
            shiny::tabPanel(title = "Cohort definition",
                            copyToClipboardButton(toCopyId = ns("cohortDefinitionText"),
                                                  style = "margin-top: 5px; margin-bottom: 5px;"),
                            shiny::htmlOutput(ns("cohortDefinitionText"))),
            shiny::tabPanel(
              title = "Concept Sets",
              reactable::reactableOutput(outputId = ns("conceptsetExpressionsInCohort")),
              shiny::conditionalPanel(condition = "output.cohortDefinitionConceptSetExpressionRowIsSelected == true",
                                      ns = ns,
                                      tags$table(tags$tr(
                                        tags$td(
                                          shiny::radioButtons(
                                            inputId = ns("conceptSetsType"),
                                            label = "",
                                            choices = c("Concept Set Expression",
                                                        "Resolved",
                                                        "Mapped",
                                                        "Orphan concepts",
                                                        "Json"),
                                            selected = "Concept Set Expression",
                                            inline = TRUE
                                          )
                                        ),
                                        tags$td(
                                          shiny::uiOutput(ns("databasePicker"))
                                        ),
                                        tags$td(
                                          shiny::htmlOutput(ns("subjectCountInCohortConceptSet"))
                                        ),
                                        tags$td(
                                          shiny::htmlOutput(ns("recordCountInCohortConceptSet"))
                                        )
                                      ))),
              shiny::conditionalPanel(
                ns = ns,
                condition = "output.cohortDefinitionConceptSetExpressionRowIsSelected == true &
                  input.conceptSetsType != 'Resolved' &
                  input.conceptSetsType != 'Mapped' &
                  input.conceptSetsType != 'Json' &
                  input.conceptSetsType != 'Orphan concepts'",
                htmltools::withTags(
                  table(width = "100%",
                        tr(
                          td(align = "right",
                             button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortDefinitionConceptSetDetailsTable')}')"))
                          )
                        )
                  )
                ),
                reactable::reactableOutput(outputId = ns("cohortDefinitionConceptSetDetailsTable"))
              ),
              shiny::conditionalPanel(
                ns = ns,
                condition = "input.conceptSetsType == 'Resolved'",
                htmltools::withTags(
                  table(width = "100%",
                        tr(
                          td(align = "right",
                             button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortDefinitionResolvedConceptsTable')}')"))
                          )
                        )
                  )
                ),
                reactable::reactableOutput(outputId = ns("cohortDefinitionResolvedConceptsTable"))
              ),
              shiny::conditionalPanel(
                ns = ns,
                condition = "input.conceptSetsType == 'Mapped'",
                htmltools::withTags(
                  table(width = "100%",
                        tr(
                          td(align = "right",
                             button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortDefinitionMappedConceptsTable')}')"))
                          )
                        )
                  )
                ),
                reactable::reactableOutput(outputId = ns("cohortDefinitionMappedConceptsTable"))
              ),
              shiny::conditionalPanel(
                ns = ns,
                condition = "input.conceptSetsType == 'Orphan concepts'",
                htmltools::withTags(
                  table(width = "100%",
                        tr(
                          td(align = "right",
                             button("Download as CSV", onclick = glue::glue("Reactable.downloadDataCSV('{ns('cohortDefinitionOrphanConceptTable')}')"))
                          )
                        )
                  )
                ),
                reactable::reactableOutput(outputId = ns("cohortDefinitionOrphanConceptTable"))
              ),
              shiny::conditionalPanel(
                condition = "input.conceptSetsType == 'Json'",
                copyToClipboardButton(toCopyId = ns("cohortConceptsetExpressionJson"),
                                      style = "margin-top: 5px; margin-bottom: 5px;"),
                shiny::verbatimTextOutput(outputId = ns("cohortConceptsetExpressionJson")),
                tags$head(
                  tags$style("#cohortConceptsetExpressionJson { max-height:400px};")
                ),
                ns = ns
              )
            ),
            shiny::tabPanel(
              title = "JSON",
              copyToClipboardButton(ns("cohortDefinitionJson"), style = "margin-top: 5px; margin-bottom: 5px;"),
              shiny::verbatimTextOutput(ns("cohortDefinitionJson")),
              tags$head(
                tags$style("#cohortDefinitionJson { max-height:400px};")
              )
            ),
            shiny::tabPanel(
              title = "SQL",
              copyToClipboardButton(ns("cohortDefinitionSql"), style = "margin-top: 5px; margin-bottom: 5px;"),
              shiny::verbatimTextOutput(ns("cohortDefinitionSql")),
              tags$head(
                tags$style("#cohortDefinitionSql { max-height:400px};")
              )
            )
          ),
          ns = ns
        )
      ),
    ))
  ui
}