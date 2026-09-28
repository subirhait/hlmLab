## app.R --- hlmLab 0.2.0 interactive teaching laboratory
##
## Public deployment entry point. The app uses simulated data by default and
## accepts optional CSV uploads. Uploaded data remain in the active Shiny
## session and are not written to disk by this application.

options(shiny.maxRequestSize = 20 * 1024^2)

required_packages <- c("shiny", "hlmLab", "lme4", "dplyr", "ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this app: ",
    paste(missing_packages, collapse = ", ")
  )
}

if (utils::packageVersion("hlmLab") < "0.2.0") {
  stop("This app requires hlmLab 0.2.0 or later.")
}

suppressPackageStartupMessages({
  library(shiny)
  library(hlmLab)
  library(lme4)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

app_colors <- c(
  blue = "#326E99",
  light_blue = "#BCD2DF",
  orange = "#D97706",
  charcoal = "#1F2937",
  gray = "#6B7280"
)

simulate_hlm_data <- function(
    n_clusters,
    cluster_size,
    within_slope,
    contextual_effect,
    cross_level_effect,
    intercept_sd,
    slope_sd,
    residual_sd,
    seed) {
  set.seed(seed)

  n <- n_clusters * cluster_size
  child_id <- sprintf("C%05d", seq_len(n))
  school_id <- factor(rep(seq_len(n_clusters), each = cluster_size))
  school_index <- as.integer(school_id)

  school_resources_j <- as.numeric(scale(stats::rnorm(n_clusters)))
  ses_mean_j <- 0.35 * school_resources_j + stats::rnorm(n_clusters, sd = 0.70)
  u0_j <- stats::rnorm(n_clusters, sd = intercept_sd)
  u1_j <- stats::rnorm(n_clusters, sd = slope_sd)

  ses_within <- stats::rnorm(n)
  ses <- ses_mean_j[school_index] + ses_within
  school_resources <- school_resources_j[school_index]
  between_slope <- within_slope + contextual_effect

  math_score <-
    50 +
    within_slope * ses_within +
    between_slope * ses_mean_j[school_index] +
    2 * school_resources +
    cross_level_effect * ses_within * school_resources +
    u0_j[school_index] +
    u1_j[school_index] * ses_within +
    stats::rnorm(n, sd = residual_sd)

  data.frame(
    child_id = child_id,
    school_id = school_id,
    school_resources = school_resources,
    SES = ses,
    math_score = math_score,
    stringsAsFactors = FALSE
  )
}

simulate_longitudinal_data <- function(
    n_clusters,
    children_per_cluster,
    n_waves,
    growth_rate,
    seed) {
  set.seed(seed)

  n_children <- n_clusters * children_per_cluster
  child_index <- seq_len(n_children)
  school_index_child <- rep(seq_len(n_clusters), each = children_per_cluster)
  child_id_child <- sprintf(
    "S%02d_C%03d",
    school_index_child,
    stats::ave(child_index, school_index_child, FUN = seq_along)
  )

  school_resources_j <- as.numeric(scale(stats::rnorm(n_clusters)))
  school_intercept_j <- stats::rnorm(n_clusters, sd = 5)
  child_intercept_i <- stats::rnorm(n_children, sd = 4)
  ses_i <- 0.35 * school_resources_j[school_index_child] + stats::rnorm(n_children)

  school_index <- rep(school_index_child, each = n_waves)
  child_index_long <- rep(child_index, each = n_waves)
  time <- rep(seq.int(0, n_waves - 1L), times = n_children)
  school_resources <- school_resources_j[school_index]
  SES <- ses_i[child_index_long]

  math_score <-
    42 +
    2.5 * SES +
    growth_rate * time +
    0.15 * time^2 +
    0.35 * SES * time +
    2 * school_resources +
    school_intercept_j[school_index] +
    child_intercept_i[child_index_long] +
    stats::rnorm(length(time), sd = 4)

  data.frame(
    child_id = rep(child_id_child, each = n_waves),
    school_id = factor(school_index),
    time = time,
    school_resources = school_resources,
    SES = SES,
    math_score = math_score,
    stringsAsFactors = FALSE
  )
}

safe_formula <- function(response, terms, random_term) {
  stats::as.formula(
    paste(response, "~", paste(c(terms, random_term), collapse = " + "))
  )
}

cluster_level_variable <- function(data, cluster, variable) {
  counts <- tapply(
    data[[variable]],
    data[[cluster]],
    function(z) length(unique(z[!is.na(z)]))
  )
  all(counts <= 1L)
}

ui <- fluidPage(
  includeCSS("www/styles.css"),

  div(
    class = "app-header",
    div(
      class = "header-copy",
      h1("hlmLab"),
      p("Interactive hierarchical linear modeling laboratory")
    ),
    div(
      class = "header-links",
      tags$a(
        "CRAN",
        href = "https://CRAN.R-project.org/package=hlmLab",
        target = "_blank",
        rel = "noopener noreferrer"
      ),
      tags$a(
        "Source code",
        href = "https://github.com/subirhait/hlmLab",
        target = "_blank",
        rel = "noopener noreferrer"
      )
    )
  ),

  fluidRow(
    column(
      width = 3,
      div(
        class = "control-card",
        h3("1. Choose data"),
        radioButtons(
          "source_type",
          label = NULL,
          choices = c(
            "Simulated two-level data" = "sim",
            "Simulated three-level longitudinal data" = "sim_long",
            "Upload a CSV" = "upload"
          ),
          selected = "sim"
        ),

        conditionalPanel(
          condition = "input.source_type == 'sim'",
          sliderInput("n_clusters", "Schools", 10, 80, 30, step = 1),
          sliderInput("cluster_size_sim", "Students per school", 5, 80, 30, step = 1),
          sliderInput("within_slope", "Within-school slope", -2, 8, 3, step = 0.25),
          sliderInput("contextual_effect", "Contextual effect (between - within)", -4, 8, 4, step = 0.25),
          sliderInput("cross_level_effect", "Cross-level interaction", -3, 3, 1, step = 0.25),
          sliderInput("intercept_sd", "Random-intercept SD", 1, 15, 6, step = 0.5),
          sliderInput("slope_sd", "Random-slope SD", 0, 4, 1.25, step = 0.25),
          sliderInput("residual_sd", "Residual SD", 2, 20, 10, step = 0.5),
          numericInput("sim_seed", "Simulation seed", 2026, min = 1, step = 1)
        ),

        conditionalPanel(
          condition = "input.source_type == 'sim_long'",
          sliderInput("long_n_clusters", "Schools", 5, 40, 20, step = 1),
          sliderInput("long_children", "Children per school", 5, 30, 12, step = 1),
          sliderInput("long_waves", "Waves per child", 3, 8, 5, step = 1),
          sliderInput("long_growth", "Average growth per wave", -2, 5, 1.5, step = 0.25),
          numericInput("long_seed", "Simulation seed", 2027, min = 1, step = 1)
        ),

        conditionalPanel(
          condition = "input.source_type == 'upload'",
          fileInput("file", "CSV file", accept = c(".csv", "text/csv")),
          helpText("Maximum size: 20 MB. Column names are made R-safe after upload."),
          div(
            class = "privacy-note",
            strong("Privacy note: "),
            "use de-identified teaching data only."
          )
        ),

        tags$hr(),
        h3("2. Choose variables"),
        uiOutput("variable_mapping"),

        tags$hr(),
        h3("3. Update results"),
        actionButton("run", "RUN / Update analysis", class = "btn-primary btn-block"),
        uiOutput("run_status")
      )
    ),

    column(
      width = 9,
      div(
        class = "analysis-card",
        tabsetPanel(
          id = "main_tabs",

          tabPanel(
            "Start",
            h2("A decomposition-first workflow"),
            p(
              "Move from raw clustering to variance decomposition, ICC, partial pooling,",
              "contextual effects, random-slope heterogeneity, and observed cross-level moderation."
            ),
            div(
              class = "workflow-grid",
              div(class = "workflow-step", span("1"), strong("Decompose"), p("Separate between- and within-cluster variation.")),
              div(class = "workflow-step", span("2"), strong("Quantify"), p("Estimate the ICC and design effect.")),
              div(class = "workflow-step", span("3"), strong("Pool"), p("See how multilevel estimates shrink raw means.")),
              div(class = "workflow-step", span("4"), strong("Separate"), p("Distinguish within, between, and contextual effects.")),
              div(class = "workflow-step", span("5"), strong("Compare"), p("Contrast random slopes with cross-level interaction."))
            ),
            h3("Data preview"),
            uiOutput("data_message"),
            tableOutput("head_table"),
            p(class = "small-note", "Results update only after you click RUN / Update analysis.")
          ),

          tabPanel(
            "Decomposition",
            h2("Between- and within-cluster variance"),
            p("The descriptive decomposition is the first diagnostic: where does the observed variation live?"),
            verbatimTextOutput("decomposition_text"),
            plotOutput("decomposition_plot", height = "430px")
          ),

          tabPanel(
            "ICC",
            h2("Intraclass correlation and design effect"),
            p("The ICC is the model-based share of outcome variance attributable to cluster differences."),
            verbatimTextOutput("icc_text"),
            plotOutput("icc_plot", height = "380px"),
            tags$hr(),
            h3("What different ICC values look like"),
            fluidRow(
              column(4, numericInput("icc_low", "Low ICC", 0.05, min = 0.01, max = 0.95, step = 0.01)),
              column(4, numericInput("icc_mid", "Moderate ICC", 0.25, min = 0.01, max = 0.95, step = 0.01)),
              column(4, numericInput("icc_high", "High ICC", 0.60, min = 0.01, max = 0.95, step = 0.01))
            ),
            plotOutput("icc_demo_plot", height = "410px")
          ),

          tabPanel(
            "Partial pooling",
            h2("Raw means and multilevel estimates"),
            p("Arrows show how empirical Bayes estimates move cluster means toward the overall mean."),
            verbatimTextOutput("shrinkage_text"),
            plotOutput("shrinkage_plot", height = "560px")
          ),

          tabPanel(
            "Context",
            h2("Within, between, and contextual effects"),
            p("The app group-mean centers the Level-1 predictor and constructs its cluster mean automatically."),
            verbatimTextOutput("context_text"),
            plotOutput("context_plot", height = "430px")
          ),

          tabPanel(
            "Random slopes",
            h2("Unexplained slope heterogeneity"),
            p("Each blue line is a cluster-specific conditional association; the orange line is the average."),
            verbatimTextOutput("random_slope_text"),
            plotOutput("random_slope_plot", height = "500px")
          ),

          tabPanel(
            "Cross-level interaction",
            h2("Observed Level-2 moderation"),
            p("Unlike a random slope, this display attributes systematic slope differences to an observed cluster characteristic."),
            uiOutput("cross_level_note"),
            verbatimTextOutput("cross_level_text"),
            plotOutput("cross_level_plot", height = "470px")
          ),

          tabPanel(
            "Longitudinal",
            h2("Three-level decomposition and growth"),
            p("Use the simulated longitudinal data or upload a long-format CSV, then select the cluster, child/person, and time variables."),
            uiOutput("longitudinal_note"),
            h3("Between-cluster, between-person, and within-person decomposition"),
            verbatimTextOutput("bpw_text"),
            plotOutput("bpw_plot", height = "390px"),
            tags$hr(),
            h3("Observed trajectories"),
            plotOutput("growth_plot", height = "450px")
          ),

          tabPanel(
            "About",
            h2("About this teaching app"),
            p(
              "This app accompanies hlmLab, an R package for visualization and decomposition",
              "in hierarchical linear models. It is intended for classroom demonstration and",
              "exploratory teaching—not for survey-weighted or production inference."
            ),
            tags$ul(
              tags$li("Software: hlmLab 0.2.0 or later"),
              tags$li("Models: lme4::lmer()"),
              tags$li("Public example data: simulated within each session"),
              tags$li("Uploaded CSV files: held only for the active session")
            ),
            p(
              tags$a(
                "Package documentation and installation",
                href = "https://CRAN.R-project.org/package=hlmLab",
                target = "_blank",
                rel = "noopener noreferrer"
              )
            ),
            p(class = "small-note", paste("Running hlmLab", as.character(utils::packageVersion("hlmLab"))))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$file)
    tryCatch(
      {
        d <- utils::read.csv(
          input$file$datapath,
          stringsAsFactors = FALSE,
          check.names = TRUE
        )
        validate(
          need(nrow(d) > 1L, "The CSV must contain at least two rows."),
          need(ncol(d) >= 3L, "The CSV must contain at least three columns.")
        )
        d
      },
      error = function(e) {
        validate(need(FALSE, paste("The CSV could not be read:", conditionMessage(e))))
      }
    )
  })

  source_data <- reactive({
    if (identical(input$source_type, "sim")) {
      simulate_hlm_data(
        n_clusters = input$n_clusters,
        cluster_size = input$cluster_size_sim,
        within_slope = input$within_slope,
        contextual_effect = input$contextual_effect,
        cross_level_effect = input$cross_level_effect,
        intercept_sd = input$intercept_sd,
        slope_sd = input$slope_sd,
        residual_sd = input$residual_sd,
        seed = input$sim_seed
      )
    } else if (identical(input$source_type, "sim_long")) {
      simulate_longitudinal_data(
        n_clusters = input$long_n_clusters,
        children_per_cluster = input$long_children,
        n_waves = input$long_waves,
        growth_rate = input$long_growth,
        seed = input$long_seed
      )
    } else {
      uploaded_data()
    }
  })

  variable_names <- reactive({
    if (identical(input$source_type, "sim")) {
      c("child_id", "school_id", "school_resources", "SES", "math_score")
    } else if (identical(input$source_type, "sim_long")) {
      c("child_id", "school_id", "time", "school_resources", "SES", "math_score")
    } else {
      names(uploaded_data())
    }
  })

  output$variable_mapping <- renderUI({
    cn <- variable_names()
    numeric_candidates <- cn[vapply(source_data()[cn], is.numeric, logical(1))]
    validate(
      need(
        length(numeric_candidates) >= 2L,
        "The data must contain at least two numeric columns for the outcome and Level-1 predictor."
      )
    )
    outcome_default <- if ("math_score" %in% cn) "math_score" else numeric_candidates[1L]
    predictor_default <- if ("SES" %in% cn) "SES" else numeric_candidates[min(2L, length(numeric_candidates))]
    moderator_default <- if ("school_resources" %in% cn) "school_resources" else "None"
    id_candidates <- c(
      "child_id", "childid", "student_id", "studentid",
      "person_id", "personid", "id"
    )
    id_match <- match(id_candidates, tolower(cn), nomatch = 0L)
    person_default <- if (
      identical(input$source_type, "upload") && any(id_match > 0L)
    ) {
      cn[id_match[id_match > 0L][1L]]
    } else {
      "None"
    }
    person_choices <- if (identical(input$source_type, "upload")) {
      c("None", cn)
    } else {
      c("None", "child_id")
    }
    time_choices <- if (identical(input$source_type, "upload")) {
      c("None", cn)
    } else if (identical(input$source_type, "sim_long")) {
      c("None", "time")
    } else {
      "None"
    }

    tagList(
      selectInput(
        "cluster", "Cluster ID",
        choices = cn,
        selected = if ("school_id" %in% cn) "school_id" else cn[1L]
      ),
      selectInput(
        "outcome", "Outcome (numeric)",
        choices = cn,
        selected = outcome_default
      ),
      selectInput(
        "x_l1", "Level-1 predictor (numeric)",
        choices = cn,
        selected = predictor_default
      ),
      selectInput(
        "moderator", "Level-2 moderator",
        choices = c("None", cn),
        selected = moderator_default
      ),
      selectInput(
        "id_long", "Child/person ID (longitudinal)",
        choices = person_choices,
        selected = if (input$source_type %in% c("sim", "sim_long")) "child_id" else person_default
      ),
      selectInput(
        "time_long", "Time variable (optional)",
        choices = time_choices,
        selected = if (input$source_type %in% c("upload", "sim_long") && "time" %in% cn) "time" else "None"
      ),
      if (identical(input$source_type, "sim")) {
        helpText("Each child appears once in the two-level data. Choose the simulated longitudinal data for repeated observations over time.")
      } else if (identical(input$source_type, "sim_long")) {
        helpText("Each child appears at every wave and is nested within one school.")
      }
    )
  })

  output$data_message <- renderUI({
    d <- source_data()
    div(
      class = "data-summary",
      strong(format(nrow(d), big.mark = ",")), " rows and ",
      strong(ncol(d)), " columns are available."
    )
  })

  output$head_table <- renderTable({
    utils::head(source_data(), 8L)
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  analysis <- eventReactive(
    input$run,
    {
      d <- source_data()
      req(input$cluster, input$outcome, input$x_l1)

      validate(
        need(input$cluster %in% names(d), "Select a valid cluster variable."),
        need(input$outcome %in% names(d), "Select a valid outcome."),
        need(input$x_l1 %in% names(d), "Select a valid Level-1 predictor."),
        need(is.numeric(d[[input$outcome]]), "The outcome must be numeric."),
        need(is.numeric(d[[input$x_l1]]), "The Level-1 predictor must be numeric."),
        need(length(unique(d[[input$cluster]])) >= 3L, "At least three clusters are required."),
        need(length(unique(d[[input$cluster]])) < nrow(d), "The cluster ID must repeat across rows.")
      )

      d[[input$cluster]] <- factor(d[[input$cluster]])

      list(
        data = d,
        source_type = input$source_type,
        cluster = input$cluster,
        outcome = input$outcome,
        predictor = input$x_l1,
        moderator = input$moderator,
        person = input$id_long,
        time = input$time_long,
        icc_demo = c(input$icc_low, input$icc_mid, input$icc_high),
        seed = input$sim_seed
      )
    },
    ignoreInit = TRUE
  )

  analysis_ready <- reactive(!is.null(analysis()))

  output$run_status <- renderUI({
    if (input$run < 1L) {
      div(class = "run-status waiting", "Choose variables, then click RUN.")
    } else {
      a <- analysis()
      div(
        class = "run-status ready",
        paste(format(nrow(a$data), big.mark = ","), "rows analyzed")
      )
    }
  })

  observeEvent(input$run, {
    a <- analysis()
    showNotification(
      paste("Analysis updated:", format(nrow(a$data), big.mark = ","), "rows"),
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE)

  decomposition <- reactive({
    req(analysis_ready())
    a <- analysis()
    hlm_decompose(a$data, var = a$outcome, cluster = a$cluster)
  })

  output$decomposition_text <- renderPrint({
    req(decomposition())
    print(decomposition())
  })

  output$decomposition_plot <- renderPlot({
    req(decomposition())
    plot(decomposition())
  }, res = 110)

  random_intercept_model <- reactive({
    req(analysis_ready())
    a <- analysis()
    d <- a$data[stats::complete.cases(a$data[c(a$outcome, a$cluster)]), , drop = FALSE]
    validate(need(nrow(d) > 5L, "Too few complete observations for the model."))

    lme4::lmer(
      safe_formula(a$outcome, character(0), paste0("(1 | ", a$cluster, ")")),
      data = d,
      REML = TRUE
    )
  })

  average_cluster_size <- reactive({
    req(analysis_ready())
    a <- analysis()
    d <- a$data[stats::complete.cases(a$data[c(a$outcome, a$cluster)]), , drop = FALSE]
    mean(as.numeric(table(d[[a$cluster]])))
  })

  icc_result <- reactive({
    hlm_icc(random_intercept_model(), cluster_size = average_cluster_size())
  })

  output$icc_text <- renderPrint({
    cat(sprintf("Average observed cluster size: %.2f\n\n", average_cluster_size()))
    print(icc_result())
  })

  output$icc_plot <- renderPlot({
    hlm_icc_plot(random_intercept_model(), cluster_size = average_cluster_size())
  }, res = 110)

  output$icc_demo_plot <- renderPlot({
    req(analysis_ready())
    a <- analysis()
    values <- sort(unique(a$icc_demo))
    validate(
      need(length(values) >= 2L, "Choose at least two distinct ICC values."),
      need(all(values > 0 & values < 1), "Every ICC value must be between 0 and 1.")
    )
    hlm_icc_demo(
      icc = values,
      n_clusters = 12,
      cluster_size = 18,
      seed = a$seed
    ) +
      labs(subtitle = "Same total variation; clustering shifts from within to between clusters")
  }, res = 110)

  output$shrinkage_text <- renderPrint({
    p <- hlm_shrinkage_plot(random_intercept_model(), n_clusters = 30)
    d <- attr(p, "data")
    cat(sprintf("Clusters displayed: %d\n", nrow(d)))
    cat(sprintf("Reliability range: %.3f to %.3f\n", min(d$reliability), max(d$reliability)))
    cat(sprintf("Mean reliability: %.3f\n", mean(d$reliability)))
  })

  output$shrinkage_plot <- renderPlot({
    hlm_shrinkage_plot(
      random_intercept_model(),
      n_clusters = 30,
      select = "spread",
      style = "arrows"
    )
  }, res = 110)

  mundlak_data <- reactive({
    req(analysis_ready())
    a <- analysis()
    d <- a$data[stats::complete.cases(a$data[c(a$outcome, a$predictor, a$cluster)]), , drop = FALSE]
    validate(need(nrow(d) > 5L, "Too few complete observations for the contextual model."))

    mean_name <- paste0(a$predictor, "_cluster_mean")
    within_name <- paste0(a$predictor, "_within")
    cluster_mean <- ave(d[[a$predictor]], d[[a$cluster]], FUN = mean)
    d[[mean_name]] <- cluster_mean
    d[[within_name]] <- d[[a$predictor]] - cluster_mean

    list(
      data = d,
      mean_name = mean_name,
      within_name = within_name
    )
  })

  contextual_model <- reactive({
    a <- analysis()
    md <- mundlak_data()
    lme4::lmer(
      safe_formula(
        a$outcome,
        c(md$within_name, md$mean_name),
        paste0("(1 | ", a$cluster, ")")
      ),
      data = md$data,
      REML = TRUE
    )
  })

  context_result <- reactive({
    md <- mundlak_data()
    hlm_context(
      contextual_model(),
      x_within = md$within_name,
      x_between = md$mean_name
    )
  })

  output$context_text <- renderPrint({
    print(context_result())
    v <- stats::vcov(contextual_model())
    md <- mundlak_data()
    covariance <- v[md$mean_name, md$within_name]
    cat(sprintf("\nFixed-effect covariance used in the contrast: %.6g\n", covariance))
  })

  output$context_plot <- renderPlot({
    hlm_context_plot(context_result())
  }, res = 110)

  random_slope_model <- reactive({
    req(analysis_ready())
    a <- analysis()
    md <- mundlak_data()
    lme4::lmer(
      safe_formula(
        a$outcome,
        c(md$within_name, md$mean_name),
        paste0("(", md$within_name, " | ", a$cluster, ")")
      ),
      data = md$data,
      REML = TRUE,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 1e5)
      )
    )
  })

  output$random_slope_text <- renderPrint({
    m <- random_slope_model()
    md <- mundlak_data()
    vc <- as.data.frame(lme4::VarCorr(m))
    hit <- vc$var1 == md$within_name & is.na(vc$var2)
    cat(sprintf("Average within-cluster slope: %.3f\n", lme4::fixef(m)[md$within_name]))
    if (any(hit)) {
      cat(sprintf("Random-slope SD: %.3f\n", vc$sdcor[which(hit)[1L]]))
    }
    cat("Singular fit:", if (lme4::isSingular(m)) "yes" else "no", "\n")
  })

  output$random_slope_plot <- renderPlot({
    a <- analysis()
    md <- mundlak_data()
    hlm_random_slope_plot(
      random_slope_model(),
      x_within = md$within_name,
      cluster = a$cluster,
      n_clusters = 25,
      select = "spread"
    )
  }, res = 110)

  moderator_check <- reactive({
    req(analysis_ready())
    a <- analysis()
    if (identical(a$moderator, "None")) {
      return(list(ok = FALSE, message = "Select an observed Level-2 moderator in the sidebar."))
    }
    if (!a$moderator %in% names(a$data)) {
      return(list(ok = FALSE, message = "The selected moderator is not present in the analysis data."))
    }
    if (!cluster_level_variable(a$data, a$cluster, a$moderator)) {
      return(list(
        ok = FALSE,
        message = "The moderator varies within at least one cluster. Select a variable that is constant within clusters."
      ))
    }
    list(ok = TRUE, message = "The selected moderator is constant within clusters.")
  })

  output$cross_level_note <- renderUI({
    req(analysis_ready())
    check <- moderator_check()
    div(
      class = if (check$ok) "model-note valid" else "model-note invalid",
      check$message
    )
  })

  cross_level_model <- reactive({
    req(analysis_ready())
    check <- moderator_check()
    validate(need(check$ok, check$message))

    a <- analysis()
    md <- mundlak_data()
    needed <- c(a$outcome, a$cluster, a$moderator, md$within_name, md$mean_name)
    d <- md$data[stats::complete.cases(md$data[needed]), , drop = FALSE]
    validate(need(length(unique(d[[a$moderator]])) >= 2L, "The moderator must have at least two values."))

    interaction_term <- paste0(md$within_name, ":", a$moderator)
    lme4::lmer(
      safe_formula(
        a$outcome,
        c(md$within_name, md$mean_name, a$moderator, interaction_term),
        paste0("(1 | ", a$cluster, ")")
      ),
      data = d,
      REML = TRUE
    )
  })

  output$cross_level_text <- renderPrint({
    m <- cross_level_model()
    a <- analysis()
    md <- mundlak_data()
    interaction_name <- paste0(md$within_name, ":", a$moderator)
    reverse_name <- paste0(a$moderator, ":", md$within_name)
    coefficient_name <- if (interaction_name %in% names(lme4::fixef(m))) interaction_name else reverse_name
    cat(sprintf("Interaction coefficient: %.3f\n", lme4::fixef(m)[coefficient_name]))
    cat("Moderator:", a$moderator, "\n")
  })

  output$cross_level_plot <- renderPlot({
    a <- analysis()
    md <- mundlak_data()
    hlm_cross_level_plot(
      cross_level_model(),
      x_within = md$within_name,
      moderator = a$moderator
    )
  }, res = 110)

  output$longitudinal_note <- renderUI({
    if (identical(input$source_type, "sim")) {
      return(div(
        class = "model-note invalid",
        "The selected simulation is two-level. Choose the simulated longitudinal data or upload a long-format CSV."
      ))
    }
    if (input$run < 1L) {
      return(div(class = "model-note invalid", "Select the longitudinal variables, then click RUN."))
    }
    a <- analysis()
    if (!identical(a$source_type, input$source_type)) {
      return(div(
        class = "model-note invalid",
        "Click RUN to update the analysis for the selected data."
      ))
    }
    if (identical(a$person, "None") || identical(a$time, "None")) {
      div(
        class = "model-note invalid",
        "Select distinct child/person and time variables, then click RUN again."
      )
    } else {
      div(class = "model-note valid", "The three-level longitudinal variables are active.")
    }
  })

  bpw_result <- reactive({
    req(analysis_ready())
    a <- analysis()
    validate(
      need(a$source_type %in% c("sim_long", "upload"),
           "Choose the simulated longitudinal data or upload a long-format CSV."),
      need(!identical(a$person, "None"), "Select a child/person ID to enable three-level decomposition."),
      need(!identical(a$time, "None"), "Select a time variable to enable three-level decomposition."),
      need(all(c(a$person, a$time) %in% names(a$data)), "The longitudinal variables are not present in the data."),
      need(length(unique(c(a$cluster, a$person, a$time, a$outcome))) == 4L,
           "Cluster, child/person, time, and outcome must be four distinct variables.")
    )

    d <- a$data[stats::complete.cases(a$data[c(a$cluster, a$person, a$time, a$outcome)]), , drop = FALSE]
    person_cluster_counts <- tapply(
      as.character(d[[a$cluster]]),
      d[[a$person]],
      function(z) length(unique(z))
    )
    person_sizes <- table(d[[a$person]])
    validate(
      need(nrow(d) > 5L, "Too few complete longitudinal observations."),
      need(all(person_cluster_counts <= 1L), "Each child/person ID must be nested in exactly one cluster."),
      need(sum(person_sizes >= 2L) >= 3L, "At least three children/people must have repeated observations.")
    )

    hlm_decompose_long(
      d,
      var = a$outcome,
      cluster = a$cluster,
      id = a$person,
      time = a$time
    )
  })

  output$bpw_text <- renderPrint({
    if (identical(input$source_type, "sim")) {
      cat("Choose the simulated longitudinal data or upload a long-format CSV.\n")
      return(invisible(NULL))
    }
    req(analysis_ready())
    a <- analysis()
    if (!identical(a$source_type, input$source_type)) {
      cat("Click RUN to update the analysis for the selected data.\n")
    } else if (identical(a$person, "None") || identical(a$time, "None")) {
      cat("Select child/person and time variables, then click RUN again.\n")
    } else {
      print(bpw_result())
    }
  })

  output$bpw_plot <- renderPlot({
    req(input$source_type %in% c("sim_long", "upload"))
    req(analysis_ready())
    req(identical(analysis()$source_type, input$source_type))
    plot(bpw_result())
  }, res = 110)

  output$growth_plot <- renderPlot({
    req(input$source_type %in% c("sim_long", "upload"))
    req(analysis_ready())
    a <- analysis()
    validate(
      need(identical(a$source_type, input$source_type),
           "Click RUN to update the analysis for the selected data."),
      need(a$source_type %in% c("sim_long", "upload"),
           "Choose the simulated longitudinal data or upload a long-format CSV."),
      need(!identical(a$person, "None"), "Select a child/person ID to enable the growth plot."),
      need(!identical(a$time, "None"), "Select a time variable to enable the growth plot."),
      need(length(unique(c(a$cluster, a$person, a$time, a$outcome))) == 4L,
           "Cluster, child/person, time, and outcome must be four distinct variables.")
    )

    unit <- a$person
    needed <- c(unit, a$time, a$outcome)
    validate(need(all(needed %in% names(a$data)), "The selected longitudinal variables are unavailable."))
    d <- a$data[stats::complete.cases(a$data[needed]), , drop = FALSE]
    units <- unique(d[[unit]])
    if (length(units) > 20L) {
      set.seed(2026)
      units <- sample(units, 20L)
      d <- d[d[[unit]] %in% units, , drop = FALSE]
    }

    ggplot(
      d,
      aes(
        x = .data[[a$time]],
        y = .data[[a$outcome]],
        group = .data[[unit]],
        colour = factor(.data[[unit]])
      )
    ) +
      geom_line(alpha = 0.65, linewidth = 0.7) +
      geom_point(alpha = 0.55, size = 1.2) +
      scale_colour_manual(values = colorRampPalette(c(app_colors[["light_blue"]], app_colors[["blue"]]))(length(units))) +
      labs(
        x = a$time,
        y = a$outcome,
        title = paste("Observed trajectories for", length(units), "individuals"),
        colour = unit
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  }, res = 110)
}

shinyApp(ui = ui, server = server)
