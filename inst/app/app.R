## app.R --- hlmLab interactive teaching app
##
## Features:
##   - 2-level lab (variance, ICC, contextual, random slopes)
##   - 3-level B-P-W decomposition (cluster, person, time)
##   - Longitudinal growth plots
##
## Requires:
##   hlmLab, shiny, lme4, dplyr, ggplot2, scales

library(shiny)
library(hlmLab)
library(lme4)
library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# Helper: simulate 2-level data with user-chosen parameters
# ------------------------------------------------------------
simulate_hlm_data <- function(J, nJ, beta0, beta1, tau00, sigma2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  N <- J * nJ

  u0j <- rnorm(J, 0, sqrt(tau00))
  school_id <- rep(1:J, each = nJ)
  u0        <- u0j[school_id]

  SES   <- rnorm(N, 0, 1)
  error <- rnorm(N, 0, sqrt(sigma2))

  math_score <- beta0 + u0 + beta1 * SES + error

  data.frame(
    school_id  = factor(school_id),
    SES        = SES,
    math_score = math_score
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("hlmLab: Interactive Multilevel Teaching App"),

  sidebarLayout(
    sidebarPanel(
      h4("Data source"),
      radioButtons(
        "source_type", label = NULL,
        choices = c("Simulated data" = "sim", "Upload .csv" = "upload"),
        selected = "sim"
      ),

      # --- Simulated-data controls ---
      conditionalPanel(
        condition = "input.source_type == 'sim'",
        tags$hr(),
        h4("Simulation controls (2-level)"),
        sliderInput("J",     "Number of clusters (schools)",      min = 5, max = 80, value = 30, step = 1),
        sliderInput("nJ",    "Students per cluster",              min = 5, max = 80, value = 40, step = 1),
        sliderInput("beta1", "True Level-1 slope for X",         min = -5, max = 10, value = 5,  step = 0.5),
        sliderInput("tau00", "Between-cluster SD (random intercept)", min = 5, max = 40, value = 15, step = 1),
        sliderInput("sigma2","Within-cluster SD (residual)",      min = 5, max = 40, value = 10, step = 1),
        numericInput("sim_seed", "Simulation seed (for reproducibility)", value = 123, min = 1)
      ),

      # --- Upload-data controls ---
      conditionalPanel(
        condition = "input.source_type == 'upload'",
        tags$hr(),
        fileInput("file", "Upload a .csv file", accept = ".csv"),
        helpText("After uploading, choose the cluster ID, person ID (optional),
                  time (optional), outcome, and Level-1 predictor below.")
      ),

      tags$hr(),
      uiOutput("var_select_ui"),

      tags$hr(),
      numericInput(
        "cluster_size", "Approx. average cluster size (for design effect)",
        value = 30, min = 2
      ),

      tags$hr(),
      actionButton("run", "RUN / Update analysis", class = "btn-primary"),
      helpText("Change settings, then click RUN to update results.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Data",
                 h4("First rows of the data"),
                 tableOutput("head_table")
        ),
        tabPanel("Variance decomposition (2-level)",
                 h4("Between vs within variance"),
                 verbatimTextOutput("var_decomp_text"),
                 plotOutput("var_decomp_plot", height = "350px")
        ),
        tabPanel("ICC",
                 h4("Intraclass correlation"),
                 verbatimTextOutput("icc_text"),
                 plotOutput("icc_plot", height = "350px")
        ),
        tabPanel("Contextual effects",
                 h4("Mundlak decomposition"),
                 verbatimTextOutput("context_text"),
                 plotOutput("context_plot", height = "350px")
        ),
        tabPanel("Random slopes",
                 h4("Cross-level interaction geometry"),
                 helpText("Random slopes for the Level-1 predictor by cluster."),
                 plotOutput("fan_plot", height = "350px")
        ),
        tabPanel("3-Level B-P-W",
                 h4("Three-level variance decomposition (Between-Person-Within)"),
                 helpText("Requires a cluster ID (Level-3), person ID (Level-2), and time variable (Level-1)."),
                 verbatimTextOutput("bpw_text"),
                 plotOutput("bpw_plot", height = "350px")
        ),
        tabPanel("Growth",
                 h4("Longitudinal growth trajectories"),
                 helpText("Uses selected outcome and time variable. For many units, a random subset is plotted."),
                 plotOutput("growth_plot", height = "400px")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  # -----------------------
  # 1. Data reactive (sim vs upload) triggered by RUN
  # -----------------------
  dat <- eventReactive(
    input$run,
    {
      if (input$source_type == "sim") {
        simulate_hlm_data(
          J      = input$J,
          nJ     = input$nJ,
          beta0  = 50,
          beta1  = input$beta1,
          tau00  = input$tau00^2,    # slider is SD; square to get variance
          sigma2 = input$sigma2^2,
          seed   = input$sim_seed
        )
      } else {
        req(input$file)
        read.csv(input$file$datapath)
      }
    },
    ignoreNULL = FALSE   # run once on app launch
  )

  output$head_table <- renderTable({
    req(dat())
    head(dat())
  })

  # -----------------------
  # 2. Variable selectors (depends on current data)
  # -----------------------
  output$var_select_ui <- renderUI({
    d  <- dat()
    cn <- names(d)

    tagList(
      h4("Model variable mapping"),
      selectInput("cluster",   "Cluster ID (Level-2 / Level-3)", choices = cn,
                  selected = if ("school_id" %in% cn) "school_id" else cn[1]),
      selectInput("id_long",   "Person ID (Level-2, optional)",  choices = c("None", cn),
                  selected = if ("id"   %in% cn) "id"   else "None"),
      selectInput("time_long", "Time variable (Level-1, optional)", choices = c("None", cn),
                  selected = if ("time" %in% cn) "time" else "None"),
      selectInput("outcome",   "Outcome (Y)", choices = cn,
                  selected = if ("math_score" %in% cn) "math_score" else cn[2]),
      selectInput("x_l1",     "Level-1 predictor (X)", choices = cn,
                  selected = if ("SES" %in% cn) "SES" else cn[3])
    )
  })

  valid_inputs <- reactive({
    d <- dat()
    all(c(input$cluster, input$outcome, input$x_l1) %in% names(d))
  })

  # helper: cluster must have repeated values
  cluster_ok <- reactive({
    req(valid_inputs())
    d  <- dat()
    cl <- input$cluster
    length(unique(d[[cl]])) < nrow(d)
  })

  # -----------------------
  # 3. Variance decomposition (2-level B vs W)
  # -----------------------
  decomp_obj <- reactive({
    req(valid_inputs())
    d  <- dat()
    cl <- input$cluster

    if (length(unique(d[[cl]])) >= nrow(d)) {
      stop("Cluster ID must have repeated values (e.g., school, class). ",
           "The selected variable appears to be unique for each row.")
    }

    hlm_decompose(
      data    = d,
      var     = input$outcome,
      cluster = cl
    )
  })

  output$var_decomp_text <- renderPrint({
    req(decomp_obj())
    print(decomp_obj())
  })

  output$var_decomp_plot <- renderPlot({
    req(decomp_obj())
    dec <- decomp_obj()
    df  <- dec$summary

    df$component <- factor(df$component, levels = df$component)

    ggplot(df, aes(x = component, y = share)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        x     = NULL,
        y     = "Share of total variance",
        title = paste("Variance partitioning for", dec$var)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  # -----------------------
  # 4. Random-intercept model + ICC (2-level)
  # -----------------------
  m_ri <- reactive({
    req(valid_inputs(), cluster_ok())
    d  <- dat()
    cl <- input$cluster
    y  <- input$outcome
    d[[cl]] <- factor(d[[cl]])

    fml <- as.formula(paste(y, "~ 1 + (1 |", cl, ")"))
    lmer(fml, data = d)
  })

  icc_obj <- reactive({
    req(m_ri())
    hlm_icc(m_ri(), cluster_size = input$cluster_size)
  })

  output$icc_text <- renderPrint({
    req(icc_obj())
    icc_obj()
  })

  output$icc_plot <- renderPlot({
    req(icc_obj())
    ic <- icc_obj()

    df <- data.frame(
      component = c("Between-cluster variance", "Within-cluster variance"),
      variance  = c(ic$re_var, ic$resid_var)
    )
    df$share <- df$variance / sum(df$variance)

    subtitle_txt <- sprintf("ICC = %.3f", ic$icc)
    if (!is.na(ic$deff)) {
      subtitle_txt <- paste0(
        subtitle_txt,
        sprintf("   |   Design effect \u2248 %.2f", ic$deff)
      )
    }

    ggplot(df, aes(x = "", y = share, fill = component)) +
      geom_col(width = 0.4) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        x        = NULL,
        y        = "Share of total variance",
        title    = "Intraclass correlation (ICC) as variance partitioning",
        subtitle = subtitle_txt,
        fill     = NULL
      ) +
      theme_minimal()
  })

  # -----------------------
  # 5. Contextual effects (Mundlak)
  # -----------------------
  mundlak_data <- reactive({
    req(valid_inputs(), cluster_ok())
    d  <- dat()
    cl <- input$cluster
    x  <- input$x_l1

    cluster_mean         <- ave(d[[x]], d[[cl]], FUN = mean, na.rm = TRUE)
    d[[paste0(x, "_mean")]] <- cluster_mean
    d[[paste0(x, "_c")]]    <- d[[x]] - cluster_mean
    d[[cl]] <- factor(d[[cl]])
    d
  })

  m_mundlak <- reactive({
    d  <- mundlak_data()
    y  <- input$outcome
    x  <- input$x_l1
    cl <- input$cluster

    fml <- as.formula(
      paste0(y, " ~ ", x, "_c + ", x, "_mean + (1 | ", cl, ")")
    )
    lmer(fml, data = d)
  })

  ctx_obj <- reactive({
    x <- input$x_l1
    hlm_context(
      model     = m_mundlak(),
      x_within  = paste0(x, "_c"),
      x_between = paste0(x, "_mean")
    )
  })

  output$context_text <- renderPrint({
    req(ctx_obj())
    print(ctx_obj())
  })

  output$context_plot <- renderPlot({
    req(ctx_obj())
    df <- as.data.frame(ctx_obj())

    df$effect_type <- factor(
      df$effect_type,
      levels = c("Between-cluster", "Contextual (B - W)", "Within-cluster")
    )

    ggplot(df, aes(x = effect_type, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point() +
      geom_errorbar(
        aes(ymin = estimate - 1.96 * se,
            ymax = estimate + 1.96 * se),
        width = 0.1
      ) +
      labs(
        x     = NULL,
        y     = "Effect size",
        title = "Within, between, and contextual effects"
      ) +
      theme_minimal()
  })

  # -----------------------
  # 6. Random slopes fan plot
  # -----------------------
  m_rs <- reactive({
    d  <- mundlak_data()
    y  <- input$outcome
    x  <- input$x_l1
    cl <- input$cluster

    fml <- as.formula(
      paste0(y, " ~ ", x, "_c + ", x, "_mean + (", x, "_c | ", cl, ")")
    )
    lmer(fml, data = d)
  })

  output$fan_plot <- renderPlot({
    req(m_rs())
    x  <- paste0(input$x_l1, "_c")
    cl <- input$cluster

    hlm_random_slope_plot(
      model      = m_rs(),
      x_within   = x,
      cluster    = cl,
      n_points   = 40,
      n_clusters = 30
    )
  })

  # -----------------------
  # 7. 3-Level B-P-W decomposition (cluster, person, time)
  # -----------------------
  bpw_obj <- reactive({
    req(valid_inputs())
    req(input$id_long != "None", input$time_long != "None")

    d  <- dat()
    cl <- input$cluster
    id <- input$id_long
    tm <- input$time_long

    if (!all(c(cl, id, tm) %in% names(d))) {
      stop("Cluster ID, person ID, and time variable must all be valid columns.")
    }

    hlm_decompose_long(
      data    = d,
      var     = input$outcome,
      cluster = cl,
      id      = id,
      time    = tm
    )
  })

  output$bpw_text <- renderPrint({
    if (input$id_long == "None" || input$time_long == "None") {
      cat("Select a person ID and time variable in the sidebar to enable 3-level decomposition.\n")
    } else {
      req(bpw_obj())
      print(bpw_obj())
    }
  })

  output$bpw_plot <- renderPlot({
    req(input$id_long != "None", input$time_long != "None")
    req(bpw_obj())
    dec <- bpw_obj()
    df  <- dec$summary

    df$component <- factor(df$component, levels = df$component)

    ggplot(df, aes(x = component, y = share)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        x     = NULL,
        y     = "Share of total variance",
        title = paste("3-level variance partitioning for", dec$var)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  # -----------------------
  # 8. Growth plot (longitudinal trajectories)
  # -----------------------
  output$growth_plot <- renderPlot({
    if (input$time_long == "None") {
      plot.new()
      text(0.5, 0.5, "Select a time variable in the sidebar to enable growth plots.")
      return(invisible(NULL))
    }

    d  <- dat()
    tm <- input$time_long
    y  <- input$outcome
    cl <- input$cluster

    if (!all(c(tm, y, cl) %in% names(d))) {
      plot.new()
      text(0.5, 0.5, "Time, outcome, or cluster variable not found in data.")
      return(invisible(NULL))
    }

    d[[cl]] <- factor(d[[cl]])

    # mean trajectories by cluster over time
    df_mean <- d %>%
      group_by_at(c(cl, tm)) %>%
      summarise(mean_y = mean(get(y), na.rm = TRUE), .groups = "drop")

    # if many clusters, sample up to 15
    cl_levels <- unique(df_mean[[cl]])
    if (length(cl_levels) > 15) {
      keep_cl <- sample(cl_levels, 15)
      df_mean <- df_mean[df_mean[[cl]] %in% keep_cl, ]
    }

    ggplot(df_mean, aes_string(x = tm, y = "mean_y", group = cl, color = cl)) +
      geom_line(alpha = 0.7) +
      labs(
        x     = tm,
        y     = paste("Mean", y),
        title = "Cluster-level mean trajectories over time"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

}

# ------------------------------------------------------------
# Run the app
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
