# =============================================================================
# EcoSentinel – Module 5: Shiny Web Dashboard
# Full interactive web application — run with: shiny::runApp("app.R")
# =============================================================================
# LAYOUT:
#   • Sidebar: controls (indicator select, threshold slider, batch slider)
#   • Tab 1 – Overview   : live ecosystem signal + prediction gauge
#   • Tab 2 – EWS Panel  : early warning signals + Kendall tau
#   • Tab 3 – XAI Global : permutation importance + indicator donut
#   • Tab 4 – XAI Local  : LIME explanation for selected time point
#   • Tab 5 – Detection  : changepoint analysis + timeline
#   • Tab 6 – Report     : model performance (ROC, confusion matrix)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(DT)
  library(scales)
  library(pROC)
})

# ── Load all pre-computed artefacts ─────────────────────────────────────────
eco_raw      <- readRDS("eco_data_raw.rds")
eco_norm     <- readRDS("eco_data_norm.rds")
rf_model     <- readRDS("rf_model.rds")
feature_cols <- readRDS("feature_cols.rds")
perm_df      <- readRDS("perm_importance.rds")
lime_out     <- readRDS("lime_explanation.rds")
roc_obj      <- readRDS("roc_object.rds")
kendall      <- readRDS("kendall_results.rds")
ind_cont     <- readRDS("indicator_contribution.rds")
cm           <- readRDS("confusion_matrix.rds")
cps          <- readRDS("changepoints.rds")

# Compute model predictions on full dataset for timeline
eco_model_all <- eco_norm %>% filter(!is.na(ice_var))
pred_all      <- predict(rf_model, eco_model_all[, feature_cols], type="prob")[,"NearTip"]
eco_model_all$NearTip_Prob <- pred_all

# ── Dark theme for ggplot ─────────────────────────────────────────────────────
theme_eco_shiny <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill="#1a1f2e", color=NA),
      panel.background = element_rect(fill="#1a1f2e", color=NA),
      panel.grid.major = element_line(color="#2d3748"),
      panel.grid.minor = element_blank(),
      text             = element_text(color="#e2e8f0"),
      axis.text        = element_text(color="#a0aec0"),
      axis.title       = element_text(color="#cbd5e0"),
      plot.title       = element_text(color="#63b3ed", size=14, face="bold"),
      plot.subtitle    = element_text(color="#718096", size=10),
      legend.background= element_rect(fill="#1a1f2e"),
      legend.text      = element_text(color="#cbd5e0"),
      strip.text       = element_text(color="#63b3ed", face="bold")
    )
}

COL_NEARTIP <- "#FC8181"
COL_STABLE  <- "#68D391"
COL_BLUE    <- "#63B3ED"
COL_WARN    <- "#F6E05E"
COLORS_4    <- c("#63B3ED","#F6E05E","#FC8181","#68D391")

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "black",

  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src="", height="24px"),
      tags$b("🌍 EcoSentinel"),
      style="font-size:18px; color:#63B3ED;"
    ),
    titleWidth = 240,
    tags$li(class="dropdown",
      tags$p(style="color:#68D391; margin-top:15px; margin-right:20px;
                     font-size:13px;",
             "Ecological Tipping Point Detection System"))
  ),

  # ── Sidebar ──────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 240,
    tags$style("
      .sidebar { background-color: #0d1117 !important; }
      .sidebar-menu li a { color: #8b949e !important; }
      .sidebar-menu li.active a { color: #58a6ff !important;
                                   background-color: #161b22 !important; }
    "),
    sidebarMenu(
      menuItem("🏠 Overview",       tabName="tab_overview",  icon=icon("home")),
      menuItem("📈 Early Warnings", tabName="tab_ews",       icon=icon("chart-line")),
      menuItem("🔍 XAI – Global",   tabName="tab_xai_global",icon=icon("globe")),
      menuItem("🎯 XAI – Local",    tabName="tab_xai_local", icon=icon("crosshairs")),
      menuItem("🔬 Changepoints",   tabName="tab_cp",        icon=icon("search")),
      menuItem("📊 Model Report",   tabName="tab_report",    icon=icon("clipboard")),
      hr(style="border-color:#30363d;"),
      tags$div(style="padding:12px 16px; color:#8b949e; font-size:12px;",
        tags$b(style="color:#58a6ff;","Controls"),
        br(), br(),
        sliderInput("threshold", "Alert Threshold",
                    min=0.3, max=0.8, value=0.5, step=0.05),
        sliderInput("time_idx", "View Time Point (month)",
                    min=31, max=360, value=250, step=1),
        selectInput("indicator", "Primary Indicator",
                    choices=c("Sea Ice"="sea_ice",
                              "Amazon Rainfall"="amazon_rain",
                              "Coral Stress"="coral_stress",
                              "Soil Carbon"="soil_carbon"),
                    selected="sea_ice"),
        sliderInput("ews_window", "EWS Rolling Window (months)",
                    min=12, max=60, value=30, step=6)
      )
    )
  ),

  # ── Body ─────────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .main-footer { background-color: #0d1117; }
      .box { background-color: #161b22; border-top: 0px;
             border-radius: 8px; box-shadow: none; }
      .box-header { background-color: #161b22; color: #c9d1d9; }
      .box-title  { color: #58a6ff; font-size: 14px; font-weight: bold; }
      .small-box  { border-radius: 8px; }
      .value-box .small-box { background-color: #161b22 !important; }
      h3 { color: #58a6ff; }
      .nav-tabs-custom .nav-tabs li.active a { border-top: 2px solid #58a6ff; }
      .info-box { background-color: #161b22; border-radius: 8px; }
      .info-box-icon { border-radius: 8px 0 0 8px; }
      body { font-family: 'SF Mono', 'Fira Code', monospace; }
    "))),

    tabItems(

      # ── TAB 1: OVERVIEW ────────────────────────────────────────────────────
      tabItem(tabName="tab_overview",
        fluidRow(
          valueBoxOutput("vbox_status",   width=3),
          valueBoxOutput("vbox_prob",     width=3),
          valueBoxOutput("vbox_auc",      width=3),
          valueBoxOutput("vbox_accuracy", width=3)
        ),
        fluidRow(
          box(title="Ecosystem Signal + Prediction Probability",
              width=12, solidHeader=FALSE,
              plotlyOutput("plot_overview_main", height="450px"))
        ),
        fluidRow(
          box(title="Alert Status", width=4,
              tags$div(id="alert_box", style="padding:20px; text-align:center;",
                uiOutput("ui_alert_message"))),
          box(title="Current Indicator Value", width=4,
              plotlyOutput("plot_gauge", height="220px")),
          box(title="Prediction Explanation (Quick)", width=4,
              plotOutput("plot_top5_imp", height="220px"))
        )
      ),

      # ── TAB 2: EARLY WARNING SIGNALS ───────────────────────────────────────
      tabItem(tabName="tab_ews",
        fluidRow(
          box(title="Early Warning Signals — Critical Slowing Down",
              width=12, solidHeader=FALSE,
              plotlyOutput("plot_ews_main", height="420px"))
        ),
        fluidRow(
          box(title="Kendall τ Statistical Validity Test", width=7,
              plotOutput("plot_kendall", height="300px")),
          box(title="EWS Theory", width=5,
              tags$div(style="padding:12px; color:#c9d1d9; font-size:13px;",
                tags$h4(style="color:#58a6ff;","Critical Slowing Down (CSD)"),
                tags$p("Before a system tips, it slows its recovery from
                        disturbances. This is mathematically predicted by
                        bifurcation theory."),
                tags$h4(style="color:#58a6ff;","Measurable Signatures:"),
                tags$ul(
                  tags$li(tags$b("Variance ↑ :"),"System fluctuates more"),
                  tags$li(tags$b("AC-1 ↑ :"),"System remembers longer"),
                  tags$li(tags$b("Skewness ↑ :"),"Asymmetric fluctuations"),
                  tags$li(tags$b("CV ↑ :"),"Relative variability grows")
                ),
                tags$h4(style="color:#58a6ff;","Kendall τ Test:"),
                tags$p("Non-parametric test of monotonic trend.
                        τ > 0, p < 0.05 = statistically significant warning.")
              ))
        )
      ),

      # ── TAB 3: XAI GLOBAL ─────────────────────────────────────────────────
      tabItem(tabName="tab_xai_global",
        fluidRow(
          box(title="Permutation Feature Importance (Global Explanation)",
              width=8, solidHeader=FALSE,
              plotlyOutput("plot_perm_imp", height="450px")),
          box(title="Indicator Contribution", width=4,
              plotlyOutput("plot_donut", height="450px"))
        ),
        fluidRow(
          box(title="How to Read This", width=12,
              tags$div(style="color:#c9d1d9; font-size:13px; padding:10px;",
                tags$p("• Each bar shows how much accuracy the model LOSES
                         when that feature is randomly shuffled."),
                tags$p("• A large drop means the model heavily relies on that feature."),
                tags$p("• Color = which ecosystem indicator the feature belongs to."),
                tags$p("• The donut chart shows which ecosystem contributes most
                         to the tipping point warning signal overall.")
              ))
        )
      ),

      # ── TAB 4: XAI LOCAL ──────────────────────────────────────────────────
      tabItem(tabName="tab_xai_local",
        fluidRow(
          box(title="Local LIME Explanation for Selected Time Point",
              width=8, solidHeader=FALSE,
              plotlyOutput("plot_lime", height="420px")),
          box(title="Instance Details", width=4,
              uiOutput("ui_instance_details"))
        ),
        fluidRow(
          box(title="What LIME Does", width=12,
              tags$div(style="color:#c9d1d9; font-size:13px; padding:10px;",
                tags$p("LIME (Local Interpretable Model-Agnostic Explanations)
                         explains WHY the model made a specific prediction."),
                tags$p("• It creates small perturbations around the selected point."),
                tags$p("• Fits a simple linear model in that local neighborhood."),
                tags$p("• Red bars = features pushing toward 'Near Tipping Point'."),
                tags$p("• Green bars = features pushing toward 'Stable'."),
                tags$p("• Use the 'View Time Point' slider to explore different predictions.")
              ))
        )
      ),

      # ── TAB 5: CHANGEPOINTS ────────────────────────────────────────────────
      tabItem(tabName="tab_cp",
        fluidRow(
          box(title="Bayesian Changepoint Detection — PELT Algorithm",
              width=12, solidHeader=FALSE,
              plotlyOutput("plot_cp_main", height="420px"))
        ),
        fluidRow(
          box(title="Detected Changepoints Summary", width=6,
              DT::dataTableOutput("tbl_changepoints")),
          box(title="Changepoint Theory", width=6,
              tags$div(style="color:#c9d1d9; font-size:13px; padding:10px;",
                tags$h4(style="color:#58a6ff;","PELT Algorithm"),
                tags$p("Pruned Exact Linear Time — finds the optimal set of
                         changepoints that minimizes a cost function + BIC penalty."),
                tags$p("Unlike threshold-based methods, PELT does NOT need you
                         to pre-specify where or how many changepoints exist."),
                tags$h4(style="color:#58a6ff;","What Changes?"),
                tags$p("We detect changes in:"),
                tags$ul(
                  tags$li("Mean of the signal (level shift)"),
                  tags$li("Variance of the signal (volatility shift)"),
                  tags$li("Both combined (full regime change)")
                ),
                tags$h4(style="color:#58a6ff;","Comparison with True Tipping Point"),
                tags$p("The true tipping point was injected at month 240.
                         How close does PELT get? That's our validation.")
              ))
        )
      ),

      # ── TAB 6: MODEL REPORT ────────────────────────────────────────────────
      tabItem(tabName="tab_report",
        fluidRow(
          box(title="ROC Curve", width=6,
              plotOutput("plot_roc", height="350px")),
          box(title="Confusion Matrix", width=6,
              plotOutput("plot_cm", height="350px"))
        ),
        fluidRow(
          box(title="Model Performance Metrics", width=6,
              tableOutput("tbl_metrics")),
          box(title="About This Project", width=6,
              tags$div(style="color:#c9d1d9; font-size:13px; padding:10px;",
                tags$h4(style="color:#58a6ff;","EcoSentinel"),
                tags$p("A research-grade Ecological Tipping Point Detection
                         system combining:"),
                tags$ul(
                  tags$li("Nonlinear Early Warning Signals (EWS)"),
                  tags$li("Bayesian Changepoint Detection (PELT/BIC)"),
                  tags$li("Random Forest with Balanced Sampling"),
                  tags$li("Global XAI (Permutation Importance)"),
                  tags$li("Local XAI (LIME-style local surrogate)"),
                  tags$li("Kendall τ statistical validation")
                ),
                tags$p(style="color:#8b949e;",
                       "Dataset: 30-year simulated ecological time series",
                       "mimicking Arctic Sea Ice, Amazon Rainfall,",
                       "Coral Bleaching, and Soil Carbon dynamics.")
              ))
        )
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ── Reactive: current prediction at selected time point ──────────────────
  current_pred <- reactive({
    idx <- input$time_idx
    row <- eco_model_all %>% filter(month_index == idx)
    if (nrow(row) == 0) row <- eco_model_all[nrow(eco_model_all),]
    row
  })

  # ── Value Boxes ─────────────────────────────────────────────────────────────
  output$vbox_status <- renderValueBox({
    prob <- current_pred()$NearTip_Prob
    if (length(prob) == 0) prob <- 0
    status <- ifelse(prob > input$threshold, "⚠ ALERT", "✓ STABLE")
    color  <- ifelse(prob > input$threshold, "red", "green")
    valueBox(status, "System Status", icon=icon("bell"), color=color)
  })

  output$vbox_prob <- renderValueBox({
    prob <- current_pred()$NearTip_Prob
    if (length(prob) == 0) prob <- 0
    valueBox(sprintf("%.1f%%", prob*100), "Tipping Probability",
             icon=icon("percent"), color=ifelse(prob>0.5,"yellow","blue"))
  })

  output$vbox_auc <- renderValueBox({
    valueBox(sprintf("%.4f", as.numeric(auc(roc_obj))),
             "Model AUC", icon=icon("chart-area"), color="purple")
  })

  output$vbox_accuracy <- renderValueBox({
    acc <- cm$overall["Accuracy"]
    valueBox(sprintf("%.1f%%", acc*100),
             "Test Accuracy", icon=icon("check"), color="teal")
  })

  # ── Overview Main Plot ────────────────────────────────────────────────────
  output$plot_overview_main <- renderPlotly({
    sel_col <- input$indicator
    label_map <- c(sea_ice="Sea Ice Extent (Mkm²)",
                   amazon_rain="Amazon Rainfall (mm/mo)",
                   coral_stress="Coral Bleaching Stress",
                   soil_carbon="Soil Carbon Index")
    sel_label <- label_map[sel_col]
    threshold <- input$threshold

    p <- ggplot(eco_model_all, aes(x=date)) +
      geom_line(aes(y=NearTip_Prob, color="Tipping Probability"),
                linewidth=0.8) +
      geom_ribbon(aes(ymin=0, ymax=NearTip_Prob,
                      fill=ifelse(NearTip_Prob > threshold, "Alert","Safe")),
                  alpha=0.3) +
      geom_hline(yintercept=threshold, linetype="dashed",
                 color=COL_WARN, linewidth=0.8) +
      geom_vline(xintercept=as.numeric(eco_raw$date[241]),
                 color=COL_NEARTIP, linewidth=1, linetype="solid") +
      geom_vline(xintercept=as.numeric(eco_raw$date[input$time_idx]),
                 color="#bc8cff", linewidth=1.2, linetype="dotted") +
      scale_fill_manual(values=c("Alert"=COL_NEARTIP,"Safe"=COL_STABLE),
                        guide="none") +
      scale_color_manual(values=c("Tipping Probability"=COL_BLUE),
                         guide="none") +
      scale_y_continuous(labels=percent_format()) +
      labs(x="Date", y="P(NearTip)") +
      theme_eco_shiny()

    ggplotly(p, tooltip=c("x","y")) %>%
      layout(paper_bgcolor="#1a1f2e", plot_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── Alert Message ─────────────────────────────────────────────────────────
  output$ui_alert_message <- renderUI({
    prob <- current_pred()$NearTip_Prob
    if (length(prob) == 0) prob <- 0
    if (prob > input$threshold) {
      tags$div(style="background:#2d1b1b; border:2px solid #FC8181;
                       border-radius:8px; padding:20px;",
        tags$h3(style="color:#FC8181; margin:0;","🚨 TIPPING POINT ALERT"),
        tags$p(style="color:#FEB2B2; font-size:18px; margin:8px 0 0;",
               sprintf("Probability: %.1f%%", prob*100)),
        tags$p(style="color:#FC8181;","Ecological regime shift imminent.")
      )
    } else {
      tags$div(style="background:#1a2e1a; border:2px solid #68D391;
                       border-radius:8px; padding:20px;",
        tags$h3(style="color:#68D391; margin:0;","✅ SYSTEM STABLE"),
        tags$p(style="color:#9AE6B4; font-size:18px; margin:8px 0 0;",
               sprintf("Probability: %.1f%%", prob*100)),
        tags$p(style="color:#68D391;","No tipping point detected.")
      )
    }
  })

  # ── Gauge Plot ─────────────────────────────────────────────────────────────
  output$plot_gauge <- renderPlotly({
    prob <- current_pred()$NearTip_Prob
    if (length(prob) == 0) prob <- 0
    plot_ly(type="indicator", mode="gauge+number",
            value=round(prob*100, 1),
            gauge=list(
              axis=list(range=list(0,100), tickcolor="white"),
              bar=list(color=ifelse(prob>input$threshold,"#FC8181","#68D391")),
              bgcolor="#1a1f2e", bordercolor="#2d3748",
              steps=list(
                list(range=c(0,50),  color="#1a2e1a"),
                list(range=c(50,75), color="#2d2a14"),
                list(range=c(75,100),color="#2d1b1b")
              ),
              threshold=list(
                line=list(color="#F6E05E", width=3),
                thickness=0.75, value=input$threshold*100
              )
            ),
            number=list(suffix="%", font=list(color="#e2e8f0"))) %>%
      layout(paper_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"),
             margin=list(l=20,r=20,t=30,b=20))
  })

  # ── Top 5 importance (sidebar quick view) ────────────────────────────────
  output$plot_top5_imp <- renderPlot({
    bg <- "#161b22"
    top5 <- perm_df %>% head(5)
    ggplot(top5, aes(x=reorder(Feature,Importance), y=Importance,
                      fill=Indicator)) +
      geom_col(alpha=0.85) +
      coord_flip() +
      scale_fill_manual(values=setNames(COLORS_4,
        c("Sea Ice","Amazon Rain","Coral Stress","Soil Carbon"))) +
      labs(x=NULL, y="Importance") +
      theme_eco_shiny() +
      theme(legend.position="none",
            plot.background=element_rect(fill=bg,color=NA),
            panel.background=element_rect(fill=bg,color=NA))
  }, bg="transparent")

  # ── EWS Main Plot ─────────────────────────────────────────────────────────
  output$plot_ews_main <- renderPlotly({
    sel <- input$indicator
    win <- input$ews_window
    sig <- eco_raw[[sel]]
    n   <- length(sig)
    rv  <- ac <- rep(NA_real_, n)
    for(i in win:n) {
      w <- sig[(i-win+1):i]
      rv[i] <- var(w, na.rm=TRUE)
      if(sd(w)>0) ac[i] <- cor(w[-win],w[-1])
    }
    ews_df <- data.frame(
      date     = eco_raw$date,
      Variance = rv / max(rv, na.rm=TRUE),
      AC1      = (ac - min(ac,na.rm=TRUE)) /
                 (max(ac,na.rm=TRUE) - min(ac,na.rm=TRUE) + 1e-9)
    ) %>% filter(!is.na(Variance)) %>%
      pivot_longer(c(Variance,AC1), names_to="EWS", values_to="Value")

    p <- ggplot(ews_df, aes(x=date, y=Value, color=EWS)) +
      geom_line(linewidth=0.8) +
      geom_vline(xintercept=as.numeric(eco_raw$date[241]),
                 color=COL_NEARTIP, linewidth=1.1) +
      scale_color_manual(values=c("Variance"=COL_BLUE,"AC1"=COL_WARN)) +
      scale_y_continuous(labels=percent_format()) +
      labs(x="Date", y="Normalized EWS") +
      theme_eco_shiny()
    ggplotly(p) %>%
      layout(paper_bgcolor="#1a1f2e", plot_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── Kendall Plot ─────────────────────────────────────────────────────────
  output$plot_kendall <- renderPlot({
    ggplot(kendall, aes(x=reorder(paste(Indicator, EWS), Tau),
                          y=Tau, fill=Significant)) +
      geom_col(alpha=0.85, width=0.65) +
      geom_hline(yintercept=0, color="#4a5568") +
      coord_flip() +
      scale_fill_manual(values=c("YES ✓"=COL_STABLE,"no"=COL_NEARTIP)) +
      labs(x=NULL, y="Kendall's Tau", fill="p<0.05?") +
      theme_eco_shiny()
  }, bg="transparent")

  # ── Permutation Importance ─────────────────────────────────────────────
  output$plot_perm_imp <- renderPlotly({
    top_n <- perm_df %>% head(16)
    p <- ggplot(top_n, aes(x=reorder(Feature,Importance),
                             y=Importance, fill=Indicator,
                             text=paste0("Feature: ",Feature,
                                         "\nIndicator: ",Indicator,
                                         "\nEWS: ",EWS_Type,
                                         "\nImportance: ",round(Importance,4)))) +
      geom_col(alpha=0.9) + coord_flip() +
      scale_fill_manual(values=setNames(COLORS_4,
        c("Sea Ice","Amazon Rain","Coral Stress","Soil Carbon"))) +
      labs(x=NULL, y="Permutation Importance") +
      theme_eco_shiny()
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="#1a1f2e", plot_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── Donut ────────────────────────────────────────────────────────────────
  output$plot_donut <- renderPlotly({
    plot_ly(ind_cont, labels=~Indicator, values=~Percentage,
            type="pie", hole=0.55,
            marker=list(colors=COLORS_4,
                        line=list(color="#0d1117", width=2)),
            textinfo="label+percent") %>%
      layout(showlegend=FALSE,
             paper_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── LIME ────────────────────────────────────────────────────────────────
  output$plot_lime <- renderPlotly({
    # Re-run LIME for selected time point on-the-fly (lightweight)
    idx <- which(eco_model_all$month_index == input$time_idx)
    if (length(idx) == 0) idx <- nrow(eco_model_all)

    instance   <- eco_model_all[idx, feature_cols]
    n_perturb  <- 100
    set.seed(7)
    perturbed  <- as.data.frame(lapply(instance, function(v) {
      pmin(pmax(v + rnorm(n_perturb, 0, 0.05), 0), 1)
    }))
    weights <- exp(-rowSums((perturbed - matrix(unlist(instance),
                   nrow=n_perturb, ncol=length(feature_cols), byrow=TRUE))^2)
                   / 0.5^2)
    preds  <- predict(rf_model, perturbed, type="prob")[,"NearTip"]
    lm_fit <- lm(as.formula(paste("y ~", paste(feature_cols,collapse="+"))),
                 data=cbind(perturbed, y=preds), weights=weights)
    coefs  <- coef(lm_fit)[-1]
    lime_df <- data.frame(
      Feature    = names(coefs),
      Coefficient= as.numeric(coefs),
      Direction  = ifelse(as.numeric(coefs)>0,"→ NearTip","→ Stable")
    ) %>% arrange(desc(abs(Coefficient))) %>% head(12)

    p <- ggplot(lime_df, aes(x=reorder(Feature,abs(Coefficient)),
                              y=Coefficient, fill=Direction,
                              text=paste0(Feature,"\nCoef: ",
                                           round(Coefficient,4)))) +
      geom_col(alpha=0.85) + coord_flip() +
      scale_fill_manual(values=c("→ NearTip"=COL_NEARTIP,
                                  "→ Stable" =COL_STABLE)) +
      labs(x=NULL, y="Local Coefficient") +
      theme_eco_shiny()
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="#1a1f2e", plot_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── Instance Details ─────────────────────────────────────────────────────
  output$ui_instance_details <- renderUI({
    idx  <- which(eco_model_all$month_index == input$time_idx)
    if (length(idx)==0) idx <- nrow(eco_model_all)
    row  <- eco_model_all[idx,]
    prob <- row$NearTip_Prob
    tags$div(style="padding:12px; color:#c9d1d9; font-size:13px;",
      tags$h4(style="color:#58a6ff;","Selected Time Point"),
      tags$p(sprintf("📅 Date: %s", format(row$date, "%b %Y"))),
      tags$p(sprintf("🌊 Sea Ice: %.3f Mkm²",     row$sea_ice)),
      tags$p(sprintf("🌧 Amazon Rain: %.1f mm/mo", row$amazon_rain)),
      tags$p(sprintf("🪸 Coral Stress: %.3f",      row$coral_stress)),
      tags$p(sprintf("🌱 Soil Carbon: %.2f",        row$soil_carbon)),
      tags$hr(style="border-color:#30363d;"),
      tags$p(sprintf("🎯 NearTip Probability: %.1f%%", prob*100),
             style=sprintf("color:%s; font-size:16px; font-weight:bold;",
                           ifelse(prob>0.5, COL_NEARTIP, COL_STABLE))),
      tags$p(sprintf("📍 True Tipping State: %s",
                     ifelse(row$tipping_point==1, "Post-Tipping","Pre-Tipping")))
    )
  })

  # ── Changepoint Plot ─────────────────────────────────────────────────────
  output$plot_cp_main <- renderPlotly({
    sel <- input$indicator
    detected <- cps[[sel]]
    df <- eco_raw %>% select(date, month_index, value=all_of(sel))
    p <- ggplot(df, aes(x=date, y=value)) +
      geom_line(color=COL_BLUE, linewidth=0.7) +
      geom_vline(xintercept=as.numeric(eco_raw$date[241]),
                 color=COL_NEARTIP, linewidth=1.2) +
      labs(x="Date", y="Value") +
      theme_eco_shiny()
    if (length(detected) > 0) {
      cp_dates <- eco_raw$date[detected]
      p <- p + geom_vline(xintercept=as.numeric(cp_dates),
                           color=COL_TEAL <- "#68D391",
                           linewidth=1, linetype="dashed")
    }
    ggplotly(p) %>%
      layout(paper_bgcolor="#1a1f2e", plot_bgcolor="#1a1f2e",
             font=list(color="#e2e8f0"))
  })

  # ── Changepoints Table ───────────────────────────────────────────────────
  output$tbl_changepoints <- DT::renderDataTable({
    rows <- lapply(names(cps), function(nm) {
      cps_vec <- cps[[nm]]
      if (length(cps_vec)==0) cps_vec <- NA
      data.frame(
        Indicator = nm,
        Detected_Months = paste(cps_vec, collapse=", "),
        True_TP = 240,
        Closest_Error = ifelse(all(is.na(cps_vec)), NA,
                               min(abs(na.omit(cps_vec) - 240)))
      )
    })
    tbl <- do.call(rbind, rows)
    DT::datatable(tbl, options=list(dom="t", pageLength=10),
                  style="default")
  })

  # ── ROC ──────────────────────────────────────────────────────────────────
  output$plot_roc <- renderPlot({
    roc_df <- data.frame(
      Spec=rev(1 - roc_obj$specificities),
      Sens=rev(roc_obj$sensitivities))
    ggplot(roc_df, aes(x=Spec, y=Sens)) +
      geom_ribbon(aes(ymin=Spec,ymax=Sens), fill=COL_BLUE, alpha=0.2) +
      geom_line(color=COL_BLUE, linewidth=1.3) +
      geom_abline(slope=1, intercept=0, color="#4a5568", linetype="dashed") +
      annotate("text", x=0.6, y=0.2,
               label=sprintf("AUC = %.4f", as.numeric(auc(roc_obj))),
               color=COL_BLUE, size=5, fontface="bold") +
      scale_x_continuous(labels=percent_format()) +
      scale_y_continuous(labels=percent_format()) +
      labs(x="1-Specificity", y="Sensitivity") +
      theme_eco_shiny()
  }, bg="transparent")

  # ── Confusion Matrix Heatmap ─────────────────────────────────────────────
  output$plot_cm <- renderPlot({
    cm_tbl <- as.data.frame(cm$table)
    ggplot(cm_tbl, aes(x=Reference, y=Prediction, fill=Freq,
                         label=Freq)) +
      geom_tile() + geom_text(color="white", size=7, fontface="bold") +
      scale_fill_gradient(low="#1a1f2e", high=COL_BLUE) +
      labs(x="True Label", y="Predicted Label") +
      theme_eco_shiny() +
      theme(legend.position="none")
  }, bg="transparent")

  # ── Metrics Table ─────────────────────────────────────────────────────────
  output$tbl_metrics <- renderTable({
    data.frame(
      Metric    = c("Accuracy","Sensitivity","Specificity",
                    "Precision","F1-Score","AUC"),
      Value     = c(
        sprintf("%.3f", cm$overall["Accuracy"]),
        sprintf("%.3f", cm$byClass["Sensitivity"]),
        sprintf("%.3f", cm$byClass["Specificity"]),
        sprintf("%.3f", cm$byClass["Pos Pred Value"]),
        sprintf("%.3f", cm$byClass["F1"]),
        sprintf("%.4f", as.numeric(auc(roc_obj)))
      )
    )
  }, striped=TRUE, hover=TRUE, bordered=TRUE)

} # end server

# =============================================================================
# RUN
# =============================================================================
shinyApp(ui, server)
