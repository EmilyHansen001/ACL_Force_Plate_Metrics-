### APPP FINAL 
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(DT)
library(janitor)
library(lubridate)

# -----------------------------
# Load master dataset
# -----------------------------

master_data <- read_csv(
  "r_project/master_FINAL.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    group = case_when(
      group == "Subject" ~ "ACL",
      TRUE ~ group
    )
  )

# -----------------------------
# Load demographics
# -----------------------------

demo <- read_csv(
  "demographics/demographic_data.csv",
  show_col_types = FALSE
) %>%
  clean_names()

demo_clean <- demo %>%
  slice(-1) %>%
  rename(
    id = q1,
    sex = q4,
    sport = q6,
    group = q7,
    graft = q9,
    affected_limb = q11
  ) %>%
  select(id, sex, sport, group, graft, affected_limb) %>%
  mutate(
    id = str_trim(id),
    group = case_when(
      group == "Subject" ~ "ACL",
      group == "Control" ~ "Control",
      TRUE ~ group
    ),
    affected_limb = str_trim(affected_limb)
  )

# -----------------------------
# Load raw timeline data
# -----------------------------

files <- list.files(
  "jump_data_clean",
  pattern = "^S.*\\.csv$",
  full.names = TRUE
)

keep_cols <- c(
  "Date", "Time", "ID",
  "Jump Height",
  "Left Avg. Propulsive Force",
  "Right Avg. Propulsive Force",
  "Left Avg. Braking Force",
  "Right Avg. Braking Force",
  "Left Avg. Landing Force",
  "Right Avg. Landing Force"
)

read_jump_file <- function(file) {
  read_csv(
    file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    select(any_of(keep_cols))
}

timeline_raw <- map_df(files, read_jump_file) %>%
  clean_names() %>%
  mutate(
    id = str_trim(id),
    session_datetime = mdy_hms(paste(date, time)),
    across(
      c(
        jump_height,
        left_avg_propulsive_force,
        right_avg_propulsive_force,
        left_avg_braking_force,
        right_avg_braking_force,
        left_avg_landing_force,
        right_avg_landing_force
      ),
      ~ as.numeric(str_replace_all(.x, "[^0-9.-]", ""))
    )
  ) %>%
  filter(!is.na(id), !is.na(session_datetime)) %>%
  left_join(demo_clean, by = "id") %>%
  arrange(id, session_datetime) %>%
  group_by(id) %>%
  mutate(session_num = row_number()) %>%
  ungroup() %>%
  mutate(
    prop_asym = case_when(
      group == "Control" ~
        (left_avg_propulsive_force - right_avg_propulsive_force) /
        (left_avg_propulsive_force + right_avg_propulsive_force) * 100,
      group == "ACL" & affected_limb == "Left" ~
        (right_avg_propulsive_force - left_avg_propulsive_force) /
        (right_avg_propulsive_force + left_avg_propulsive_force) * 100,
      group == "ACL" & affected_limb == "Right" ~
        (left_avg_propulsive_force - right_avg_propulsive_force) /
        (left_avg_propulsive_force + right_avg_propulsive_force) * 100
    ),
    brake_asym = case_when(
      group == "Control" ~
        (left_avg_braking_force - right_avg_braking_force) /
        (left_avg_braking_force + right_avg_braking_force) * 100,
      group == "ACL" & affected_limb == "Left" ~
        (right_avg_braking_force - left_avg_braking_force) /
        (right_avg_braking_force + left_avg_braking_force) * 100,
      group == "ACL" & affected_limb == "Right" ~
        (left_avg_braking_force - right_avg_braking_force) /
        (left_avg_braking_force + right_avg_braking_force) * 100
    ),
    land_asym = case_when(
      group == "Control" ~
        (left_avg_landing_force - right_avg_landing_force) /
        (left_avg_landing_force + right_avg_landing_force) * 100,
      group == "ACL" & affected_limb == "Left" ~
        (right_avg_landing_force - left_avg_landing_force) /
        (right_avg_landing_force + left_avg_landing_force) * 100,
      group == "ACL" & affected_limb == "Right" ~
        (left_avg_landing_force - right_avg_landing_force) /
        (left_avg_landing_force + right_avg_landing_force) * 100
    )
  )

# -----------------------------
# UI
# -----------------------------

ui <- navbarPage(
  "ACL Force Plate Explorer",
  
  tabPanel(
    "About",
    fluidPage(
      h2("ACL Force Plate Dataset"),
      p("This app explores countermovement jump force plate data from ACL-reconstructed athletes and healthy controls."),
      p("The goal is to examine whether ACL athletes demonstrate persistent limb asymmetry despite similar overall jump performance."),
      
      wellPanel(
        h4("How to Use This App"),
        p("Use the tabs above to compare ACL and control athletes, explore injured versus uninjured limb force, and view individual athlete trends over time."),
        p("Boxplots show group distributions, individual dots represent athletes, and statistical tests help evaluate whether observed differences are meaningful."),
        p("For asymmetry values, numbers closer to zero indicate greater symmetry.")
      ),
      
      h4("Dataset Features"),
      tags$ul(
        tags$li("Countermovement jump force plate data"),
        tags$li("ACL-reconstructed athletes and healthy controls"),
        tags$li("Jump height, force variables, and limb asymmetry metrics"),
        tags$li("Repeated testing sessions for longitudinal athlete trends")
      )
    )
  ),
  
  tabPanel(
    "Main Findings",
    fluidPage(
      h2("Main Findings"),
      
      wellPanel(
        h4("Overall Summary"),
        tags$ul(
          tags$li("ACL athletes showed similar overall jump performance compared to controls."),
          tags$li("Within the ACL group, the injured limb tended to produce less force than the uninjured limb."),
          tags$li("Propulsive force appeared to be the most sensitive phase for detecting persistent limb deficits."),
          tags$li("Individual athlete timelines revealed that asymmetry patterns vary across athletes and across sessions."),
          tags$li("The method used to calculate asymmetry changes how deficits are detected and interpreted.")
        )
      ),
      
      wellPanel(
        h4("Clinical Interpretation"),
        p("These results suggest that athletes may appear recovered based on overall jump performance, while still demonstrating limb-specific deficits."),
        p("Injury-specific comparisons, such as uninjured versus injured limb force, may reveal asymmetries that general ACL versus control comparisons do not detect."),
        p("The propulsive phase may be especially important to examine during return-to-sport assessment and rehabilitation monitoring.")
      ),
      
      wellPanel(
        h4("Important Note"),
        p("This app is designed for exploratory analysis. Statistical results should be interpreted alongside sample size, variability, and clinical context.")
      )
    )
  ),
  
  tabPanel(
    "Group Comparisons",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          h4("How to Use This Tab"),
          p("Select a variable to compare ACL and control athletes."),
          p("Boxplots show the distribution for each group. Dots represent individual athletes."),
          p("A Welch two-sample t-test is shown below the plot.")
        ),
        selectInput(
          "group_var",
          "Choose Variable:",
          choices = c(
            "Jump Height" = "jump_height",
            "mRSI" = "m_rsi",
            "Peak Propulsive Force" = "peak_propulsive_force",
            "Peak Landing Force" = "peak_landing_force",
            "Propulsive Asymmetry" = "l_r_avg_propulsive_force",
            "Braking Asymmetry" = "l_r_avg_braking_force",
            "Landing Asymmetry" = "l_r_avg_landing_force"
          )
        )
      ),
      mainPanel(
        plotOutput("group_plot"),
        h4("Welch Two-Sample T-Test"),
        verbatimTextOutput("group_test")
      )
    )
  ),
  
  tabPanel(
    "ACL Limb Deficits",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          h4("How to Use This Tab"),
          p("This tab compares injured and uninjured limb force within ACL athletes."),
          p("Because both values come from the same athlete, a paired t-test is used."),
          p("This helps determine whether the reconstructed limb shows reduced force production.")
        ),
        selectInput(
          "limb_metric",
          "Choose Force Phase:",
          choices = c(
            "Propulsive Force" = "prop",
            "Braking Force" = "brake",
            "Landing Force" = "land"
          )
        )
      ),
      mainPanel(
        plotOutput("limb_plot"),
        h4("Paired T-Test"),
        verbatimTextOutput("limb_test")
      )
    )
  ),
  
  tabPanel(
    "Athlete Timeline",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          h4("How to Use This Tab"),
          p("Select an athlete and metric to view changes across repeated testing sessions."),
          p("Gray lines represent other athletes in the same group."),
          p("The black line represents the selected athlete."),
          p("The blue line represents the average trend for the selected athlete's group."),
          p("For asymmetry metrics, values closer to zero indicate greater symmetry.")
        ),
        selectInput(
          "athlete_id",
          "Choose Athlete:",
          choices = sort(unique(timeline_raw$id))
        ),
        selectInput(
          "timeline_metric",
          "Choose Metric:",
          choices = c(
            "Propulsive Asymmetry" = "prop_asym",
            "Braking Asymmetry" = "brake_asym",
            "Landing Asymmetry" = "land_asym",
            "Jump Height" = "jump_height"
          )
        ),
        checkboxInput(
          "show_group_mean",
          "Show group average trend",
          value = TRUE
        )
      ),
      mainPanel(
        plotOutput("timeline_plot"),
        h4("Selected Athlete Summary"),
        tableOutput("athlete_summary")
      )
    )
  ),
  
  tabPanel(
    "Data Table",
    fluidPage(
      wellPanel(
        h4("How to Use This Tab"),
        p("This tab allows users to view the cleaned dataset used in the app."),
        p("Users can search, sort, and inspect variables directly.")
      ),
      DTOutput("data_table")
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output) {
  
  output$group_plot <- renderPlot({
    plot_data <- master_data %>%
      filter(!is.na(.data[[input$group_var]]), !is.na(group))
    
    ggplot(plot_data, aes(x = group, y = .data[[input$group_var]], fill = group)) +
      geom_boxplot(alpha = 0.7, width = 0.6) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
      theme_classic() +
      labs(
        x = "",
        y = input$group_var,
        title = "ACL vs Control Comparison"
      ) +
      theme(legend.position = "none")
  })
  
  output$group_test <- renderPrint({
    test_data <- master_data %>%
      filter(!is.na(.data[[input$group_var]]), !is.na(group))
    
    formula <- as.formula(paste(input$group_var, "~ group"))
    t.test(formula, data = test_data)
  })
  
  output$limb_plot <- renderPlot({
    acl_data <- master_data %>%
      filter(group == "ACL")
    
    if (input$limb_metric == "prop") {
      plot_data <- acl_data %>%
        select(id, injured_prop, uninjured_prop)
    } else if (input$limb_metric == "brake") {
      plot_data <- acl_data %>%
        select(id, injured_brake, uninjured_brake)
    } else {
      plot_data <- acl_data %>%
        select(id, injured_land, uninjured_land)
    }
    
    plot_data <- plot_data %>%
      pivot_longer(cols = -id, names_to = "limb", values_to = "force") %>%
      filter(!is.na(force)) %>%
      mutate(
        limb = case_when(
          str_detect(limb, "uninjured") ~ "Uninjured",
          str_detect(limb, "injured") ~ "Injured"
        )
      )
    
    ggplot(plot_data, aes(x = limb, y = force, fill = limb)) +
      geom_boxplot(alpha = 0.7, width = 0.6) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
      theme_classic() +
      labs(
        x = "",
        y = "Force (N)",
        title = "Injured vs Uninjured Limb Force"
      ) +
      theme(legend.position = "none")
  })
  
  output$limb_test <- renderPrint({
    acl_data <- master_data %>%
      filter(group == "ACL")
    
    if (input$limb_metric == "prop") {
      t.test(acl_data$injured_prop, acl_data$uninjured_prop, paired = TRUE)
    } else if (input$limb_metric == "brake") {
      t.test(acl_data$injured_brake, acl_data$uninjured_brake, paired = TRUE)
    } else {
      t.test(acl_data$injured_land, acl_data$uninjured_land, paired = TRUE)
    }
  })
  
  output$timeline_plot <- renderPlot({
    selected_data <- timeline_raw %>%
      filter(id == input$athlete_id)
    
    group_of_selected <- selected_data$group[1]
    
    background_data <- timeline_raw %>%
      filter(group == group_of_selected)
    
    p <- ggplot() +
      geom_line(
        data = background_data,
        aes(x = session_num, y = .data[[input$timeline_metric]], group = id),
        alpha = 0.2,
        color = "gray50"
      ) +
      geom_point(
        data = background_data,
        aes(x = session_num, y = .data[[input$timeline_metric]], group = id),
        alpha = 0.2,
        color = "gray50"
      ) +
      geom_line(
        data = selected_data,
        aes(x = session_num, y = .data[[input$timeline_metric]]),
        color = "black",
        linewidth = 1.4
      ) +
      geom_point(
        data = selected_data,
        aes(x = session_num, y = .data[[input$timeline_metric]]),
        color = "black",
        size = 2
      ) +
      theme_classic() +
      labs(
        title = paste("Timeline for Athlete", input$athlete_id),
        subtitle = paste("Group:", group_of_selected),
        x = "Session Number",
        y = input$timeline_metric
      )
    
    if (input$timeline_metric != "jump_height") {
      p <- p + geom_hline(yintercept = 0, linetype = "dashed")
    }
    
    if (input$show_group_mean) {
      p <- p +
        stat_summary(
          data = background_data,
          aes(x = session_num, y = .data[[input$timeline_metric]]),
          fun = mean,
          geom = "line",
          linewidth = 1.2,
          color = "blue"
        )
    }
    
    p
  })
  
  output$athlete_summary <- renderTable({
    master_data %>%
      filter(id == input$athlete_id) %>%
      select(
        id,
        group,
        sex,
        sport,
        graft,
        affected_limb,
        jump_height,
        injured_prop,
        uninjured_prop,
        asym_prop_inj
      )
  })
  
  output$data_table <- renderDT({
    datatable(master_data)
  })
}

shinyApp(ui = ui, server = server)