library(shiny)
library(bslib)
library(rms)
library(survminer)
library(survival)


# Define UI for app that draws a histogram ----
ui <- page_sidebar(# App title ----
                   #title = "r",# Sidebar panel for inputs ----
                   ui <- fluidPage(
                       # App title ----
                       titlePanel("IDAS: Ischemia and Donor Age - Interactive Assessment Tool for Heart Transplant Survival")
                   ),
                   
                   # Sidebar layout with input and output definitions ----
                   sidebarLayout(# Sidebar panel for inputs ----
                                 sidebarPanel(# Input: Slider for the number of bins ----
                                              fluidRow(
                                                  column(
                                                      6,
                                                      sliderInput(
                                                          "donage",
                                                          "Donor age (years):",
                                                          min = 15,
                                                          max = 85,
                                                          value = 30
                                                      ),
                                                      sliderInput(
                                                          "donw",
                                                          "Donor weight (kg):",
                                                          min = 40,
                                                          max = 150,
                                                          value = 80
                                                      ),
                                                      radioButtons("cmv", "CMV status donor:", choices = c("(-)", "(+)")),
                                                      selectInput(
                                                          "sexmatch",
                                                          "Recipient donor match:",
                                                          choices = c("Match", "Female to male", "Male to female")
                                                      )
                                                  ),
                                                  column(
                                                      6,
                                                      sliderInput(
                                                          "recage",
                                                          "Recipient age (years):",
                                                          min = 18,
                                                          max = 85,
                                                          value = 40
                                                      ),
                                                      sliderInput(
                                                          "bmi",
                                                          HTML(paste0("Recipient BMI (kg/m",tags$sup("2"),')')),
                                                          min = 10,
                                                          max = 50,
                                                          value = 25
                                                      ),
                                                      sliderInput(
                                                          "crea",
                                                          "Creatinine (µmol/L):",
                                                          min = 50,
                                                          max = 800,
                                                          value = 110
                                                      ),
                                                      selectInput(
                                                          "diagn",
                                                          "Diagnosis:",
                                                          choices = c("Dilated" = "Cardiomyopathy", "Ischemic" = "CAD", "Graftfailure" = "Graftfailure", "Congenital" = "Congenital", "Other" = "Misc", "Valve" = "Valve"),
                                                      ),
                                                      radioButtons("diab", "Diabetes:", choices = c("No", "Yes")),
                                                      radioButtons("pra", "PRA ≥10%:", choices = c("No", "Yes")),
                                                      radioButtons("icu", "ICU pre-HT:", choices = c("No", "Yes")),
                                                      radioButtons("ecmo", "ECMO pre-HT:", choices = c("No", "Yes"))
                                                  )
                                              ), width = 5), 
                                 # Main panel for displaying outputs ----
                                 mainPanel(tags$p("The IDAS (Ischemia and Donor Age - Interactive Assessment Tool for Heart Transplant Survival) calculator is a tool designed to assess the suitability of a donor for heart transplantation depending on the expected ischemic time. This interactive and dynamic nomogram is based on a comprehensive analysis of 83,761 heart transplants from International Society of Heart Transpantation (ISHLT) registry, integrating key variables to enhance clinical decision-making. The study is published in Journal of XXXX xxxx;xxx:xxx–xxx"), tags$b("Survival Probability by Ischemic Time for Heart Transplant Recipients"), plotOutput(outputId = "distPlot"),tags$b("The table presents survival estimates (%) for various ischemia intervals"),  # Add a title for the table
                                           tableOutput(outputId = "survTable"), 
                                           width = 7)
                   )
                   #mainPanel(combineOutput("distPlot","survTable"))width = 4)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # Define a new dataset for prediction
        new_data <- data.frame(
            donage = rep(input$donage, 4),
            ischemictime_cat = factor(c("0-2h", "2-3h", "3-4h", ">=4h")),
            #levels = levels(df_sub$ischemictime_cat)),
            recageyear = rep(input$recage, 4),
            bmi = rep(input$bmi, 4),
            diagn = factor(rep(input$diagn, 4)),
            recdiabetes = factor(rep(input$diab, 4)),
            recpra10 = factor(rep(input$pra, 4)),
            mostreccreat_swe = rep(input$crea, 4),
            rececmo = factor(rep(input$ecmo, 4)),
            recmedcondicu = factor(rep(input$icu, 4)),
            donweight = rep(input$donw, 4),
            doncmv = factor(rep(input$cmv, 4)),
            gendermatch = factor(rep(input$sexmatch, 4)),
            tx_year = rep(2020, 4)
        )
        
        # Customize the theme for ggsurvplot
        custom_theme <- theme_minimal() +
            theme(
                plot.title = element_text(
                    size = 16,
                    face = "bold",
                    hjust = 0.5
                ),
                plot.subtitle = element_text(
                    size = 14,
                    face = "italic",
                    hjust = 0.5
                ),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 10),
                legend.position = "bottom"
            )
        fs_adj_red2 <- readRDS(file = "fs_adj_red.rds")
        surv_fit_newdata <- survfit(fs_adj_red2, newdata = new_data)
        
        # Define time points for 1, 3, and 5 years
        time_points <- c(1, 3, 5) * 365.25  # assuming time is in days
        
        # Calculate survival probabilities
        surv_estimates <- survest(
            fs_adj_red2,
            newdata = new_data,
            times = time_points,
            conf.int = FALSE
        )
        # Define the survival plot
        surv_plot <- ggsurvplot(
            surv_fit_newdata,
            data = new_data,
            #linetype = c("solid","dashed","dotted","dotdash"),
            palette = "lancet",
            # grey, hue, Blue, RdBu
            break.time.by = 2,
            surv.scale = c("percent"),
            # ("default", "percent")
            conf.int = F,
            lwd = 1.5,
            #conf.int.fill = c("gray"),
            conf.int.style = "ribbon",
            censor = F,
            pval = F,
            #pval.size = 4,
            pval.coord = c(NULL, NULL),
            main = NULL,
            xlab = "Time after transplantation (years)",
            ylab = "Patient surviva (%)",
            font.main = c(18, "plain", "black"),
            font.x = c(18, "plain", "black"),
            font.y = c(18, "plain", "black"),
            font.tickslab = c(16, "plain", "black"),
            xlim = c(0, 12),
            ylim = c(0, 1),
            legend = c("bottom"),
            # ("top", "bottom", "left", "right","none")
            legend.title = "Duration ischemia (hours)",
            legend.labs = c("0-<2h", "2-<3h", "3-<4h", "≥4h"),
            font.legend = c(16, "plain", "black"),
            risk.table = F,
            # risk.table.title = NULL,
            # risk.table.col = "black",
            # risk.table.fontsize = 3.0,
            # risk.table.y.text = F,
            # risk.table.y.text.col = T,
            # risk.table.height = 0.30,
            #surv.plot.height = 0.77,
            ncensor.plot = F,
            #ncensor.plot.height = 0.30,
            surv.median.line = c("hv"),
            # "none", "hv", "h", "v"
            #ggtheme = theme_classic())
            ggtheme = custom_theme
        )
        # Plot the survival curve
        surv_plot
    })
    # Render the survival table
    output$survTable <- renderTable({
        new_data <- data.frame(
            donage = rep(input$donage, 4),
            ischemictime_cat = factor(c("0-2h", "2-3h", "3-4h", ">=4h")),
            #levels = levels(df_sub$ischemictime_cat)),
            recageyear = rep(input$recage, 4),
            bmi = rep(input$bmi, 4),
            diagn = factor(rep(input$diagn, 4)),
            recdiabetes = factor(rep(input$diab, 4)),
            recpra10 = factor(rep(input$pra, 4)),
            mostreccreat_swe = rep(input$crea, 4),
            rececmo = factor(rep(input$ecmo, 4)),
            recmedcondicu = factor(rep(input$icu, 4)),
            donweight = rep(input$donw, 4),
            doncmv = factor(rep(input$cmv, 4)),
            gendermatch = factor(rep(input$sexmatch, 4)),
            tx_year = rep(2020, 4)
        )
        # Load the prediction model
        fs_adj_red2 <- readRDS(file = "fs_adj_red.rds")
        # Define time points for 1, 3, and 5 years
        time_points <- c(1, 3, 5, 10)  # time is in years
        # Performing the survival etimates for the different time points
        surv_estimates <- survest(
            fs_adj_red2,
            newdata = new_data,
            times = time_points,
            conf.int = FALSE
        )
        tmp<-surv_estimates$surv*100 # convert to percentage
        rownames(tmp) <- c("0-<2h", "2-<3h", "3-<4h", "≥4h")
        colnames(tmp) <- c("1-year", "3-year", "5-year", "10-year")
        print(tmp, digit=2)
}, rownames = TRUE, digits = 1, align = "c")
}

shinyApp(ui = ui, server = server)
