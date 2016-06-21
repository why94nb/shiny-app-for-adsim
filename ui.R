require(blockrand)
require(plotly)
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Data Visualization for Rpackage 'adsim'",titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",tabName = "about",icon = icon("th")),
      menuItem("Simulation Setting Up", tabName = "setup", icon = icon("line-chart")),
      menuItem("Illustrative Statistics", tabName = "summary", icon = icon("pie-chart")),
      menuItem("Simulation Summary",tabName = "simsum", icon = icon("bar-chart")),
      menuItem("Patient Table", tabName = "result", icon = icon("table")),
      downloadButton("downloadfulltable","Download Full Table"),
      radioButtons('format', 'Document format', c('PDF')),
      downloadButton("downloadreport","Download Report")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(tabName = "about",
              tabsetPanel(
                tabPanel("Adsim Package Background",
                         fluidRow(
                           box(width = 12,status = "primary", solidHeader = T,
                               title = "Introduction",
                               p("The adsim package was developed through the Modeling and Simulation Workgroup of the Coalition Against Major Diseases (CAMD) consortia, part of the Critical Path Institute. It was developed to describe the longitudinal progression of the 11 item Alzheimer's Disease (AD) Assessment Scale cognitive sub-scale (ADAS-cog) in AD patients in both natural history and randomized clinical trial settings, utilizing both individual patient and summary level literature data, and to simulate AD clinical trials."),
                               p( "Patient data from the CAMD database (3223 patients), the Alzheimer's Disease Neruroimaging Initiative (ADNI) Study database (186 patients), and summary data from 73 literature references (representing 17,235 patients) were used during model development. Unique to this workm the  model also underwent formal review processes from both EMA and FDA, with input from both agencies during its development cycle."),
                               p("The model developed utilized a Beta Regression (BR) drug-disease-trial model (ref). The model allowed for simultaneous fitting of summary and patient level data, allowing for integration of all information available. A further advantage of the BR model was that it constrained values to the range of the original instrument for simulation purposes, in contrast to methodologies that provide appropriate constraints only for conditional expectations. Treatment effects for currently available acetyl cholinesterase inhibitors, longitudinal changes in disease severity, dropout rate, placebo effect, and factors influencing these parameters were estimated in the model."),
                               p("Based on predictive checks and external validation, an adequate BR meta-analysis model for ADAS-cog using both summary-level and patient-level data was developed. Baseline ADAS-cog was estimated from baseline MMSE score. Disease progression was dependent on time, APOe4 status, age, and gender. Study drop out was a function of time, baseline age, and baseline MMSE. The use of the BR constrained simulations to the 0-70 range of the ADAS-cog, even when residuals were incorporated. The model underwent formal evaluation for suitability of use by both the FDA and EMA, and was deemed suitable for use by both agencies.
                                 ")),
                           box(width = 6,title = "Development of a User Interface",height = 170,
                               status = "primary", solidHeader = T,
                               p("User feedback to the Modeling and Simulation Workgroup of CAMD suggested that a user interface may make the package more accessible by those less familiar with the use of statistical software such as R. Feedback also suggested that it may be useful to automate production of some of the plots and figures that normally would be developed as part of a trial simulation exercise. ")
                               ),
                           box(width = 6,title = "Resources",height = 170,
                               status = "primary", solidHeader = T,
                               strong("Github(Code for both shiny app and 'adsim' package):"),tags$a(href="https://github.com/why94nb/", "https://github.com/why94nb/"),
                               br(),
                               strong("Documentation for the adsim package:"),tags$a(href="//bitbucket.org/metrumrg/alzheimers-disease-progression-model-adascog/wiki/Home ","//bitbucket.org/metrumrg/alzheimers-disease-progression-model-adascog/wiki/Home"),
                               br(),
                               strong("Link to Original Article (Journal of Pharmacokinetics and Pharmacodynamics):"),tags$a(href=" http://www.ncbi.nlm.nih.gov/pubmed/22821139","http://www.ncbi.nlm.nih.gov/pubmed/22821139")
                               )
                               )
                ),
                tabPanel("User Guide",
                         
                         fluidRow(
                           box(width = 12, status = "primary", solidHeader = T,
                               title = "Background",
                               p("This section describes the basic use of the adsimR package. adsimR offers users the option to perform longitudinal clincial trial simulations of the ADAS-Cog-11 with different types of trial designs and drug effects being simulated. It also generates a short report defining the parameters used, summarizing outcomes of the simulations in tables and figures, and includes source code."),
                               p("There are 5 tabs located on the left of the screen. The user is intended to use the tabs in order to complete the simulation activities. An explanation of each tab is below. Please notice that all the plots and tables from the app are downloadable.")
                           ),
                           column(width = 7,
                         box(width = NULL, status = "primary", solidHeader = T,
                             title = "Simulation Setting Up",
                             p("This screen allows the user to define all the parameters required for the simulation."),
                             strong("Test vs Run: "),p("If you plan to run a large number of trial simulations, (eg 1000 ) it is highly recommended to test your settings first by clickingthe  'Test' button, large simulations will take time. The user can test and adjust settings, according to the outputs listed on the right side(fitted curves for each group and statistical test of contrast at the end point) as well as on session Illustrative Statistics and Patient Table."),
                             strong("Set Seed:"),p(" The user can set seed before running simulation for reproducibility."),
                             strong("Trial Design: "),p("Three types of trial design are available: Cross-over design, parallel group design and delayed start design. If cross-over design is chosen, then the user also need to specify first period duration, washout period and assessment frequency(eg, if it's set to be 3, then Adas-Cog will be assessed every 3 weeks. Notice: 2*FirstPeriodDuration + WashoutPeriod should be divisible by assessment frequency because of computational issue). If parallel group design is chosen, then the user should specify simulation duration. If delayed start design is chosen, then the user need to specify when the first assessment begins(starting point), how long the assessment lasts(assessment period) as well as washout period."),
                             strong("Drug Effect: "),p("Two types of drug effects are available: Symptomatic effect and disease modifying effect. The user should choose at least one of the two effects. Parameters related to symptomatic effect include emax value(the maximum possible effect), ET 50(Time to 50 % of maximum drug effect) and ET50 washout(). Parameters related to disease modifying effect is proportional decrease. "),
                             strong("Patients per Group: "),p("The user can specify how many patients in each group(placebo and treatment)."),
                             strong("Baseline MMSE range:"),p(" Mini-Mental Status Examination; 30-point questionnaire used to measure the severity of cognitive impairment, commonly used in AD trials."),
                             strong("Whether dropout:"),p("Time when a patient withdraw from the trial (missing data mechanism: MAR). The user can choose whether to take dropout of patients into account."),
                             strong("Number of Simulations: "),p("The user can specify number of simulations to run.")
                         )
                         ),
                         column(width = 5,
                         box(width = NULL, status = "primary", solidHeader = T,
                             title = "Illustrative Statistics",
                             p("In this session, spaghetti plots as well as summary baseline information about patients
                               are offered."),
                             strong("These plots will only be shown if user presses test button.")
                         ),
                         box(width = NULL, status = "primary", solidHeader = T,
                             title = "Simulation Summary",
                             p("Spaghetti plot is available, and contrast information(Difference between placebo and treatment group at the end time point) of each simulation with 95% CI
                               is plotted, summary of statistical test(Whether contrast is significant) is also given."),
                             strong("Again these plots will only be shown if user presses run button.")
                         ),
                         box(width = NULL, status = "primary", solidHeader = T,
                             title = "Patient Table",
                             p("The user is provided with two types of tables based on the simulations."),
                             p(".	The basic information table contains all the information used in the simulation regarding patient characteristics at baseline including age, gender, MMSE and ApoE. "),
                             p(".	The longitudinal data table contains ADAS-Cog information at each time point"),
                             p("The user can download either table for further analysis outside of the application.")
                         ),
                         box(width = NULL, status = "primary", solidHeader = T,
                            title = "Report",
                             p("User can download report containing all the outputs generated during the simulation. PDF
                               format is supported. ")
                            ))
              )
              ))),
      
      tabItem(tabName = "setup",
         fluidRow(
            column(width=3,
                box(width = NULL,status = "primary", solidHeader = T,
                    title="Setting Up", 
                    numericInput("seed","Please Set Seed:",value = 1234),
                    radioButtons("design", "Please Choose Trial Design", 
                                 c("Cross-over Design","Parallel Group Design",
                                   "Delayed Start Design")),
                    checkboxGroupInput("drug","Please Choose Drug Effects",
                                       c("Symptomatic","Disease Modifying"),
                                       selected = "Symptomatic"),
                    helpText("Please choose at least one drug effects"),
                    numericInput("n.per.arm", "Patients per Group:", value = 100),
                    sliderInput("bmmse","Baseline MMSE range:",
                                min = 1,max = 30,value = c(14,26)),
                    radioButtons("dropout", "Whether Dropout:", c("Yes","No")),
                    numericInput("nSim","Number of Simulations:", value = 1000)
                ),
                actionButton("goButton", "Test"),
                actionButton("goButton1", "Run")
                ),
        
              column(width=3, 
                box(width = NULL,status = "primary", solidHeader = T,collapsible = T,
                    p("The figure on the right hand side shows the fitted curve using 
                      nonlinear methods such as 'loess' or 'gam'.
                    A statistical test is also conducted to test whether the
                      difference of Adas-Cog scores for the two groups at the end of
                       the trial is significant or not."),
                    strong("These figure and table will only be shown if user click test button.")),
                box(width = NULL,status = "primary", solidHeader = T,
                 conditionalPanel(
                   condition = "input.design == 'Cross-over Design'",
                    numericInput("firstduration","First period Duration(Weeks):",
                                 value = 12),
                    numericInput("frequency", "Assessment Frequency(Week):", value = 3),
                    helpText("Notice:2*FirstPeriodDuration + 
                              WashoutPeriod should be divisible by
                              assessment frequency because of computational issue"),
                    numericInput("washout", "Washout Period(Week):", value = 3),
                    helpText("Notice:Upper bound of baseline MMSE should be no less than 20 because of 
                             computational issue")
                ),
                
                conditionalPanel(
                  condition = "input.design == 'Parallel Group Design'",
                  numericInput("duration","Duration(Weeks):",value = 84)
                ),
                
                conditionalPanel(
                  condition = "input.design == 'Delayed Start Design'",
                  numericInput("start","Delayed Start Point(Week):",value = 52),
                  numericInput("period","Trial Duration(Weeks):",value = 91)
                ),
                
                conditionalPanel(
                  condition = "input.drug.includes('Symptomatic')",
                    numericInput("Emax2","Emax Value for Treatment Group:", value = -2),
                    numericInput("et50","ET50:", value = 1),
                    numericInput("et50wash","ET50 Washout:", value = 1)
                ),
                
                conditionalPanel(
                  condition = "input.drug.includes('Disease Modifying')",
                  sliderInput("eDm2","Proportional Decrease for Treatment Group:", min = 0,
                              max = 1, value = 0.5)
                )
                )
                ),
                
         
          column(width = 6,
           box(width = NULL,status = "primary", solidHeader = T,
               plotlyOutput("plot1"),collapsible = T),
           box(width = NULL, status = "primary", solidHeader = T,
               DT::dataTableOutput("table1"),collapsible = T)
                )
         )
),
       
      tabItem(tabName = "summary",
              fluidRow(
                box(width = 8, title = "Patient-level Spaghetti Plot",collapsible = T,
                    status = "primary", solidHeader = T,
                    plotlyOutput("summary2")),
                box(width = 4, title = "Gender Information",
                    status = "primary", solidHeader = T,
                    plotlyOutput("summary1")),
                box(width = 4, title = "ApoE Proportion",
                    status = "primary", solidHeader = T,
                    plotlyOutput("summary3")),
                box(width = 4, title = "Age Information",
                    status = "primary", solidHeader = T,
                    plotlyOutput("summary4")),
                box(width = 4, title = "Baseline MMSE Information",
                    status = "primary", solidHeader = T,
                    plotlyOutput("summary5"))
                                )
                                ),
 
      tabItem(tabName = "result",
          tabsetPanel(
            tabPanel("Baseline Information",
            fluidRow(
              box(width = 12,DT::dataTableOutput("table2"),
                  downloadButton("download1")))
      ),
      tabPanel("Longitudinal Data",
               box(width = 12,DT::dataTableOutput("table3"),
                   downloadButton("download2")))
      )
      ),
      tabItem(tabName = "simsum",
              fluidRow(
                box(width = 6, title = "Trial-level Spaghetti Plot",collapsible = T,
                    status = "primary", solidHeader = T,
                    plotlyOutput("simsum1")),
                box(width = 6, title = "Trial-level Spaghetti Plot",collapsible = T,
                    status = "primary", solidHeader = T,
                    plotlyOutput("simsum4")),
                box(width = 7, title = "Contrast Information",collapsible = T,
                    status = "primary", solidHeader = T,
                    p("For each simulation, the average difference of AdasCog score of two groups
                       as well as 95% confidence interval are calculated."),
                    plotlyOutput("simsum2")),
                box(width = 5, title = "P-value Summary",collapsible = T,
                    status = "primary", solidHeader = T,
                    p("For each simulation,a statistical test is also conducted to test whether the difference of Adas-Cog scores for the two groups at the end of the trial is significant or not.
                       Proportion of P-values of the tests are summarized as the figure below."),
                    plotlyOutput("simsum3")),
                conditionalPanel(
                  condition = "input.design == 'Delayed Start Design'",
                  box(width = 7, title = "Contrast Information",collapsible = T,
                      status = "primary", solidHeader = T,
                      plotlyOutput("simsum5")),
                  box(width = 5, title = "P-value Summary",collapsible = T,
                      status = "primary", solidHeader = T,
                      plotlyOutput("simsum6"))
                )
              ))
    )
  )
)

