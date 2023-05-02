############################################
############################################
##
## BBRO BEETGRO MODEL - TO A SHINY SERVER
##
## v2023-04-28. First attempt. Goal is to have something that work for 905 NBYC
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
##
############################################
############################################

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(shinyauthr)

# DEVELOPMENT DATA
# param <- pivot_wider(read_xlsx("./905_NBYC_Shiny_local/www/parameters.xlsx", sheet = "parameters"), names_from = parameter)
# trial <- read_xlsx("./905_NBYC_Shiny_local/www/TrialData.xlsx", sheet = "TrialData") 
# users <- unique(trial$user)
# weath <- read_xlsx("./905_NBYC_Shiny_local/www/Site001.xlsx", sheet = "Sheet1")
# mod_vars <- c("evap_actual","soil_md","canopy_cover","yield_sugar","yield_s_pct_dm","yield_biom")

# RunApp DATA
param <- pivot_wider(read_xlsx("./www/parameters.xlsx", sheet = "parameters"), names_from = parameter)
trial <- read_xlsx("./www/TrialData.xlsx", sheet = "TrialData")
users <- unique(trial$user_text)
weath <- read_xlsx("./www/Site001.xlsx", sheet = "Sheet1")
mod_vars <- c("Temp., Mean","Precipitation", "Canopy Cover", "Root Depth", "Evapotranspiration", "Soil Moisture Deficit", "Yield - Biomass", "Yield - Sugar", "Sugar % DM")
mod_vars_ <- c("temp_x","precip", "canopy_cover", "root_depth", "evap_actual", "soil_md", "yield_biom", "yield_sugar","yield_s_pct_dm")


# LOGIN DETAILS
# user_base <- tibble::tibble(
#   user = c("user1", "user2"),
#   password = sapply(c("pass1", "pass2"), sodium::password_store),
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nordic Beet Yield Challenge"),
    
    # login section
    # shinyauthr::loginUI(id = "login"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("input_user",
                        "Grower:",
                        selected = "NBR",
                        choices = users
                        ),
            selectInput("input_year_trial",
                        "Year of trial:",
                        choices = 2015),
            selectInput("input_year_weather",
                        "Year of weather:",
                        choices = 2015),
            # selectInput("input_model",
            #             "Model:",
            #             c(BeetGRO = 1,
            #               Other1 = 2,
            #               Other2 = 3)
            #             ),
            
            # logout button
            # shinyauthr::logoutUI(id = "logout")

        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Overview",
                     h3("FIELD SUMMARY"),
                     tableOutput("overview_tab"),
                     hr(),
                     h3("MODEL INPUTS SUMMARY"),
                     tableOutput("overview_tab2"),
                     hr(),
                     h3("YIELD SUMMARY"),
                     tableOutput("yield_sum_tab")
                     ),
            tabPanel("Graphs",
                     selectInput("input_variable1","Variable",choices = mod_vars, selected = "Soil Moisture Deficit"), 
                     plotlyOutput("graph1"),
                     selectInput("input_variable2","Variable",choices = mod_vars, selected = "Canopy Cover"), 
                     plotlyOutput("graph2")),
            tabPanel("Table",tableOutput("results_tab"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE,
  #   log_out = reactive(logout_init())
  # )
  # 
  # # Logout to hide
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  
  ##################################
  # USER INTERFACE UPDATING
  
  # UPDATE INPUTS
  observe({
    updateSelectInput(session, "input_year_trial", choices = unique(trial$year[which(trial$user_text==input$input_user)]))
  })
  
  observe({
    updateSelectInput(session, "input_year_weather", choices = unique(trial$year[which(trial$user_text==input$input_user)]), selected = input$input_year_trial)
  })

  ##################################
  # TABLES
  
  # REACTIVE VALUES
  values <- reactiveValues()
  
  # OVERVIEW TABLE
  overview_tab <- reactive({
    user_s <- input$input_user
    year_s <- input$input_year_trial
    
    old_names <- c("user_text","year","field","elevation","latitude","soil_b")
    new_names <- c("Grower", "Year", "Field", "Elevation", "Latitude", "Soil b")
    
    trial_overview <- trial %>% 
      filter(user_text == user_s & year == year_s) %>% 
      select(c("user_text","year","field","elevation","latitude","soil_b")) %>% 
      mutate(year = as.character(year)) %>% 
      rename_at(vars(all_of(old_names)), ~ new_names)
    
    trial_overview
  })
  
  # OVERVIEW TABLE 2
  overview_tab2 <- reactive({
    user_s <- input$input_user
    year_s <- input$input_year_trial
    
    old_names <- c("date_sow", "date_emerg", "date_harvest")
    new_names <- c("Sowing date", "Emergence date", "Harvest date")
    
    trial_overview <- trial %>% 
      filter(user_text == user_s & year == year_s) %>% 
      select(c("date_sow", "date_emerg", "date_harvest")) %>%
      mutate(date_sow = format(as.Date(date_sow, origin = '1970-01-01')),
             date_emerg = format(as.Date(date_emerg, origin = '1970-01-01')),
             date_harvest = format(as.Date(date_harvest, origin = '1970-01-01'))) %>% 
      rename_at(vars(all_of(old_names)), ~ new_names)
    
    trial_overview
  })
  
  # CALCULATE MODEL
  BeetGRO_tab <- reactive({
      user_s <- input$input_user
      year_s <- input$input_year_trial
      year_weather_s <- input$input_year_weather
      trial_i <- trial %>% 
        filter(user_text == user_s & year == year_s)
      weather_source <- unlist(trial_i["weather_source_info"])
      
      soil_b <- ifelse(trial_i$soil_b < 1, 2.1, trial_i$soil_b)
      pop1 <- ifelse(trial_i$pop1 < 90000, -0.0003*(trial_i$pop1/1000)^2+0.0456*(trial_i$pop1/1000)-1.0246, 1)
      pop2 <- ifelse(trial_i$pop2 < 90000, -0.0003*(trial_i$pop2/1000)^2+0.0456*(trial_i$pop2/1000)-1.0246, 1)
      pop3 <- ifelse(trial_i$pop3 < 90000, -0.0003*(trial_i$pop3/1000)^2+0.0456*(trial_i$pop3/1000)-1.0246, 1)
      pop_loss <- ifelse(trial_i$plant_pop == 0, 1, mean(pop1, pop2, pop3))

      ## Soil parameters, function of soil_b
      soil_qfc <- param$a2 * (param$a1/5)^(1/soil_b)
      soil_qpwp <- param$a2 * (param$a1/1500)^(1/soil_b)

      ## Soil parameters, grouped by soil_b
      if(soil_b > 20){
        trial_i <- trial_i %>%
          mutate(
            kappa = 0.0008,
            gamma_i = 0.00002701,
            stress_wA = 5,
            stress_wB = 0.8,
            stress_wC = 200,
            rue_zero1 = 2.1
          )
      } else {
        trial_i <- trial_i %>%
          mutate(
            kappa = 0.0027,
            gamma_i = 0.00014,
            stress_wA = 2,
            stress_wB = 0.6,
            stress_wC = 300,
            rue_zero1 = param$rue_zero
          )
      }

      #####
      # TABLE OF EACH PARAMETER FOR EACH DAY OF YEAR FOR TRIAL "i".

      date_sow <- date(as.POSIXct(unlist(trial_i["date_sow"]), origin = '1970-01-01'))
      date_emerg <- date(as.POSIXct(unlist(trial_i["date_emerg"]), origin = '1970-01-01'))
      date_harvest <- date(as.POSIXct(unlist(trial_i["date_harvest"]), origin = '1970-01-01'))
      doy_sow <- yday(date_sow)
      doy_emerg <- yday(date_emerg)
      doy_harvest <- yday(date_harvest)
      
      values$date_sow <- date_sow
      values$date_emerg <- date_emerg
      values$date_harvest <- date_harvest
      values$doy_sow <- doy_sow
      values$doy_emerg <- doy_emerg
      values$doy_harvest <- doy_harvest

      doy_adjust <- (as.numeric(year_s)-1970)*365+floor((as.numeric(year_s)-1970)*0.25)-1
      
      BG_i <- weath %>%
        filter(year == year_weather_s & weath_id == weather_source) %>% 
        select(doy, temp_x, precip, radiation_solar, evap) %>%

        # Date (The 16435 is to convert from 2015-01-01 to 1970-01-01: THIS WILL NEED TO BE YEAR SPECIFIC)
        mutate(date = as.Date(doy + doy_adjust, origin = "1970-01-01")) %>% 
        
        # Crop status
        mutate(bbch = 00,
               bbch = replace(bbch, doy >= doy_sow, 01),
               bbch = replace(bbch, doy >= doy_emerg, 09),
               bbch = replace(bbch, doy >= doy_harvest, 99)
        ) %>%

        # Temperature
        mutate(
          #temp_x = (Tmax + Tmin)/2, # Called Temp in OpenModel
          temp_b_d = temp_x - param$Tbase,
          temp_b_d = replace(temp_b_d, temp_b_d < 0, 0)
        ) %>%
        group_by(sown = bbch >= 01) %>%
        mutate(
          temp_b_cd_sow = cumsum(temp_b_d),
          temp_b_cd_sow = replace(temp_b_cd_sow, bbch == 00, 0),
          bbch = replace(bbch, doy_emerg <= doy_sow & temp_b_cd_sow >= param$Tzero, 01)
        ) %>%
        group_by(emerged = bbch >= 09) %>%
        mutate(
          temp_b_cd_em = cumsum(temp_b_d) + 90,
          temp_b_cd_em = replace(temp_b_cd_em, bbch < 09, 0)
        ) %>%
        ungroup()

      # GOING ROW WISE(). BUT THE PROBLEM IS THAT YOU CAN'T LAG IN DPLYR::ROWWISE(), SO HAVE TO GO TO LOOP
      for(j in 1:(doy_sow-1)){
        BG_ij <- BG_i[j,] %>%
          mutate(soil_md = 0,
                 soil_md_0 = 0,
                 soil_qrel = 1,
                 canopy_cover = param$fZero,
                 temp_s_cd = 0,
                 evap_soil = 0,
                 evap_actual = 0,
                 yield_biom = 0,
                 yield_sugar = 0,
                 yield_s_pct_dm = 0,
                 root_depth = 0)

        if(j==1) BG_j <- BG_ij
        if(j!=1) BG_j <- bind_rows(BG_j, BG_ij)
      }
      for(j in doy_sow:nrow(BG_i)){
        BG_ij <- BG_i[j,] %>%
          mutate(
            # VALUES WITH LAGS, AND REMOVING NEGATIVES.
            soil_qrel = BG_j$soil_qrel[j-1],
            soil_md = BG_j$soil_md[j-1],
            soil_md_0 = case_when(doy == doy_sow ~ 0, soil_md < 0 ~ 0, T ~ soil_md), # This is soil_md from previous period, with min = 0
            evap_soil = BG_j$evap_soil[j-1],
            yield_biom = BG_j$yield_biom[j-1],
            radiation_solar = replace(radiation_solar, radiation_solar < 0, 0),
            yield_sugar = BG_j$yield_sugar[j-1],

            # CANOPY (canopy_cover)
            stress_water = ifelse(soil_qrel < trial_i$stress_wB & temp_b_cd_em > trial_i$stress_wC, (soil_qrel/trial_i$stress_wB)^trial_i$stress_wC, 1),
            temp_s_d = ifelse(temp_b_d > 0 & temp_b_d < 22 & bbch >= 09, temp_b_d * stress_water, 0), # temp_x Called Weather.DailyTemp in OpenModel, temp_s_d called DailyTemp in OpenModel
            temp_s_cd = unlist(BG_j[j-1,"temp_s_cd"]) + temp_s_d, # temp_s_cd called TotalTemp in OpenModel
            temp_s_cd = ifelse(doy == doy_emerg, 90 + temp_s_d, temp_s_cd),
            temp_f = temp_s_cd * 0.001,
            temp_f = replace(temp_f, temp_f < 0.00001, 0.00001),
            temp_s_cd = replace(temp_s_cd, temp_s_cd > 950, 950),
            canopy_cover = 0.0015 + (0.99-0.0015)/(1+exp(-4*log(temp_f/(1-temp_f)))),

            # ROOT DEPTH (is sowing depth until emergence.)
            root_depth = param$Dsowing + param$length0*exp(param$beta0/param$delta*(1-exp(-param$delta*(temp_b_cd_em-param$Tzero)))),
            root_depth = replace(root_depth, bbch < 09, param$Dsowing),

            # WATER RELATIONS
            # SSE SOIL SURFACE EVAPORATION
            evap_soil_d = min(1.5, evap)*(1-canopy_cover), # this did have if canopy_covere <1, but it always is.
            evap_soil_d = replace(evap_soil_d, evap_soil > 20, 0),
            evap_soil = evap_soil + evap_soil_d - precip,
            evap_soil = replace(evap_soil, evap_soil < 0, 0),
            # ATMOSPHERE LIMITED CROP TRANSPIRATION
            evap_cal = 1.2*canopy_cover*evap,
            evap_cal = replace(evap_cal, evap_cal <= 0, 0.001),
            # PARAMETERS
            soil_q = max(soil_qfc - soil_md_0/(root_depth*1000), 0.01), # depth is always greater than 0 post sowing, so if statement for soil_q removed.
            soil_qrel = soil_q/soil_qfc,
            psi_soil = -5*(soil_qrel)^(-soil_b),
            rsum = param$c1 + param$c2/root_depth*(soil_qrel^(-1*(2*soil_b+3))-1),
            soil_hc = soil_qrel^(2*soil_b+3),
            # SOIL LIMITED MAXIMUM CROP TRANSPIRATION
            evap_csl = (psi_soil - param$psiCrop)/rsum,
            rsum = replace(rsum, soil_qrel == 0, param$c1),
            evap_csl = replace(evap_csl, soil_qrel == 0, 0),
            # ACTUAL CROP EVAPOTRANSPIRATION
            evap_actual = min(evap_cal, evap_csl),
            # SOIL MOISTURE DEFICIT NB: IF THIS GIVE A NEGATIVE RESULT, 0 IS USED WHEN SMD IS USED TO CALC soil_q IN THE NEXT DAY.
            soil_md_d = evap_soil_d + evap_actual - precip,
            soil_md = soil_md_0 + soil_md_d,


            # RADIATION USE EFFICIENCY (rue)
            rue = 0.6*trial_i$rue_zero1*exp(-trial_i$gamma_i*yield_biom)+0.4*trial_i$rue_zero1*(evap_actual/evap_cal)*exp(-trial_i$gamma_i*yield_biom),

            # YIELDS
            yield_biom_d = rue*canopy_cover*radiation_solar, #There is a calculation for radiation in OpenModel, if radiation_solar is negative.
            yield_biom = yield_biom + yield_biom_d,
            yield_sugar = (yield_sugar + yield_biom_d*(trial_i$kappa*yield_biom/(1+trial_i$kappa*yield_biom))),
            yield_s_pct_dm = (trial_i$kappa*yield_biom/(1+trial_i$kappa*yield_biom)),
            yield_pop = pop_loss*yield_sugar
          )

        BG_j <- bind_rows(BG_j, BG_ij)
      }

      BG_i_names <- names(BG_i)
      BG_i_names <- BG_i_names[-1]
      BG_j <- BG_j[,-which(names(BG_j) %in% BG_i_names)]

      BG_i <- BG_i %>%
        left_join(BG_j, by ="doy")

      BG_i$trial_id <- trial_i$id        # Add trial_id back in to use for presentation of data.
      BG_i$id <- trial_i$id*1000+BG_i$doy
      
      # FIXING UNITS
      BG_i <- BG_i %>%
        mutate(yield_biom = yield_biom / 100,
               yield_sugar = yield_sugar / 100,
               yield_s_pct_dm = yield_s_pct_dm * 100)
      
      BG_i
    })
  
  BeetGRO_tab_display <- reactive({
    old_names <- c("date", "temp_x", "precip","canopy_cover", "root_depth", "evap_actual", "soil_md", "yield_biom", "yield_sugar", "yield_s_pct_dm")
    new_names <- c("Date", "Mean Temp", "Precipitation", "Canopy cover", "Root Depth", "Evapotranspiration", "Soil Moisture Deficit", "Yield - Biomass", "Yield - Sugar", "Sugar % DM")
    
    BG_i_display <- BeetGRO_tab() %>% 
      filter(doy > 73) %>% 
      select(date, temp_x, precip, canopy_cover, root_depth, evap_actual, soil_md, yield_biom, yield_sugar, yield_s_pct_dm) %>% 
      mutate(date = format(date)) %>% 
      rename_at(vars(all_of(old_names)), ~ new_names)
  
    BG_i_display
  })
  
  
  BeetGRO_yield_tab <- reactive({
    doy_harvest_s <- values$doy_harvest
    doy_jun1 <- 152
    doy_jul1 <- 182
    doy_aug1 <- 213
    doy_sep1 <- 244
    doy_oct1 <- 274
    doy_nov1 <- 305
    doy_dec1 <- 335
    doys <- c(doy_harvest_s, doy_jun1, doy_jul1, doy_aug1, doy_sep1, doy_oct1, doy_nov1, doy_dec1)
    #doys <- if(doy_harvest %in% doys) doys else c(doy_harvest_s, doys)
    
    old_names <- c("date", "canopy_cover", "yield_biom", "yield_sugar", "yield_s_pct_dm")
    new_names <- c("Date", "Canopy cover", "Yield - Biomass", "Yield - Sugar", "Sugar - % DM")
    
    BeetGRO_yield_tab <- BeetGRO_tab() %>%
      filter(doy %in% doys) %>%
      mutate(date = format(as.Date(doy, origin = "2015-01-01")-1, "%m-%d")) %>% 
      select(date, canopy_cover, yield_biom, yield_sugar, yield_s_pct_dm) %>% 
      rename_at(vars(all_of(old_names)), ~ new_names)
      
  })
  
  # THERE IS AN ERROR ABOUT "IF: ARGUMEENT IS OF LENGTH ZERO" WHEN THIS SUMMARY TABLE IS INCLUDED. NOT SURE WHY.
  
  ##################################
  # GRAPH
  graph1_gg <- reactive({
    y_s <- mod_vars_[which(mod_vars == input$input_variable1)]
    
    ggplot(BeetGRO_tab(), aes(x=date)) + 
      geom_line(aes_string(y = y_s), size = 1) +
      xlab("Date") + 
      geom_vline(xintercept = as.numeric(values$date_sow)) + 
      geom_vline(xintercept = as.numeric(values$date_emerg)) +
      geom_vline(xintercept = as.numeric(values$date_harvest)) +
      theme(axis.title.y=element_blank())
  })
  
  graph2_gg <- reactive({
    y_s <- mod_vars_[which(mod_vars == input$input_variable2)]
    
    ggplot(BeetGRO_tab(), aes(x=date)) + 
      geom_line(aes_string(y = y_s), size = 1) +
      xlab("Date") + 
      geom_vline(xintercept = as.numeric(values$date_sow)) + 
      geom_vline(xintercept = as.numeric(values$date_emerg)) +
      geom_vline(xintercept = as.numeric(values$date_harvest)) +
      theme(axis.title.y=element_blank())
  })
  
  ##################################
  # RENDER OUTPUTS

  output$overview_tab <- renderTable(
    overview_tab()
  )
  
  output$overview_tab2 <- renderTable(
    overview_tab2()
  )
  
  output$results_tab <- renderTable(
    BeetGRO_tab_display()
  )
  
  output$yield_sum_tab <- renderTable(
    BeetGRO_yield_tab()
  )

  output$graph1 <- plotly::renderPlotly({
    graph1_gg()
  })
  
  output$graph2 <- plotly::renderPlotly({
    graph2_gg()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
