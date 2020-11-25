#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
library(shiny)                                                                    # for shiny app functions
library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)                                                              # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages


#### 2 LOAD DATA ###############################################################

data        <- read.csv("data/longterm_data_by_KI.csv", na.strings = c("NA", "")) # load JPMI dataset
jram        <- read.csv("data/JRAM_data.csv")                                     # load JRAM dataset

item_list   <- read.csv("data/item_list.csv")                                     # load item list

country     <- st_read("gis/irq_admbnda_adm0_cso_itos_20190603.shp")              # load shapefile with country border
governorate <- st_read("gis/irq_admbnda_adm1_cso_20190603.shp")                   # load shapefile with governorate borders
district    <- st_read("gis/irq_admbnda_adm2_cso_20190603.shp")                   # load shapefile with district borders

data$date <- as.Date(data$date, format="%Y-%m-%d")                                # format date column in JPMI dataset as date
jram$Date <- as.Date(jram$Date, format="%d/%m/%Y")                                # format date column in JRAM dataset as date


#### 3 AGGREGATION #############################################################

#### * 3.1 Prices ##############################################################

prices <- data %>%
    select(date, governorate, district, site, ends_with("_price"), exchange_rate) %>%   # calculate site medians
    group_by(date, governorate, district, site) %>% 
    summarise_all(median, na.rm = TRUE) %>%
    select(date, governorate, district, ends_with("_price"), exchange_rate) %>%         # calculate district medians
    group_by(date, governorate, district) %>% 
    summarise_all(median, na.rm = TRUE) %>%
    setNames(gsub("_price","",names(.))) %>%                                            # rename columns names
    ungroup() %>%
    group_by(date) %>%            
    mutate(water_trucking2 = ifelse(is.na(water_trucking),                               # impute missing water trucking values
                                   water/(1- median( water_trucking/(water+water_trucking), na.rm=TRUE )),
                                   water_trucking),
           SMEB_food  = 10*lentils + 15*rice + 5*sugar + 0.75*salt + 5*bulgur +         # calculate SMEB values
                        4.55*vegetable_oil + 30*wheat_flour,
           SMEB_nfi   = 6*bath_soap + 6*toothbrush + 1*toothpaste + 1*shampoo +
                        32*sanitary_napkins + 1*plastic_garbage_bags + 1*detergent,
           SMEB_water = 3*water + 47/500*water_trucking2,
           SMEB       = SMEB_food + SMEB_nfi + SMEB_water,
           SMEB_food_old  = 10.8*lentils + 40.5*rice + 5.94*sugar +         # calculate SMEB values
               5.94*vegetable_oil + 40.5*wheat_flour,
           SMEB_nfi_old   = 8*bath_soap + 4*toothbrush + 2*toothpaste + 2*shampoo + 1*disinfectant_solution +
               20*sanitary_napkins + 1*plastic_garbage_bags,
           SMEB_fuel_old = 1*butane + 16.67*kerosene,
           SMEB_old       = SMEB_food_old + SMEB_nfi_old + ifelse(date < "2018-08-01", SMEB_fuel_old, SMEB_water),
           exchange_rate = exchange_rate / 100                                          # divide exchange rate by 100 since we ask for the price of 100 USD
    ) %>%
    select(-water_trucking2) %>%
    mutate(SMEB          = ifelse(date >= "2020-01-01", SMEB, NA),
           SMEB_old      = ifelse(date <  "2020-01-01", SMEB_old, NA),
           SMEB_food     = ifelse(date >= "2020-01-01", SMEB_food, NA),
           SMEB_food_old = ifelse(date <  "2020-01-01", SMEB_food_old, NA),
           SMEB_nfi      = ifelse(date >= "2020-01-01", SMEB_nfi, NA),
           SMEB_nfi_old  = ifelse(date <  "2020-01-01", SMEB_nfi_old, NA),
           #SMEB_water    = ifelse(date >= "2020-01-01", SMEB_water, NA),
           SMEB_fuel_old = ifelse(date <  "2020-01-01", SMEB_fuel_old, NA)) %>%
    mutate_if(is.numeric, round, 0) %>%                                                 # round values
    rename(Date = date, Governorate = governorate, District = district,                 # rename column names 
           "SMEB" = SMEB,
           "SMEB food" = SMEB_food,
           "SMEB NFI" = SMEB_nfi,
           "SMEB water" = SMEB_water,
           "SMEB (pre-2020)" = SMEB_old,
           "SMEB food (pre-2020)" = SMEB_food_old,
           "SMEB NFI (pre-2020)" = SMEB_nfi_old,
           "SMEB fuel (pre-2020)" = SMEB_fuel_old,
           "Lentils (1 kg)"      = lentils,
           "Rice (1 kg)"         = rice,
           "Sugar (1 kg)"        = sugar,
           "Salt (1 kg)"         = salt,
           "Bulgur (1 kg)"       = bulgur,
           "Vegetable oil (1 L)" = vegetable_oil,
           "Wheat flour (1 kg)"  = wheat_flour,
           "Bath soap (125 g)"    = bath_soap,
           "Toothbrush (1 unit)" = toothbrush,
           "Toothpaste (75 ml)"  = toothpaste,
           "Shampoo (500 ml)"    = shampoo,
           "Sanitary napkins (1 unit)" = sanitary_napkins,
           "Garbage bags (20 units)" = plastic_garbage_bags,
           "Detergent (1 kg)"       = detergent,
           "Disinfectant solution (1 L)" = disinfectant_solution,
           "Bottled water (1 L)"    = water,
           "Water trucking (500 L)" = water_trucking,
           "Butane (1 canister)" = butane,
           "Chicken (1 kg)" = chicken,
           "Chickpeas (1 kg)" = chickpeas,
           "Comb (1 unit)" = comb,
           "Dry milk (500 g)" = dry_milk,
           "Eggs (30 units)" = eggs,
           "Kerosene (1 L)" = kerosene,
           "Nail clippers (1 unit)" = nail_clippers,
           "Onions (1 kg)" = onions,
           "Tea (500 g)" = tea,
           "Towel (1 unit)" = towel,
           "White beans (1 kg)" = white_beans,
           "US dollars (1 USD)" = exchange_rate
    )

prices_long <- gather(prices, Item, Price, 4:ncol(prices))                        # change dataframe to long format

#### * 3.2 Indicators ##########################################################

indicators <- data %>%
    select(date, governorate, district, starts_with("challenges."), ends_with("_shortage"), ends_with("_imported"), supply_routes,
           -starts_with("butane_"), -starts_with("chicken_"), -starts_with("chickpeas_"), -starts_with("comb_"),
           -starts_with("dry_milk_"), -starts_with("eggs_"), -starts_with("kerosene_"), -starts_with("nail_clippers_"),
           -starts_with("onions_"), -starts_with("tea_"), -starts_with("towel_"), -starts_with("white_beans_"))

indicators <- indicators %>%
    gather(Indicator, Value, 4:(ncol(indicators))) %>%
    filter(Indicator != "challenges.decline") %>%
    group_by(date, governorate, district, Indicator) %>%
    summarise(freq = sum(Value == 1 | Value == "yes", na.rm = TRUE) / sum(!is.na(Value)) * 100) %>%
    mutate_if(is.numeric, round, 0) %>% 
    spread(Indicator, freq) %>%
    mutate(challenges.atleast1 = 100 - challenges.none) %>%
    rename(Date = date,
           Governorate = governorate,
           District = district,
           '% of traders reporting challenge in past 30 days: Issues with check points or other movement restrictions' = challenges.check_points,
           '% of traders reporting challenge in past 30 days: Shortages in demand' = challenges.demand,
           '% of traders reporting challenge in past 30 days: I do not know' = challenges.dnk,
           '% of traders reporting challenge in past 30 days: Issues with government regulations' = challenges.government_regulations,
           '% of traders reporting challenge in past 30 days: Issues with insecurity or instability in the area' = challenges.insecurity,
           '% of traders reporting challenge in past 30 days: Shortages in liquidity' = challenges.liquidity,
           '% of traders reporting challenge in past 30 days: None' = challenges.none,
           '% of traders reporting challenge in past 30 days: Other' = challenges.other,
           '% of traders reporting challenge in past 30 days: At least one challenge reported' = challenges.atleast1,
           
           '% of traders reporting harmful supply route change in past 30 days' = supply_routes,
           
           '% of traders reporting shortage of LENTILS in past 30 days'        = lentils_shortage,
           '% of traders reporting shortage of RICE in past 30 days'           = rice_shortage,
           '% of traders reporting shortage of SUGAR in past 30 days'          = sugar_shortage,
           '% of traders reporting shortage of SALT in past 30 days'           = salt_shortage,
           '% of traders reporting shortage of BULGUR in past 30 days'         = bulgur_shortage,
           '% of traders reporting shortage of VEGETABLE OIL in past 30 days'  = vegetable_oil_shortage,
           '% of traders reporting shortage of WHEAT FLOUR in past 30 days'    = wheat_flour_shortage,
           '% of traders reporting shortage of BATH SOAP in past 30 days'      = bath_soap_shortage,
           '% of traders reporting shortage of TOOTHBRUSH in past 30 days'     = toothbrush_shortage,
           '% of traders reporting shortage of TOOTHPASTE in past 30 days'     = toothpaste_shortage,
           '% of traders reporting shortage of SHAMPOO in past 30 days'        = shampoo_shortage,
           '% of traders reporting shortage of SANITARY NAPKINS in past 30 days' = sanitary_napkins_shortage,
           '% of traders reporting shortage of GARBAGE BAGS in past 30 days'   = plastic_garbage_bags_shortage,
           '% of traders reporting shortage of DETERGENT in past 30 days'      = detergent_shortage,
           '% of traders reporting shortage of DISINFECTANT SOLUTION in past 30 days' = disinfectant_solution_shortage,
           '% of traders reporting shortage of BOTTLED WATER in past 30 days'  = water_shortage,
           '% of traders reporting shortage of WATER TRUCKING in past 30 days' = water_trucking_shortage,
           
           '% of traders importing LENTILS'        = lentils_imported,
           '% of traders importing RICE'           = rice_imported,
           '% of traders importing SUGAR'          = sugar_imported,
           '% of traders importing SALT'           = salt_imported,
           '% of traders importing BULGUR'         = bulgur_imported,
           '% of traders importing VEGETABLE OIL'  = vegetable_oil_imported,
           '% of traders importing WHEAT FLOUR'    = wheat_flour_imported,
           '% of traders importing BATH SOAP'      = bath_soap_imported,
           '% of traders importing TOOTHBRUSH'     = toothbrush_imported,
           '% of traders importing TOOTHPASTE'     = toothpaste_imported,
           '% of traders importing SHAMPOO'        = shampoo_imported,
           '% of traders importing SANITARY NAPKINS' = sanitary_napkins_imported,
           '% of traders importing GARBAGE BAGS'   = plastic_garbage_bags_imported,
           '% of traders importing DETERGENT'      = detergent_imported,
           '% of traders importing DISINFECTANT SOLUTION' = disinfectant_solution_imported,
           '% of traders importing BOTTLED WATER'  = water_imported
    )

full <- left_join(prices, indicators, by = c("Date", "Governorate", "District"))


#### 4 REFERENCES ##############################################################

dates <- sort(unique(prices_long$Date))                                           # define list with date range in data
dates_min  <- as.Date("2020-01-01")                                               # set minimum date to be displayed
dates_max  <- max(prices_long$Date)                                               # maximum date in data
dates_max2 <- sort(unique(prices_long$Date), decreasing=T)[2]                     # second-latest date

dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
dates_max_1y$year <- dates_max_1y$year-1
dates_max_1y <- as.Date(dates_max_1y)

dates_jram <- sort(unique(jram$Date))                                             # date range in JRAM data
dates_min_jram <- min(dates_jram)                                                 # minimum date in JRAM data
dates_max_jram <- max(dates_jram)                                                 # maximum date in JRAM data

plot_location_list <- prices_long %>%                                             # define location list (which is later used as choice filter options)
    ungroup() %>%
    select(Governorate, District) %>%                                             # extract governorate and district columns
    arrange(Governorate, District) %>%                                            # list alphabetically
    filter(!duplicated(District))                                                 # remove duplicates

indicator_list <- names(indicators) %>%
    str_subset(c("Date", "Governorate", "District"), negate = TRUE)               # extract additional indicator list

cols      <- c("rgb(238,88,89)", "rgb(88,88,90)", "rgb(165,201,161)",             # define color palette for plot lines
               "rgb(86,179,205)", "rgb(246,158,97)", "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

full_list <- data.frame(Item = indicator_list,                                    # create full indicator list
                        Group = "VIII. Additional Indicators") %>% arrange(Item)
full_list <- rbind(item_list, full_list)                                          # combine prices and additional indicators

jram$partners <- paste(jram$Lead_Partner, jram$Other_partners_involved, sep = ", ")
jram$partners <- gsub(", $", "", jram$partners)

partner_list <- strsplit(jram$partners, ", ")
partner_list <- sort(unique(unlist(partner_list)))


#### 5 PREPARE DATA ############################################################

#### * 5.1 Define dataframe ####################################################

prices_country <- prices %>%                                                      # aggregate price data at country level
    select(-Governorate, -District) %>%
    group_by(Date) %>% 
    summarise_all(median, na.rm = TRUE)
prices_country_long <- gather(prices_country, Item, Price, 2:ncol(prices_country))# transform country-level price data to long format

prices_country_home <- prices_country %>%                                         # filter out SMEB data from country level price data
    filter(Date >= dates_min) %>%
    select(Date, SMEB, `SMEB food`, `SMEB NFI`, `SMEB water`) %>%
    gather(Item, Price, SMEB:'SMEB water')                                        # transform SMEB data to long format so highcharter can read dataframe

prices_changes <- prices_country_long %>%                                         # calculate bi-monthly/yearly changes of item prices
    filter(Date == dates_max | Date == dates_max2 | Date == dates_max_1y) %>%
    group_by(Item) %>%
    mutate(change = percent(Price/lag(Price, order_by=Date)-1, accuracy = 1),
           change2 = percent(Price/lag(Price, n = 2, order_by=Date)-1, accuracy = 1)) %>%
    mutate(change = ifelse(!grepl('^\\-', change) & change != "0%" & !is.na(change), paste0("+", change, HTML(" &#9650;")), change),
           change = ifelse(grepl('^\\-', change), paste0(change, HTML(" &#9660;")), change),
           change = ifelse(change == "0%", paste0(change, HTML(" &#9654;")), change),
           change2 = ifelse(!grepl('^\\-', change2) & change2 != "0%" & !is.na(change2), paste0("+", change2, HTML(" &#9650;")), change2),
           change2 = ifelse(grepl('^\\-', change2), paste0(change2, HTML(" &#9660;")), change2),
           change2 = ifelse(change2 == "0%", paste0(change2, HTML(" &#9654;")), change2)) %>%
    filter(Date == dates_max,
           !is.na(Price)) %>%
    select(-Date) %>%
    mutate(Price  = format(Price, big.mark=","),
           change2 = replace_na(change2, "NA")) %>%
    rename("Price (in IQD)" = Price,
           "Bi-monthly change" = change,
           "Yearly change" = change2)

prices_changes_items <- prices_changes %>%
    filter(!str_detect(Item, "^SMEB"))

prices_changes_meb <- prices_changes %>%
    filter(str_detect(Item, "^SMEB")) %>%
    arrange(Item)

data_latest <- data %>%                                                                                   # latest dataset for download on dashboard page
    filter(date == dates_max) %>%
    select(-(106:ncol(data)), -ends_with("_restock"))

jram <- jram %>%
    filter(GPS_Coordinates != "") %>%                                                                     # delete entries without coordinates from JRAM dataset
    separate(GPS_Coordinates, c("lat","lon"), ",", convert = TRUE) %>%                                    # divide coordinates into latitude and longitude
    mutate(Link_to_Assessment = ifelse(Link_to_Assessment == "", "", paste0("<a href=", Link_to_Assessment, ">link</a>"))) %>%
    mutate(popup_data = paste0('<strong>Lead partner: </strong>', Lead_Partner,                           # define pop up window contents
                               '<br><strong>Governorate:</strong> ', Governorate,
                               '<br><strong>Location:</strong> ', Location,
                               '<br><strong>Market:</strong> ', Market,
                               '<br><strong>Date:</strong> ', format(Date, "%B %Y"),
                               '<br><strong>Other partners involved:</strong> ', Other_partners_involved,
                               '<br><strong>Info:</strong> ', Info,
                               '<br><strong>Contact name:</strong> ', Contact_Name,
                               '<br><strong>Contact email:</strong> <a href=mailto:', Contact_Email, '>', Contact_Email, '</a>',
                               '<br><strong>Link to assessment:</strong> ', Link_to_Assessment),
           label = paste0(Location, " - ", Lead_Partner, " (", format(Date, "%b %Y"),")")                 # define hover label contents
           )


#### * 5.2 Dashboard tables ######################################################

smeb <- data.frame(Category = c(rep("Food Items", 7), rep("Non-Food Items", 8), rep("Water", 2)),         # define SMEB content table
                   Item = c("Bulgur", "Lentils", "Rice", "Salt", "Sugar", "Vegetable oil",
                            "Wheat flour", "Bath soap", "Adult toothbrush", "Child toothbrush",
                            "Detergent", "Garbage bags", "Sanitary napkins", "Shampoo",
                            "Toothpaste", "Water trucking", "Drinking water"),
                   Quantity = c("5 kg", "10 kg", "15 kg", "0.75 kg", "6 kg", "4.55 L", "30 kg",
                                "6 x 125 g", "3 units", "3 units", "1 kg", "1 pack (20)",
                                "4 packs (32)", "1 x 500 ml", "1 x 75 ml", "47 L/person",
                                "3 L/person"))

smeb_kbl <- smeb %>%                                                                                      # make a html (kable) object out of dataframe
    kbl(escape = F) %>%
    kable_styling(bootstrap_options = c("hover", "condensed", "striped"), fixed_thead = T, full_width = F) %>%
    column_spec(1, width = "8em", bold = T, background = "white") %>%
    column_spec(2, width = "10em") %>%
    column_spec(3, width = "8em") %>%
    collapse_rows(columns = 1, valign = "top")


table_changes_meb <- prices_changes_meb %>%                                                               # style key figures table
        kbl(escape = F, format.args = list(big.mark = ","), align = "lrrr") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = F) %>%
        column_spec(1, width = "7em") %>%
        column_spec(3, color = ifelse(grepl('^\\+', prices_changes_meb$'Bi-monthly change'), "red", ifelse(grepl('^\\-', prices_changes_meb$'Bi-monthly change'), "green", "auto"))) %>%
        column_spec(4, color = ifelse(grepl('^\\+', prices_changes_meb$'Yearly change'), "red", ifelse(grepl('^\\-', prices_changes_meb$'Yearly change'), "green", "auto")))
    

month_collected      <- paste0(format(dates_max, "%B"), " ",format(dates_max, "%Y"))                      # define overview of last round
shops_covered        <- nrow(data_latest)
districts_covered    <- n_distinct(data_latest$district, na.rm = FALSE)
governorates_covered <- n_distinct(data_latest$governorate, na.rm = FALSE)
overview_round       <- data.frame(figure = c("Month", "Shops covered", "Districts covered", "Governorates covered"),
                                   value  = c(month_collected, shops_covered, districts_covered, governorates_covered)
                                   )

table_round <- overview_round %>%                                                                         # style overview table
    kbl(escape = F, format.args = list(big.mark = ","), align = "lr", col.names = NULL) %>%
    column_spec(1, width = "12em") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = T) %>%
    row_spec(1, extra_css = "border-top: 2px solid gainsboro")


table_changes <- prices_changes_items %>%                                                                 # style item table
    kbl(escape = F, format.args = list(big.mark = ","), align = "lrrr") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = F) %>%
    column_spec(1, width = "13em") %>%
    column_spec(3, color = ifelse(grepl('^\\+', prices_changes_items$'Bi-monthly change'), "red", ifelse(grepl('^\\-', prices_changes_items$'Bi-monthly change'), "green", "auto"))) %>%
    column_spec(4, color = ifelse(grepl('^\\+', prices_changes_items$'Yearly change'), "red", ifelse(grepl('^\\-', prices_changes_items$'Yearly change'), "green", "auto"))) %>%
    row_spec(c(7, 9, 17), extra_css = "border-bottom: 2px solid gainsboro")


#### 6 UI ######################################################################

ui <- bootstrapPage(

    navbarPage("Joint Price Monitoring Initiative (JPMI)",                        # define dashboard title
               theme = shinytheme("simplex"),                                     # set theme
               
               #### * 6.1 Home ######################################################################
               
               tabPanel("Dashboard",                                                                          # define panel title
                        icon = icon("tachometer-alt"),                                                        # select icon to be displayed in front of title
                        
                        div(class="dashboard",                                                                # set dashboard class from CSS file
                            
                            tags$head(includeCSS("styles.css")),                                              # load CSS stylesheet
                            
                            leafletOutput("map_home", width = "100%", height = "100%"),                       # display background map
                            
                            absolutePanel(                                                                    # define introduction box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "20", left = "20", right = "auto", bottom = "auto", width = "400", height = "auto",
                                h4("Introduction"),
                                p("The Joint Price Monitoring Initiative (JPMI) is a bi-monthly data collection exercise launched by the Iraq Cash Working Group (CWG)
                                  in November 2016. The initiative aims to inform cash-based interventions in Iraq by providing indicative information on key commodities
                                  sold in local marketplaces. The initiative is guided by the CWG, led by REACH and supported by the CWG members.",
                                  style="text-align:justify"),
                                p("This website displays a wide range of indicators collected through the JPMI, such as prices for key food
                                  and non-food items (NFIs), as well as the costs associated with the Survival Minimum Expenditure Basket (SMEB).",
                                  style="text-align:justify"),
                                p(tags$i(h6("Refer to the tools displayed in the panel above if you wish to analyse disaggregated data.
                                            Display price data over time with the Price Plot, do spatial analysis with the Map, or
                                            discover the data with the Data Explorer. See Info for more on the JPMI.",
                                            style="color:grey;text-align:justify"))),
                                br()
                            ),
                            
                            absolutePanel(                                                                    # define chart box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "393", left = "20", right = "auto", bottom = "auto", width = "400", height = "auto",
                                hchart(prices_country_home, "line",                                           # define chart
                                       hcaes(x = Date, y = Price, group = Item)) %>%
                                    hc_yAxis(min = 0, title = list(text = "")) %>%
                                    hc_xAxis(title = "", labels = list(align = "center")) %>%
                                    hc_size(height = "253") %>%
                                    hc_title(
                                        text = "Overall Median SMEB Over Time (in IQD)",
                                        margin = 10,
                                        align = "left",
                                        style = list(fontSize = 15)
                                    ) %>%
                                    hc_colors(cols) %>%
                                    hc_legend(style = list(fontSize = 8))
                            ),
                            
                            absolutePanel(
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "20", left = "440", right = "auto", bottom = "auto", width = "350", height = "240",
                                h4(paste0("Key Figures", " (", format(dates_max, "%B"), " ", format(dates_max, "%Y"), ")")),
                                HTML(table_changes_meb), br()
                            ),
                            
                            absolutePanel(
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "280", left = "440", right = "auto", bottom = "auto", width = "350", height = "195",
                                h4("Latest Round"),
                                HTML(table_round), br()
                            ),
                            
                            absolutePanel(
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "495", left = "440", right = "auto", bottom = "auto", width = "350", height = "173",
                                h4("Data Download"),
                                "Visit the Data Explorer or download the full dataset from the latest round here:",
                                br(), br(),
                                downloadButton("downloadDataLatest",
                                               paste0("Download ", format(dates_max, "%B"), " ", format(dates_max, "%Y"), " dataset")),
                                br(), br()
                            ),
                            
                            absolutePanel(
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "20", left = "810", right = "auto", bottom = "auto",
                                width = "427",
                                h4(paste0("Overall Median Item Prices", " (", format(dates_max, "%B"), " ", format(dates_max, "%Y"), ")")),
                                HTML(table_changes), br()
                            ),
                            
                            absolutePanel(id = "dropdown", top = 47, left = 750, width = 200, fixed=FALSE, draggable = FALSE, height = "auto",
                                          dropdown(
                                              h4("SMEB contents"),
                                              column(
                                                  HTML(smeb_kbl),
                                                  width = 7),
                                              column(p(h6("The Survival Minimum Expenditure Basket (SMEB) represents the minimum culturally adjusted group of items
                                                             required to support a six-person Iraqi household for one month, as defined by the CWG.")),
                                                     p(h6("The SMEB reported on this website only includes the food, NFI and water components. Not included are rent,
                                                             electricity, communication and transportation.")),
                                                     p(h6("The composition of the SMEB was revised twice: 1) In the September
                                                            2018 round and onwards, the current water component replaced the fuel component. 2) Since January 2020,
                                                            the SMEB furthermore includes modified food and NFI components.")),
                                                     p(h6("More details on the SMEB can be found here:",
                                                          tags$a(href="https://www.humanitarianresponse.info/en/operations/iraq/document/survival-minimum-expenditure-basket-technical-guidance-note-october-2019",
                                                                 "SMEB Guidance Note"), ".")),
                                                     width = 5),
                                              width = "650px",
                                              tooltip = tooltipOptions(title = "Click for more details on the SMEB."),
                                              size = "xs",
                                              up = FALSE,
                                              style = "jelly", icon = icon("info"),
                                              animate = animateOptions(
                                                  enter = "fadeInDown",
                                                  exit  = "fadeOutUp",
                                                  duration = 0.5)
                                          )
                            ),
                                                                                                              # display CWG & REACH logos on bottom left
                            absolutePanel(id = "logo", class = "card", bottom = 15, left = 20, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.humanitarianresponse.info/en/operations/iraq/cash-working-group', target = "_blank",
                                                                  tags$img(src='cwg.jpg', height='30'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 15, left = 150, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.reach-initiative.org', target = "_blank", tags$img(src='reach.jpg', height='30'))),
                                                                                                              # display partner logos on bottom right
                            absolutePanel(id = "logo", class = "card", bottom = 14, right = 20, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.clovekvtisni.cz/en/', target = "_blank", tags$img(src='pin.png', height='35'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 14, right = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.oxfam.org/en', target = "_blank", tags$img(src='oxfam.png', height='35'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 11, right = 130, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.nrc.no/', target = "_blank", tags$img(src='nrc.png', height='40'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 15, right = 270, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.mercycorps.org/', target = "_blank", tags$img(src='mercycorps.png', height='35'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 15, right = 380, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.rescue.org/', target = "_blank", tags$img(src='irc.png', height='34'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 14, right = 430, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://harikar.org/', target = "_blank", tags$img(src='harikar.png', height='35'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 19, right = 480, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://drc.ngo/', target = "_blank", tags$img(src='drc.png', height='25'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 14, right = 560, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.care-international.org/', target = "_blank", tags$img(src='care.png', height='37'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 18, right = 610, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.acted.org/en/', target = "_blank", tags$img(src='acted.png', height='25')))
                        )                                                                                     # close dashboard class 
               ),                                                                                             # close dashboard tabpanel
               
               
               #### * 6.2 Plot ######################################################################
               
               tabPanel("Price Plot",                                                                         # set panel title
                        icon = icon("chart-line"),                                                            # select icon
                        chooseSliderSkin(skin = "Nice", color = NULL),                                        # set theme for sliders
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                tags$i(h6("Note: Reported prices are indicative only.", style="color:#045a8d")),
                                
                                pickerInput("plot_aggregation",
                                            label = "Aggregation level:",
                                            choices = c("District", "Governorate", "Country"),
                                            selected = "Country",
                                            multiple = FALSE
                                ),

                                conditionalPanel(condition = "input.plot_aggregation == 'Country'",
                                                 radioGroupButtons("plot_type",
                                                                   label = "Plot type:",
                                                                   choices = c("Line Graph", "Boxplot"),
                                                                   selected = "Line Graph",
                                                                   justified = TRUE
                                                 )
                                ),
                                                                
                                hr(),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'District'",
                                                 radioGroupButtons("plot_by_district_item",
                                                                   label = "Group by:",
                                                                   choices = c("Item", "District"),
                                                                   selected = "Item",
                                                                   justified = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Governorate'",
                                                 radioGroupButtons("plot_by_governorate_item",
                                                                   label = "Group by:",
                                                                   choices = c("Item", "Governorate"),
                                                                   selected = "Item",
                                                                   justified = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'District'",
                                                 pickerInput("select_bydistrict_district",
                                                             label = "District(s):",
                                                             choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = c("Al-Falluja"),
                                                             multiple = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'District'",
                                                 pickerInput("select_bydistrict_item",
                                                             label = "Item:",   
                                                             choices = lapply(split(item_list$Item, item_list$Group), as.list),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = "SMEB",
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Governorate'",
                                                 pickerInput("select_bygovernorate_governorate",
                                                             label = "Governorate(s):",
                                                             choices = unique(plot_location_list$Governorate),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = c("Al-Anbar"),
                                                             multiple = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Governorate'",
                                                 pickerInput("select_bygovernorate_item",
                                                             label = "Item:",   
                                                             choices = lapply(split(item_list$Item, item_list$Group), as.list),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = "SMEB",
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Country' | (input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Item') | (input.plot_aggregation == 'District' & input.plot_by_district_item == 'Item')",
                                                 pickerInput("select_byitem_item",
                                                             label = "Item(s):",   
                                                             choices = lapply(split(item_list$Item, item_list$Group), as.list),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = c("SMEB", "SMEB food", "SMEB NFI", "SMEB water"),
                                                             multiple = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'Item'",
                                                 pickerInput("select_byitem_district",
                                                             label = "District:",
                                                             choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = "Al-Falluja",
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Item'",
                                                 pickerInput("select_byitem_governorate",
                                                             label = "Governorate:",
                                                             choices = unique(plot_location_list$Governorate),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = "Al-Anbar",
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                hr(),
                                
                                conditionalPanel(condition = "input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph')",
                                                 sliderTextInput("select_date",                                                # set date slider
                                                                 "Months:",
                                                                 force_edges = TRUE,
                                                                 choices = dates,
                                                                 selected = c(dates_min, dates_max)
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Country' & input.plot_type == 'Boxplot'",
                                                 sliderTextInput("select_date_boxplot",                                        # set date slider
                                                                 "Month:",
                                                                 force_edges = TRUE,
                                                                 choices = dates,
                                                                 selected = dates_max
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph')",
                                                 prettySwitch(
                                                     inputId = "select_index",
                                                     label = "Index series", 
                                                     status = "success",
                                                     fill = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.select_index == true & (input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph'))",
                                                 sliderTextInput("select_date_index",
                                                                 "Reference Month:",
                                                                 force_edges = TRUE,
                                                                 choices = dates,
                                                                 selected = dates_max
                                                 ),
                                ),
                                
                                h6("Select aggregation level, item(s), location(s) and month from drop-down menues to update plot.
                                   Displayed values are median prices - retail prices are first aggregated on site level and then
                                   on district level (and then on governorate/country level)."),
                                
                                absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                              fixed=TRUE, draggable = FALSE, height = "auto",
                                              dropdown(
                                                  h4("SMEB contents"),
                                                  column(
                                                      HTML(smeb_kbl),
                                                      width = 7),
                                                  column(p(h6("The Survival Minimum Expenditure Basket (SMEB) represents the minimum culturally adjusted group of items
                                                             required to support a six-person Iraqi household for one month, as defined by the CWG.")),
                                                         p(h6("The SMEB reported on this website only includes the food, NFI and water components. Not included are rent,
                                                             electricity, communication and transportation.")),
                                                         p(h6("The composition of the SMEB was revised twice: 1) In the September
                                                            2018 round and onwards, the current water component replaced the fuel component. 2) Since January 2020,
                                                            the SMEB furthermore includes modified food and NFI components.")),
                                                         p(h6("More details on the SMEB can be found here:",
                                                              tags$a(href="https://www.humanitarianresponse.info/en/operations/iraq/document/survival-minimum-expenditure-basket-technical-guidance-note-october-2019",
                                                                     "SMEB Guidance Note"), ".")),
                                                         width = 5),
                                                  width = "650px",
                                                  tooltip = tooltipOptions(title = "Click for more details on the SMEB."),
                                                  size = "xs",
                                                  up = TRUE,
                                                  style = "jelly", icon = icon("info"),
                                                  animate = animateOptions(
                                                      enter = "fadeInLeftBig",
                                                      exit  = "fadeOutLeft",
                                                      duration = 0.5)
                                              )
                                ),
                                
                                width = 3,                                                                    # set bootstrap width of sidebar (out of 12)
                            ),                                                                                # close sidebar panel
                            
                            mainPanel(
                                conditionalPanel(condition = "input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph')",
                                                 br(),
                                                 tags$i(textOutput("plot_text"), style = "color: red"),                        # display error message displayed if there is no data available
                                                 highchartOutput("graph", width = "100%", height = "600px"),                   # display large chart
                                                 width = 8                                                                     # set width of main panel (out of 12, as per bootstrap logic)
                                ),
                                
                                conditionalPanel(condition = "input.plot_aggregation == 'Country' & input.plot_type == 'Boxplot'",
                                                 br(),
                                                 highchartOutput("boxplot", width = "100%", height = "600px"),
                                                 tags$i(h6("The boxplots are built with district medians and illustrate the variation of prices across the country.", style="color:#045a8d; text-align:center"))
                                )
                            )
                        )
               ),
               
               
               #### * 6.3 Map ######################################################################
               
               tabPanel("Map", icon = icon("map"),
                        
                        div(class="outer",
                            
                            tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                            ),
                            
                            leafletOutput("map", width = "100%", height = "100%"),
                            
                            absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
                                width = 330, height = "auto",
                                
                                pickerInput("map_indicator_select",
                                            label = "Indicator:",   
                                            choices = c("Prices", "Availability and Challenges"),
                                            selected = "Prices",
                                            multiple = FALSE
                                ),
                                
                                conditionalPanel(condition = "input.map_indicator_select == 'Prices'",
                                                 pickerInput("map_item_select",
                                                             label = "Item:",   
                                                             choices = lapply(split(item_list$Item, item_list$Group), as.list),
                                                             selected = "SMEB",
                                                             options = list(`actions-box` = TRUE),
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.map_indicator_select == 'Availability and Challenges'",
                                                 pickerInput("map_addindicator_select",
                                                             label = HTML("Availability and Challenges:<br><i>(% of assessed traders)</i>"),   
                                                             choices = sort(indicator_list),
                                                             selected = "% of traders reporting harmful supply route change in past 30 days",
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             multiple = FALSE
                                                 )
                                ),
                                
                                sliderTextInput("map_date_select",
                                                "Month:",
                                                force_edges = TRUE,
                                                choices = dates,
                                                selected = dates_max,
                                                animate = TRUE
                                )                                                                              # close sliderTextInput
                                
                            ),                                                                                 # close absolutePanel
                            
                            absolutePanel(id = "no_data", fixed = TRUE, draggable = FALSE, top = 50, left = 0, right = 0, bottom = 0,
                                          width = "550", height = "20",
                                          tags$i(h4(textOutput("map_text"), style = "color: red; background-color: white;"))
                            ),
                            
                            conditionalPanel(condition = "input.map_indicator_select == 'Prices' & (input.map_item_select == 'SMEB' | input.map_item_select == 'SMEB food' | input.map_item_select == 'SMEB NFI' | input.map_item_select == 'SMEB water' |
                                             input.map_item_select == 'SMEB (pre-2020)' | input.map_item_select == 'SMEB food (pre-2020)' | input.map_item_select == 'SMEB NFI (pre-2020)' | input.map_item_select == 'SMEB water (pre-2020)')",
                                             absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                                           fixed=TRUE, draggable = FALSE, height = "auto",
                                                           dropdown(
                                                             h4("SMEB contents"),
                                                             column(
                                                               HTML(smeb_kbl),
                                                               width = 7),
                                                             column(p(h6("The Survival Minimum Expenditure Basket (SMEB) represents the minimum culturally adjusted group of items
                                                             required to support a six-person Iraqi household for one month, as defined by the CWG.")),
                                                                    p(h6("The SMEB reported on this website only includes the food, NFI and water components. Not included are rent,
                                                             electricity, communication and transportation.")),
                                                                    p(h6("The composition of the SMEB was revised twice: 1) In the September
                                                            2018 round and onwards, the current water component replaced the fuel component. 2) Since January 2020,
                                                            the SMEB furthermore includes modified food and NFI components.")),
                                                                    p(h6("More details on the SMEB can be found here:",
                                                                         tags$a(href="https://www.humanitarianresponse.info/en/operations/iraq/document/survival-minimum-expenditure-basket-technical-guidance-note-october-2019",
                                                                                "SMEB Guidance Note"), ".")),
                                                                    width = 5),
                                                             width = "650px",
                                                             tooltip = tooltipOptions(title = "Click for more details on the SMEB."),
                                                             size = "xs",
                                                             up = TRUE,
                                                             style = "jelly", icon = icon("info"),
                                                             animate = animateOptions(
                                                               enter = "fadeInLeftBig",
                                                               exit  = "fadeOutLeft",
                                                               duration = 0.5)
                                                           )                                                  # close dropdown
                                             )                                                                # close absolute panel
                            )                                                                                 # close conditional panel
                        )                                                                                     # close div for class outer
               ),                                                                                             # close tab panel

               
               #### * 6.4 Data Explorer ######################################################################

               tabPanel("Data Explorer", icon = icon("table"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                radioGroupButtons("table_aggregation",
                                                  label = "Aggregation level:",
                                                  choices = c("District", "Key Informant"),
                                                  selected = "District",
                                                  justified = TRUE
                                ),
                                
                                conditionalPanel(condition = "input.table_aggregation != 'District'",
                                                 tags$i(h6("Note: Only district-level data can be displayed in the table on the right.
                                                               You can download data on either aggregation level by setting your desired
                                                               parameters and clicking on the download button below.",
                                                          style="color:red")),
                                ),
                                
                                hr(),
                                
                                conditionalPanel(condition = "input.table_aggregation == 'District'",
                                                 pickerInput("table_show_vars",
                                                             label = "Indicators:",   
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             choices = lapply(split(full_list$Item, full_list$Group), as.list),
                                                             selected = c("SMEB", "SMEB food", "SMEB NFI", "SMEB water"),
                                                             multiple = TRUE
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.table_aggregation == 'Key Informant'",
                                                 pickerInput("table_show_vars_ki",
                                                             label = "Indicators:",   
                                                             choices = names(data),
                                                             options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                             selected = names(data),
                                                             multiple = TRUE
                                                 )
                                ),
                                
                                pickerInput("table_district",
                                            label = "Districts:",   
                                            choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                            options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                            selected = plot_location_list$District,
                                            multiple = TRUE
                                ),
                                
                                sliderTextInput("table_date_select",
                                                "Months:",
                                                force_edges = TRUE,
                                                choices = dates,
                                                selected = c(dates_min, dates_max)
                                ),
                                
                                hr(),
                                
                                actionButton("table_reset", "Reset filters"),
                                
                                downloadButton("downloadData", "Download as CSV"),
                                
                                width = 3
                            ),
                            
                            mainPanel(
                                DT::dataTableOutput("table", width = "100%", height = "100%"),
                                width = 9
                            )
                        )
               ),
               
               
               #### * 6.5 Info ######################################################################
               
               tabPanel("Info", icon = icon("info"),
                        
                        column(
                            h4("Background"),
                            p("The Joint Price Monitoring Initiative (JPMI) was developed by the Cash Working Group (CWG) and REACH Initiative to conduct
                              harmonized price monitoring activities among cash actors in Iraq.",
                              style="text-align:justify;margin-bottom:20px"),
                            h4("Methodology"),
                            p("In each assessed marketplace, JPMI field teams record indicators on selected food and non-food items (NFIs) sold by local retailers.
                              Marketplaces are defined as permanent areas of commerce diverse enough to provide access to a variety of food and non-food items
                              (NFIs). Within each district, marketplaces are selected by partner agency field staff in order to ensure that localized knowledge
                              is taken into consideration. Partner staff are instructed to select primary markets within their selected districts to ensure
                              relevant price data is collected.",
                              style="text-align:justify;"),
                            p("Monitored commodities have been identified by the CWG based on what is typically consumed by an average Iraqi household.
                              All assessable commodities of the Survival Minimum Expenditure Basket (SMEB) are included. In line with the purpose of the
                              SMEB, only the lowest available prices are recorded for each item.",
                              style="text-align:justify;"),
                            p("All data collection is conducted through a KoBo-based mobile data collection tool. Following data collection, REACH compiles
                              and cleans all partner data and crosschecks outliers with field teams. The cleaned data is then uploaded and displayed
                              on this website. Data collection for the JPMI occurs on a bi-monthly basis with the website being updated after each round.",
                              style="text-align:justify;"),
                            p("For more details on methodology, refer to the ",
                              tags$a(href="https://www.impact-repository.org/document/reach/7a6169cc/reach_irq_tor_joint_price_monitoring_initiative_jpmi_july_2018_0.pdf", target = "_blank",
                                     "JPMI terms of reference (ToR)"), ".",
                              style="text-align:justify;margin-bottom:20px"),
                            h4("Limitations"),
                            p("All data is gathered by partner agencies of the JPMI. As such, the geographic coverage of the JPMI is determined by
                              partner capacity, ability and interest, and has changed over time. This means that the markets and districts covered
                              across the assessed months are not consistent. These changes in coverage could have an impact on overall reported prices.",
                              style="text-align:justify;margin-bottom:20px"),
                            h4("Contact"),
                            p("In case you have any questions about the JPMI, please contact:", tags$a(href="mailto:casey.clark@reach-initiative.org", "casey.clark@reach-initiative.org")),
                            br(),
                            tags$a(href='https://www.humanitarianresponse.info/en/operations/iraq/cash-working-group', target = "_blank", tags$img(src = "cwg.jpg", height = "40px")), tags$a(href='https://www.reach-initiative.org', target = "_blank", tags$img(src = "reach.jpg", height = "40px")),
                            width=6),
                        
                        column(
                            h4("Current Partners"),
                            tags$a(href="https://www.acted.org/en/", "ACTED", target = "_blank"), br(),
                            tags$a(href="https://www.care-international.org/", "CARE International", target = "_blank"), "/", tags$a(href="https://harikar.org/", "Harikar", target = "_blank"), br(),
                            tags$a(href="https://drc.ngo/", "Danish Refugee Council (DRC)", target = "_blank"), br(),
                            tags$a(href="https://www.rescue.org/", "International Rescue Committee (IRC)", target = "_blank"), br(),
                            tags$a(href="https://www.mercycorps.org/", "Mercy Corps", target = "_blank"), br(),
                            tags$a(href="https://www.nrc.no/", "Norwegian Refugee Council (NRC)", target = "_blank"), br(),
                            tags$a(href="https://www.oxfam.org/en", "Oxfam", target = "_blank"), br(),
                            tags$a(href="https://www.clovekvtisni.cz/en/", "People in Need (PIN)", target = "_blank"), br(),
                            br(),
                            h4("Previous Partners"),
                            tags$a(href="https://www.actioncontrelafaim.org/en/", "Action contre la Faim (ACF)", target = "_blank"), br(),
                            tags$a(href="https://www.drk.de/en/", "German Red Cross (GRC)", target = "_blank"), "/", tags$a(href="https://en.ircs.org.iq/", "Iraqi Red Crescent Society (IRCS)", target = "_blank"), br(),
                            tags$a(href="https://www.medair.org/", "Medair", target = "_blank"), br(),
                            tags$a(href="https://www.pah.org.pl/en/", "Polish Humanitarian Action (PAH)", target = "_blank"), br(),
                            tags$a(href="https://www.qandil.org/", "Qandil", target = "_blank"), br(),
                            tags$a(href="https://www.ri.org/", "Relief International (RI)", target = "_blank"), br(),
                            tags$a(href="https://www.savethechildren.net/", "Save the Children", target = "_blank"), br(),
                            tags$a(href="https://www.tearfund.org/", "Tearfund", target = "_blank"), br(),
                            tags$a(href="https://www.tdh.ch/en", "Terre des Hommes (TdH)", target = "_blank"), br(),
                            tags$a(href="https://www.trianglegh.org/index_en.php", "Triangle Génération Humanitaire (TGH)", target = "_blank"), br(),
                            tags$a(href="https://www.unhcr.org/", "UNHCR", target = "_blank"), br(),
                            tags$a(href="https://www.welthungerhilfe.org/", "Welthungerhilfe", target = "_blank"), br(),
                            tags$a(href="https://www.worldvision.org/", "World Vision", target = "_blank"), br(),
                            width=6)
                        ),

               
               #### * 6.6 JRAM ######################################################################
               
               tabPanel("JRAM", icon = icon("map-marker"),
               
                        div(class="outer",
                            
                            tags$head(includeCSS("styles.css")),
                            
                            leafletOutput("map_jram", width = "100%", height = "100%"),
                            
                            absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
                                width = 330, height = "auto",
                                
                                pickerInput("jram_partner_select",
                                            label = "Partners:",   
                                            choices = partner_list,
                                            selected = partner_list,
                                            options = list(`actions-box` = TRUE),
                                            multiple = TRUE
                                ),
                                pickerInput("jram_governorate_select",
                                            label = "Governorates:",   
                                            choices = sort(unique(jram$Governorate)),
                                            selected = unique(jram$Governorate),
                                            options = list(`actions-box` = TRUE),
                                            multiple = TRUE
                                ),
                                sliderTextInput("jram_date_select",
                                                "Months:",
                                                choices = dates_jram,
                                                force_edges = TRUE,
                                                selected = c(dates_min_jram, dates_max_jram)
                                ),
                                actionButton("jram_reset", "Reset filters")
                            ),
                            
                            absolutePanel(top = 50, left = 55, fixed=TRUE, draggable = FALSE, height = "auto", width = 180,
                                          dropdown(
                                              h4("Introduction"),
                                              p("The Joint Rapid Assessment of Markets (JRAM) was developed and launched by the Cash Working Group of
                                                Iraq (CWG) in April 2017 in order to establish a harmonised and collaborative mechanism for conducting
                                                market assessments in areas newly accessible to humanitarian actors. The CWG serves as the mandating
                                                body for the assessment, with REACH Initiative (REACH) acting as a technical partner and CWG partners
                                                carrying out all field-level data collection, analysis, and reporting. The JRAM was designed to provide
                                                comprehensive market-level data on the impact of protracted crises on markets, specifically with respect
                                                to the status of infrastructure, security, and suppliers; the prices and availability of key goods; and
                                                the response capacity of traders. The data collected are then used to determine if cash and market-based
                                                programming are appropriate intervention mechanisms. This dashboard presents the locations and lead
                                                partners for JRAMs conducted since August 2017.", style="text-align: justify;margin-bottom:20px"),
                                              h4("Methodology"),
                                              p("The JRAM uses a qualitative approach based on the Rapid Assessment of Markets (RAM) system developed by
                                                the International Committee of the Red Cross (ICRC). The populations of interest are market actors in the
                                                selected area, including wholesalers, retailers, and consumers. CWG partners identify relevant markets
                                                for assessment based on operational relevance and feasibility. Partners use purposive sampling to identify
                                                respondents from each of the three groups, and undertake field data collection using an ODK-based
                                                questionnaire. Partners then clean and analyse data using tools and guidance developed by REACH.", style="text-align: justify;margin-bottom:20px"),
                                              h4("Limitations"),
                                              p("Due to the qualitative nature of the JRAM, findings are indicative only and cannot be statistically
                                              generalised to the assessed area. Findings are also relevant only to the specified markets and should not
                                              be regarded as indicative of market functionality elsewhere in Iraq.", style="text-align: justify;margin-bottom:20px"),
                                              h4("Disclaimer"),
                                              p("All responsibility for data quality, analysis, interpretation, and outputs lies with the implementing
                                                partner who led the JRAM. REACH does not validate partner data, analysis, or findings. Providing links
                                                to publicly available results therefore does not necessarily imply endorsement by REACH.", style="text-align: justify;"),
                                              width = "800px",
                                              #size = "sm",
                                              up = FALSE,
                                              style = "jelly", icon = icon("info"),
                                              tooltip = tooltipOptions(placement = "right", title = "Click for more details on the JRAM."),
                                              animate = animateOptions(
                                                  enter = "fadeInLeft",
                                                  exit = "fadeOutLeft",
                                                  duration = 0.5)
                                          )                                                                   # close dropdownButton
                            )                                                                                 # close absolutePanel
                        )                                                                                     # close div
               )                                                                                              # close tabPanel
    )                                                                                                         # close navbarpage
)                                                                                                             # close bootstrappage

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
    
    #### 7.1 Home ######################################################################
    
    output$map_home <- renderLeaflet({
        map_home <- leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, dragging = FALSE, minZoom = 12, maxZoom = 12)) %>%
            setView(lng = 44.379140, lat = 33.315743, zoom = 12) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB",
                             options = providerTileOptions(opacity = 0.8))
    })
    
    output$downloadDataLatest <- downloadHandler(
        filename = function() {
            paste("IRQ-JPMI-download-", format(dates_max, "%b"), "-", format(dates_max, "%Y"),".csv", sep = "")
        },
        content = function(file) {
            write.csv(data_latest, file, row.names = FALSE)
        }
    )
    
    
    #### 7.2 Plot ######################################################################

    plot_district_select <- reactive({
        if (input$plot_by_district_item == "Item") {input$select_byitem_district} else {input$select_bydistrict_district}
    })
        
    plot_governorate_select <- reactive({
        if (input$plot_by_governorate_item == "Item") {input$select_byitem_governorate} else {input$select_bygovernorate_governorate}
    })
    
    plot_item_select <- reactive({
        if (input$plot_aggregation == 'Country' | (input$plot_aggregation == 'Governorate' & input$plot_by_governorate_item == 'Item') | (input$plot_aggregation == 'District' & input$plot_by_district_item == 'Item')) {input$select_byitem_item} else if (input$plot_aggregation == 'District' & input$plot_by_district_item == 'District') {input$select_bydistrict_item} else {input$select_bygovernorate_item}
    })
    
    plot_datasetInput <- reactive({prices_long %>%
            filter(
                is.null(plot_item_select()) | Item %in% plot_item_select()
            ) %>%
            execute_if(input$plot_type == 'Line Graph' | input$plot_aggregation != 'Country',
                       filter(
                           Date >= input$select_date[1] & Date <= input$select_date[2]
                       )
            ) %>%
            execute_if(input$plot_type == 'Boxplot' & input$plot_aggregation == 'Country',
                       filter(
                           Date == input$select_date_boxplot
                       )
            ) %>%
            execute_if(input$plot_aggregation == 'District', filter(is.null(plot_district_select()) | District %in% plot_district_select())) %>%
            execute_if(input$plot_aggregation == 'Governorate', select(-District)) %>%
            execute_if(input$plot_aggregation == 'Governorate', group_by(Date, Governorate, Item)) %>%
            execute_if(input$plot_aggregation == 'Governorate', summarise_all(median, na.rm = TRUE)) %>%
            execute_if(input$plot_aggregation == 'Governorate', filter(is.null(plot_governorate_select()) | Governorate %in% plot_governorate_select())) %>%
            execute_if(input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country', select(-Governorate, -District)) %>%
            execute_if(input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country', group_by(Date, Item)) %>%
            execute_if(input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country', summarise_all(median, na.rm = TRUE)) %>%
            execute_if(((input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country') | input$plot_aggregation != 'Country') & input$select_index == 'TRUE' & input$plot_aggregation == 'District',
                       group_by(Governorate, District, Item)) %>%
            execute_if(((input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country') | input$plot_aggregation != 'Country') & input$select_index == 'TRUE' & input$plot_aggregation == 'Governorate',
                       group_by(Governorate, Item)) %>%
            execute_if(((input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country') | input$plot_aggregation != 'Country') & input$select_index == 'TRUE' & input$plot_aggregation == 'Country',
                       group_by(Item)) %>%
            execute_if(((input$plot_type == 'Line Graph' & input$plot_aggregation == 'Country') | input$plot_aggregation != 'Country') & input$select_index == 'TRUE',
                       mutate(Price = round(((Price / c(Price[Date == input$select_date_index], NA)[1])-1)*100, digits = 1))) %>%
            filter(!is.na(Price))
    })
    
    output$plot_text <- renderText({
        if (nrow(plot_datasetInput()) == 0) {
            "There is no data for this selection. Change the time frame or select another indicator/location."} else {""}
    })
    
    output$graph <- renderHighchart({
        
        if (input$plot_aggregation == "Country" | (input$plot_aggregation == "District" & input$plot_by_district_item == "Item") | (input$plot_aggregation == "Governorate" & input$plot_by_governorate_item == "Item")) {
            graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Item))
            
        } else if (input$plot_aggregation == "District"){
            graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = District))
            
        } else {
            graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Governorate))
        }
        graph <- graph %>%
            hc_xAxis(title = "") %>%
            hc_colors(cols) %>%
            hc_exporting(
                enabled = TRUE,
                filename = paste0("IRQ-JPMI-linegraph_export-", Sys.Date()),
                buttons = list(
                    contextButton = list(
                        menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")
                    )),
                sourceWidth = 1000,
                sourceHeight = 600
            ) %>%
            execute_if(input$select_index == 'TRUE',
                       hc_xAxis(plotLines = list(list(label = list(text = "Ref. month", style = list(fontSize = "11px", color = "dimgrey")), width = 1, color = "#FF0000", dashStyle = "dash", value = datetime_to_timestamp(as.Date(input$select_date_index, tz = 'UTC')))))
            ) %>%
            execute_if(input$select_index == 'TRUE',
                       hc_yAxis(labels = list(format = "{value}%"), title = list(text = "% deviation from ref. month"), plotLines = list(list(width = 1, color = "#FF0000", dashStyle = "dash", value = 0, zIndex=2)))
            ) %>%
            execute_if(input$select_index == 'TRUE',
                       hc_tooltip(valueSuffix = "%") 
            ) %>%
            execute_if(input$select_index == 'FALSE',
                       hc_tooltip(valueSuffix = " IQD") 
            ) %>%
            execute_if(input$select_index == 'FALSE',
                       hc_yAxis(min = 0, title = list(text = "Price (in IQD)"))
            )
    })
    
    
    output$boxplot <- renderHighchart({
        
        boxplot <- hcboxplot(x = plot_datasetInput()$Price, var = plot_datasetInput()$Item, outliers = FALSE) %>%
            hc_chart(type = "column") %>%
            hc_tooltip(valueSuffix = " IQD") %>%
            hc_yAxis(min = 0, title = list(text = "Price (in IQD)")) %>%
            hc_exporting(
                enabled = TRUE,
                filename = paste0("IRQ-JPMI-boxplot_export-", Sys.Date()),
                buttons = list(
                    contextButton = list(
                        menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")
                    )),
                sourceWidth = 1000,
                sourceHeight = 600
            ) %>%
            hc_plotOptions(boxplot = list(fillColor = "#e9e9e9",
                                          lineWidth = 1,
                                          lineColor = "#5c5c5c",
                                          medianWidth = 2,
                                          medianColor = "#d9230f",
                                          stemColor = "#5c5c5c",
                                          stemWidth = 1,
                                          whiskerColor = "#5c5c5c",
                                          whiskerLength = "0%",
                                          whiskerWidth = 1
                                          ),
                           series = list(dataSorting = list(enabled = TRUE, sortKey = "median"))
                           )

    })

    
    #### 7.3 Map ######################################################################
    
    map_indicator_select <- reactive({
        if (input$map_indicator_select == "Prices") {input$map_item_select} else {input$map_addindicator_select}
    })
        
    output$map_text <- renderText({""})
    
    output$map <- renderLeaflet({
        
        prices_map <- prices %>% filter(Date == dates_max)
        district <- left_join(district, prices_map, by = c("ADM2_EN" = "District"))
        
        labels <- sprintf("<strong>%s</strong><br/>%s IQD (%s)", district$ADM2_EN, format(district$SMEB, big.mark=","), format(district$Date, "%b %Y")) %>% lapply(htmltools::HTML)
        pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                            domain = district$SMEB, na.color = "transparent"
        )
        
        map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
            addMapPane(name = "base", zIndex = 410) %>% 
            addMapPane(name = "polygons", zIndex = 420) %>% 
            addMapPane(name = "label", zIndex = 430) %>%
            addPolygons(data = district, group = "District", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(district$SMEB),
                        stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666666",
                            fillOpacity = 0.75,
                            bringToFront = TRUE
                        ),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"),
                        options = leafletOptions(pane = "polygons")
            ) %>%
            addPolygons(data = governorate, group = "Governorate", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 0.6, opacity = 1) %>%
            addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
            addLegend("bottomright", pal = pal, values = district$SMEB,
                      title = "Price:",
                      labFormat = labelFormat(prefix = "IQD "),
                      opacity = 1
            ) %>%
            setMapWidgetStyle(style = list(background = "transparent")) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                             options = c(providerTileOptions(opacity = 0.6),
                                         leafletOptions(pane = "base"))) %>%
            addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                             options = c(providerTileOptions(opacity = 1),
                                         leafletOptions(pane = "label"))) %>%
            addLayersControl(overlayGroups = c("Labels", "Country", "Governorate", "District", "Base map"))
    })

    observe({
        
        prices_map <- prices %>% filter(Date == input$map_date_select)
        district <- left_join(district, prices_map, by = c("ADM2_EN" = "District"))
        indicators_map <- indicators %>% filter(Date == input$map_date_select)
        district <- left_join(district, indicators_map, by = c("ADM2_EN" = "District"))
        
        output$map_text <- renderText({
            if (all(is.na(district[[map_indicator_select()]])) == TRUE) {
            "There is no data for this selection. Select another month or indicator."} else {NULL}
        })
        
        if (all(is.na(district[[map_indicator_select()]])) == TRUE) {
            return(NULL)
        } 
        
        if (input$map_indicator_select == "Prices") {
        labels <- sprintf("<strong>%s</strong><br/>%s IQD (%s)", district$ADM2_EN, format(district[[map_indicator_select()]], big.mark=","), format(district$Date.x, "%b %Y")) %>% lapply(htmltools::HTML)
        } else {
            labels <- sprintf("<strong>%s</strong><br/>%s&#37; of assessed traders (%s)", district$ADM2_EN, district[[map_indicator_select()]], format(district$Date.x, "%b %Y")) %>% lapply(htmltools::HTML)
        }
                                                                                                                                                  
        pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                            domain = district[[map_indicator_select()]], na.color = "transparent"
                            )
        pal2 <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                            domain = range(0,100), na.color = "transparent"
        )
        
        leafletProxy("map") %>%
            clearControls %>%
            clearGroup("District") %>%
            execute_if(input$map_indicator_select == "Prices",
                       addPolygons(data = district, group = "District", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(district[[map_indicator_select()]]),
                                   stroke = TRUE, color = "#58585A", weight = 0.4, opacity = 1,
                                   highlight = highlightOptions(
                                       weight = 5,
                                       color = "#666666",
                                       fillOpacity = 0.75,
                                       bringToFront = TRUE
                                   ),
                                   label = labels,
                                   labelOptions = labelOptions(
                                       style = list("font-weight" = "normal", padding = "3px 8px"),
                                       textsize = "15px",
                                       direction = "auto"),
                                   options = leafletOptions(pane = "polygons")
                       )
            )%>%
            execute_if(input$map_indicator_select == "Availability and Challenges",
                       addPolygons(data = district, group = "District", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal2(district[[map_indicator_select()]]),
                                   stroke = TRUE, color = "#58585A", weight = 0.4, opacity = 1,
                                   highlight = highlightOptions(
                                       weight = 5,
                                       color = "#666666",
                                       fillOpacity = 0.75,
                                       bringToFront = TRUE
                                   ),
                                   label = labels,
                                   labelOptions = labelOptions(
                                       style = list("font-weight" = "normal", padding = "3px 8px"),
                                       textsize = "15px",
                                       direction = "auto"),
                                   options = leafletOptions(pane = "polygons")
                       )
            ) %>%
            execute_if(input$map_indicator_select == "Prices",
                       addLegend("bottomright", pal = pal, values = district[[map_indicator_select()]],
                                 title = "Price:",
                                 labFormat = labelFormat(prefix = "IQD "),
                                 opacity = 1
                       )
            ) %>%
            execute_if(input$map_indicator_select == "Availability and Challenges",
                       addLegend("bottomright", pal = pal2, values = range(0,100),
                                 title = "% of traders:",
                                 labFormat = labelFormat(suffix = " %"),
                                 opacity = 1
                       )
            )
    })
    
    
    #### 7.4 Data Explorer ######################################################################

    table_datasetInput1 <- reactive({
            full %>% filter(
                is.null(input$table_district) | District %in% input$table_district,
                Date >= input$table_date_select[1] & Date <= input$table_date_select[2]
            ) %>%
                select("Date", "Governorate", "District", input$table_show_vars)
    })
    
    table_datasetInput2 <- reactive({
            data %>% filter(
                is.null(input$table_district) | district %in% input$table_district,
                date >= input$table_date_select[1] & date <= input$table_date_select[2]
            ) %>%
                select(input$table_show_vars_ki)
    })
    
    table_datasetInput <- reactive({
        if (input$table_aggregation == "District") {table_datasetInput1()} else {table_datasetInput2()}
    })
    
    output$table <- renderDT({
        
        DT::datatable(
            table_datasetInput1(),
            extensions = c('ColReorder'),
            options = list(autoWidth = TRUE, dom = 't', paging = FALSE, colReorder= TRUE, fixedHeader = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 3)),
            rownames = FALSE,
            style = 'bootstrap', class = 'table-condensed table-hover table-striped'
        ) %>%
            formatRound(
                4:ncol(table_datasetInput1()),
                digits = 0,
                interval = 3,
                mark = ",",
                dec.mark = getOption("OutDec")
            ) %>%
            formatStyle(names(table_datasetInput1()), "white-space"="nowrap")
            #formatStyle(3, `border-right` = "solid 1px gainsboro")
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("IRQ-JPMI-data-download-", Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
            write.csv(table_datasetInput(), file, row.names = FALSE, na = "")
        }
    )
    
    observe({
        input$table_reset
        updatePickerInput(session, "table_district", selected = plot_location_list$District)
        updatePickerInput(session, "table_show_vars", selected = c("Date", "Governorate", "District", "SMEB", "SMEB food", "SMEB NFI", "SMEB water"),)
        updatePickerInput(session, "table_show_vars_ki", selected = names(data),)
        updateSliderTextInput(session, "table_date_select", selected = c(dates_min, dates_max))
    }) 
    
    
    #### 7.6 JRAM ######################################################################
    
    output$map_jram <- renderLeaflet({                                            # define leaflet map
        map_jram <- leaflet(data = jram, options = leafletOptions(attributionControl=FALSE, zoomControl = TRUE)) %>%
            #setView(lng = 44.376, lat = 35.0, zoom = 7) %>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
            addAwesomeMarkers(~lon, ~lat, popup = ~popup_data, label = ~label)
    })
    
    observe({                                                                     # update markers on map if selection changes
        jram_select <- jram %>%
            filter(is.null(input$jram_governorate_select) | Governorate %in% input$jram_governorate_select,
                   is.na(Date) | is.null(input$jram_date_select) | Date >= input$jram_date_select[1] & Date <= input$jram_date_select[2],
                   grepl(paste(input$jram_partner_select, collapse = "|"), partners)
            )
        
        leafletProxy("map_jram", data = jram_select) %>%
            clearMarkers() %>%
            addAwesomeMarkers(~lon, ~lat, popup = ~popup_data, label = ~label) 
    })
    
    observe({                                                                     # set parameters for reset button for each of the inputs
        input$jram_reset
        updatePickerInput(session, "jram_partner_select", selected = partner_list)
        updatePickerInput(session, "jram_governorate_select", selected = unique(jram$Governorate))
        updateSliderTextInput(session, "jram_date_select", selected = c(dates_min_jram, dates_max_jram))
    })
    
}                                                                                 # close server function

shinyApp(ui = ui, server = server)                                                # run the application