library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(ggridges)
library(dplyr)
library(ggmap)
library(viridis)
library(gganimate)
library(countrycode)


data <- read.csv("suicide.csv")


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Suicides Rate")  

#Sidebar content of the dashboard
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("question")),
    menuItem("Global", tabName = "global", icon = icon("globe")),
    menuItem("Sex", tabName = "sex", icon = icon("venus-mars")),
    menuItem("Age group & Generation", tabName = "age_gen", icon = icon("accessible-icon")),
    menuItem("Indicators", tabName = "indicators", icon = icon("chart-line")),
    menuItem("Basic Codes", tabName = "code", icon = icon("search"))
  )
)

##page 0
 intro_row1 <- fluidRow(
   infoBox(
     title = "Why do we care about suicides"
     ,textOutput("intro1"),
     tags$head(tags$style("#intro2{font-size: 20px}")),
     width = 12
   )
 )
 
 intro_row2 <- fluidRow(
   box(
     title = "References & More Info."
     ,status = "primary"
     ,solidHeader = TRUE 
     ,collapsible = TRUE
     ,textOutput("intro2"),
     tags$head(tags$style("#intro2{font-size: 10px}"))
   )
 )

##page 1
global_row2 <- fluidRow(
  box(
    title = "Global suicides"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("global_plot"))
  ,
  box(
      selectInput(inputId = "select_country",
      label = "Select country",
     choices = unique(data$X...country)
   )
),
box(
  title = "What's the trend?"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE
  ,textOutput("global_text1"),
  tags$head(tags$style("#global_text1{font-size: 20px}"))
)
)


global_row1 <- fluidRow(
  
  box(
    title = "Global suicides by countries"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("global_map", height = "500px", width = "100%"),
    width = 12
  )) 

global_row3 <- fluidRow( 
  box(
    title = "Suicides per capita by countries"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("global_bar"),
    width = 12)
  )

##page 2


sex_row1 <- fluidRow( 
  box(
    title = "Global suicides by gender"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("sex_plot")
  ),
  box(
    title = "Male higher than Female",
    status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("sex_text1"),
    tags$head(tags$style("#sex_text1{font-size: 20px}"))
  )
  )

sex_row2 <- fluidRow(
  box(
  title = "Suicides by gender by countries"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,plotOutput("sex_bar"),
  width = 12
))

##page 3

age_row <- fluidRow( 
  box(
    title = "Global suicides by age groups"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("age_plot")
  ),
  box(
    title = "Global suicides by generations"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("gen_plot")
  ))

age_row2 <- fluidRow( 
  box(
    title = "Sadder when Older?"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("age_text"),
    tags$head(tags$style("#age_text{font-size: 20px}"))
  ),
  box(
    title = "Decrease through generations?"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("gen_text"),
    tags$head(tags$style("#gen_text{font-size: 20px}"))
  ))

age_row3 <- fluidRow( 
  box(
    title = "Generation definition"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("gen_text1"),
    tags$head(tags$style("#intro2{font-size: 20px}")),
    width = 12
  ))

##page 4
gdp_row1 <- fluidRow( 
  box(
    title = "GDP vs. Suicides per capita"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("gdp_plot")
  ),
  box(
    title = "HDI vs. Suicides per capita"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("hdi_plot")
  ))

gdp_row2 <- fluidRow( 
  box(
    title = "Richer ??? More suicides"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("gdp_text"),
    tags$head(tags$style("#gdp_text{font-size: 20px}"))
  ),
  box(
    title = "More developed, more suicides"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,textOutput("hdi_text"),
    tags$head(tags$style("#hdi_text{font-size: 20px}"))
  ))

##page 5
rmd <- fluidPage(
  includeMarkdown("proj_try.Rmd")
)

# combine fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem("intro", intro_row1, intro_row2), 
    tabItem("global", global_row1, global_row2, global_row3), 
    tabItem("sex", sex_row1, sex_row2),
    tabItem("age_gen", age_row, age_row2, age_row3),
    tabItem("indicators", gdp_row1, gdp_row2), 
    tabItem("code", rmd)
  )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Suicides Rates Ovewview from 1985 to 2016"),
  sidebar,
  body
)

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation
  data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
  data$generation <- factor(data$generation, levels = c("G.I. Generation", "Silent", "Boomers", "Generation X", "Millenials", "Generation Z"))
  
  total <- data %>%
    select("X...country":"suicides.100k.pop", "HDI.for.year":"generation") %>%
    group_by(year) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
  mean <- round(mean(total$suicides_per_100k), 2)
  
  #intro
  output$intro1 <- renderText({"Van Gogh, Hemingway, Avicii, and other great souls committed suicide. Many people mourn and remember them even today. 
    Close to 800 000 less famous people die by suicide every year; that is one person every 40 seconds. While the link between suicide and mental disorders is well established in high-income countries, many suicides happen impulsively in moments of crisis, such as when facing life stresses such as financial problems, relationship break-up or chronic illness. 
    Suicide does not just occur in high-income countries, but is a global phenomenon in all regions of the world. In fact, over 79% of global suicides occurred in low- and middle-income countries in 2016.
    Is the situation getting better or worse over years? Which group of people is at the highest risk? In this project, I compare socio-economic info with suicide rates by year and country, to see where the world is heading."})
  output$intro2 <- renderText({
    "Dataset used in this project can be found at https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016, which combines information available at World Health Organization (http://www.who.int/mental_health/suicide-prevention/en/), World Bank (http://databank.worldbank.org/data/source/world-development-indicators#), and United Nations Development Program (http://hdr.undp.org/en/indicators/137506).
    More information on suicide prevention can be found on WHO website (https://www.who.int/health-topics/suicide#tab=tab_1).
    National suicide prevention hotline: 1-800-273-8255. "
  })
  output$picture <- renderImage({
    return(list(src = "cristian-newman-4GHL4ozk94Y-unsplash.jpg",contentType = "image/png"))
  }, deleteFile = FALSE)
  
  #global
  
  ##global plot
  
  output$global_plot <- renderPlot({
    cdata <- filter(data, X...country == input$select_country) %>%
          select("X...country":"suicides.100k.pop", "HDI.for.year":"generation") %>%
          group_by(year) %>%
          summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
    
    ggplot(total) +
      geom_point(aes(year, suicides_per_100k)) +
      geom_smooth(aes(year, suicides_per_100k), se = T, span = .3) +
      coord_cartesian() +
      geom_hline(yintercept = 13.08, linetype = "dashed") +
      geom_point(data = cdata, aes(year, suicides_per_100k), color = "red") +
      geom_smooth(data = cdata, aes(year, suicides_per_100k), se = T, span = .3, color = "red") +
      ggtitle("Suicides per 100k population by year")+
      ylab("suicides per capita")
  })
  
 output$global_text1 <- renderText({
   "You can choose a country from the above dropbox to compare its trend with the global trend.
   \n We can observe that global suicides rates are between 10-15 suicides per 100k population in 1985-2016, reaching the highest in mid 1990s and gradually decreasing afterwards. Its mean value is 13.08. \n
   However, the suicides per capita varies greatly from country to country. Some middle European countries have the highest suicides rates of 30+, while countries in tropical zone and south semisphere appears to have lower rates.
   The suicides per capita of some countries, especially European countries, dropped significantly over years (try Austria); others rises (try Korea)."
 })
  
  ##global bar
  
  country_total_pop <- data %>%
    group_by(X...country, year) %>%
    summarize(population = sum(population)) %>%
    ungroup() %>%
    group_by(X...country) %>%
    summarise(average_population_1m = (sum(population)/n())/1e6) %>%
    rename(country = X...country)
  
  country <- data %>%
    group_by(X...country, year) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000) %>%
    ungroup() %>%
    group_by(X...country) %>%
    summarize(average_suicides_per_100k = sum(suicides_per_100k)/n()) %>%
    rename(country = X...country) 
  
  Country <- left_join(country, country_total_pop, by = "country")
  
  
  
  output$global_bar <- renderPlot({
    Country <- Country %>% mutate(ToHighlight = ifelse( country == input$select_country, "yes", "no" ) )
    ggplot(Country) +
      geom_col(aes(reorder(country, average_suicides_per_100k), average_suicides_per_100k, fill = ToHighlight)) +
      theme(axis.text.x = element_text(angle = 80, color = "black"), legend.position="top") +
      scale_fill_manual( values = c("yes"="tomato", "no" = "grey"), guide = FALSE ) +
      ggtitle("Suicides per 100k population by country") +
      xlab("Country") +
      ylab("Average suicides per capita from 1985-2016")
  })
  
  ##global map
  map_world <- map_data("world")
  country_map <- Country 
  country_map$country <- recode(country_map$country, 'United States' = 'USA','United Kingdom' = 'UK', "Russian Federation" = "Russia", "Republic of Korea" = "South Korea", "United Arab Emirates"= "UAE")
  world_map <- left_join(map_world, country_map, by = c('region' = 'country'))
  
  output$global_map <- renderPlot({
    ggplot(world_map) +
      geom_polygon(aes(long, lat, group = group, fill = average_suicides_per_100k)) +
      ylim(c(-70, 90)) +
      labs(fill = "suicides per capita") +
      scale_fill_viridis_c() +
      theme(legend.position="right")+
      ggtitle("Suicides per 100k population by country") +
      xlab("Longitude") +
      ylab("Latitude") +
      coord_quickmap()
  })
  
#sex
  sex <- data %>%
    select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
    group_by(year, sex) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
 ##sex plot
  output$sex_plot <- renderPlot({
    ggplot() +
      geom_point(data = sex, aes(year, suicides_per_100k, color = sex)) +
      geom_smooth(data = sex, aes(year, suicides_per_100k, color = sex), span = 0.3)+
      geom_smooth(data = total, aes(year, suicides_per_100k), span = 0.2) +
      geom_hline(yintercept = 13.08, linetype = "dashed") +
      ggtitle("Suicides per 100k population by gender") +
      ylab("Suicides per capita")
})
  
  output$sex_text1 <- renderText({
    "Globally speaking, male has 4 times more cases of suicides than female. When looking at each country individually,
    we can see that male's suicides rate is higher than females in all countries. This implies a higher risk for male."
  })
  ##sex bar
  
  sex_country <- data %>%
    select("X...country":"suicides.100k.pop", "HDI.for.year":"generation") %>%
    group_by(X...country, sex) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
  
  output$sex_bar <- renderPlot({
    ggplot(sex_country) +
      geom_col(aes(reorder(X...country, suicides_per_100k), suicides_per_100k, fill = sex), identity = "dodge") +
      theme(axis.text.x = element_text(angle = 80, color = "black"), legend.position="top") +
      ggtitle("Suicides per 100k population by gender and countries") +
      ylab("Average suicides per capita from 1985-2016")
  })

#Age&gen
   
  ##Age
  age <- data %>%
    select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
    group_by(year, age) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000)
  
  output$age_plot <- renderPlot({
    ggplot() +
    geom_point(data = age, aes(year, suicides_per_100k, color = age)) +
    geom_smooth(data = age, aes(year, suicides_per_100k, span = .2, color = age)) +
    geom_smooth(data = total, aes(year, suicides_per_100k), span = .2, color = "black") +
    geom_hline(yintercept = 13.08, linetype = "dashed") +
    ggtitle("Suicides per 100k population by age groups") +
    ylab("Suicides per capita")
  })
  
  ##Gen
  gen <- data %>%
    select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
    group_by(year, generation) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000)
  
  output$gen_plot <- renderPlot({
    ggplot() +
      geom_point(data = gen, aes(year, suicides_per_100k, color = generation)) +
      geom_smooth(data = gen, aes(year, suicides_per_100k, color = generation), span =.5) +
      geom_smooth(data = total, aes(year, suicides_per_100k), span = .3, color = "black") +
      geom_hline(yintercept = 13.08, linetype = "dashed") +
      ggtitle("Suicides per 100k population by generations") +
      ylab("Suicides per capita")
  })
  
  output$age_text <- renderText({
    "From the age graph, we can see that the suicides incidents increase as people get older, while all age groups follow a similar pattern.
    Notice that suicides rate for 75+ elderly drops the most over years, yet still remains the highest."
  })
  output$gen_text <- renderText({
    "For the generation graph, we can see it is true that, for each generation, suicides rates increase with age.
    We may say that the suicides rate slightly decrease over generation, but starting from Gen X the suicides rate for each generation at certain age are roughly similar.
    Notice how Gen X and Millenials have 'discrete strata' for differnet age." 
  })
  output$gen_text1 <- renderText({
   "The Greatest Generation, also known as the 'G.I. Generation', includes the veterans who fought in World War II. They were born from around 1901 to 1927 and came of age during the Great Depression.
    The Silent Generation were born from approximately 1928 to 1945, who may have fought the Korean War and many of those who may have fought during the Vietnam War.
    Baby boomers are the generation that were born mostly following World War II, typically born from 1946 to 1964.
    Generation X is the generation following the baby boomers with birth years around 1965 to 1980.
    Millennials are the people following Generation X with birth years of 1981 to 1996.
    Generation Z, or simply Gen Z, is the cohort of people born after the Millennials, with cutoff being 2012.
    ---Wikipedia"
  })
  
#Indicators
  
  country_year <- data %>%
    group_by(country.year) %>%
    summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000) %>%
    ungroup()
  
  eco <- data %>%
    select(X...country, year, country.year:gdp_per_capita....) %>%
    unique() %>%
    rename(country = X...country, gdp_per_capita = gdp_per_capita....)
  
  country_eco <- inner_join(country_year, eco)
  country_eco$continent <- countrycode(country_eco$country, "country.name", "continent")
  
  ##GDP
  continent_eco_gdp <- country_eco %>%
    group_by(year, country) %>%
    na.omit(gdp_per_capita) %>%
    summarise(pop = mean(population), suicides_per_100k = mean(suicides_per_100k), gdp_per_capita = mean(gdp_per_capita)) %>%
    ungroup() 
  continent_eco_gdp$continent <- countrycode(continent_eco_gdp$country, "country.name", "continent")
  country_eco_gdp <- continent_eco_gdp %>%
    group_by(country) %>%
    summarise(suicides_per_100k = mean(suicides_per_100k), gdp_per_capita = mean(gdp_per_capita))
  country_eco_gdp$continent <- countrycode(country_eco_gdp$country, "country.name", "continent")
  country_eco_gdp
  
  output$gdp_plot <- renderPlot({
    ggplot(country_eco_gdp, aes(gdp_per_capita, suicides_per_100k)) +
      geom_point(aes(color = continent), show.legend = T) +
      geom_smooth(method = "lm", formula = y~x) +
      ggtitle("Suicides per 100k population") +
      ylab("GDP versus suicides per capita") +
      xlab("GDP per capita") + 
      annotate(geom="text", x=65000, y=32, label="y=17353.5+182.2x, r^2=0.006415",
                                        color="red")
  })
##HDI
continent_eco_hdi <- country_eco %>%
  group_by(year, country) %>%
  na.omit(HDI.for.year) %>%
  summarise(pop = mean(population), suicides_per_100k = mean(suicides_per_100k), HDI = mean(HDI.for.year)) %>%
  ungroup() 
country_eco_hdi <- continent_eco_hdi %>%
  group_by(country) %>%
  summarise(suicides_per_100k = mean(suicides_per_100k), HDI= mean(HDI))
country_eco_hdi$continent <- countrycode(country_eco_hdi$country, "country.name", "continent")

output$hdi_plot <- renderPlot({
  ggplot(country_eco_hdi,aes(HDI, suicides_per_100k)) +
    geom_point(aes(color = continent)) +
    geom_smooth(method = "lm", formula = y~x) +
    ggtitle("HDI versus suicides per 100k population") +
    ylab("Suicides per capita") +
    xlab("Human Development Index") +
    annotate(geom="text", x=0.85, y=35, label="y=0.740259+0.002516x, r^2=0.06512",
             color="red")
})

output$gdp_text <- renderText({
  "The linear model has a small R squared value and a p-value of 0.453, fail to reject that suicides rates does not change significantly with GDP. 
  Since most countries locate in the bottom left corner, disregarding a few data points might produce a linear model with bigger r squared."
})
output$hdi_text <- renderText({
  "From simple observation and calculation, the increase in HDI can better explain the increase in suicides rates.
  The p-value equals 0.0152, which implies statistical significance. However, it is interesting, since a higher HDI implies a better development of a country. 
  (HDI considers life expectancy, education, and economics)." 
})

}


shinyApp(ui, server)
