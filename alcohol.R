library(tidyverse)
library(ggplot2)
h <- head
s <- summary  
g <- glimpse  
t <- tail

# gen1: a master data frame 
gen <- read.csv("Gender_StatsData.csv", stringsAsFactors = FALSE)
gen1 <- gen %>% 
        rename(Country = ï..Country.Name)

# collection of indicators
indicator <- unique(gen$Indicator.Name)

# collection of country
country <- unique(gen$Country)

# Indicator Category: alcohol & suicide
alc <- c("Total alcohol consumption per capita, female (liters of pure alcohol, projected estimates, female 15+ years of age)",
         "Total alcohol consumption per capita, male (liters of pure alcohol, projected estimates, male 15+ years of age)")
suicide <- c("Suicide mortality rate, female (per 100,000 female population)",
             "Suicide mortality rate, male (per 100,000 male population)")

# primary data cleaning for suicide table
gen_suicide <- gen1 %>%
        filter(Indicator.Name %in% suicide) %>%
        select(Country, Country.Code, Indicator.Name, X2000, X2005, X2010, X2015, X2016)
gen_suicide_2016 <- gen_suicide %>%
        select(-c(X2000, X2005, X2010, X2015))


#primary data cleaning for alcohol table
gen_alc <- gen1 %>% 
        filter(Indicator.Name %in% alc) %>%
        select(Country, Country.Code, Indicator.Name, X2016)

# Country Category
by_region <- c("World",
               "Arab World",
               "Central Europe and the Baltics",
               "East Asia & Pacific",
               "Euro area",
               "Europe & Central Asia",
               "European Union",
               "Latin America & Caribbean",
               "Middle East & North Africa",
               "South Asia",
               "Sub-Saharan Africa",
               "North America")
by_income <- c("World",
               "High income", 
               "Upper middle income",
               "Middle income",
               "Lower middle income",
               "Low income")
by_country <- unique(gen_alc$Country[93:nrow(gen_alc)])


# data cleaning upon Country category
gen_alc_region <- gen_alc %>%
        filter(Country %in% by_region) 

gen_alc_income <- gen_alc %>%
        filter(Country %in% by_income)

gen_alc_country <- gen_alc %>% 
        filter(Country %in% by_country)


# function 
spread_gender <- function(df) {
        df <- spread(df, Indicator.Name, X2016)
        names(df) <- c("Country", "Country_Code", "Female", "Male")
        df1 <- gather(df, Gender, indicator, -c(Country, Country_Code))
        return(df1)
}


# interminate data frames 
gen_alc_region1 <- spread_gender(gen_alc_region)
gen_alc_income1 <- spread_gender(gen_alc_income)
gen_alc_country1 <- spread_gender(gen_alc_country)
gene_suicide_2016_1 <- spread_gender(gen_suicide_2016)


# data cleaning for alcohol consumption by region plot
gen_alc_region2 <- gen_alc_region1 %>%
        rename(Region = Country) 
gen_alc_region3 <- spread(gen_alc_region2, Gender, indicator)
gen_alc_region4 <- gen_alc_region3 %>% 
        mutate(Female_per_Male = round(Female / Male * 100, digits = 2))



# data cleaning for alcohol consumption by income plot
gen_alc_income2 <- gen_alc_income1 %>%
        rename(Income_Class = Country) %>% 
        mutate(Income_Class = factor(Income_Class, 
                                     levels = c("Low income", 
                                                "Lower middle income", 
                                                "Middle income",
                                                "Upper middle income", 
                                                "High income",
                                                "World")))
gen_alc_income3 <- spread(gen_alc_income2, Gender, indicator)
gen_alc_income4 <- gen_alc_income3 %>%
        mutate(Female_per_Male = round(Female / Male * 100, digits = 2))


# data cleaning for alcohol consumption vs suicide
gen_alc_country_suicide <- gen_alc_country1 %>%
        inner_join(gene_suicide_2016_1, 
                   by = c("Country", "Country_Code", "Gender"), 
                   suffix = c("Alcohcol", "Suicide"))

gen_alc_country2 <- spread(gen_alc_country1, Gender, indicator)
gen_alc_country3 <- gen_alc_country2[complete.cases(gen_alc_country2), ]

# data cleaning  for alcohol consumption by country
gen_alc_country_suicide1 <- gen_alc_country_suicide[complete.cases(gen_alc_country_suicide), ]
gen_alc_country_suicide2 <- gen_alc_country_suicide1 %>%
        rename(Alcohol_Consumption = indicatorAlcohcol,
               Suicide_Mortility = indicatorSuicide)


# plots for alcohol consumption by region

alcohol_consumption_by_region <-
        ggplot(gen_alc_region2, aes(x = Region, y = indicator, fill = Region)) + 
        geom_bar(stat = "identity", col = "black") + 
        facet_grid(. ~ Gender) + 
        ylab("Alcohol Consumption per Capita (Liters)") +
        ggtitle("Alcohol Consumption by Region and Gender in 2016") + 
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(),
              panel.background = element_rect(fill = "white"), 
              panel.grid.major.y = element_line("grey"),
              panel.grid.major.x = element_blank()) 

alcohol_consumption_by_region_by_gender <- 
        ggplot(gen_alc_region4, aes(x = Region, y = Female_per_Male, fill = Region)) +
        geom_bar(stat = "identity", col = "black") + 
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(),
              panel.background = element_rect(fill = "white"), 
              panel.grid.major.y = element_line("grey"),
              panel.grid.major.x = element_blank()) +
        ylab("Relative Alcohol Consumption (Female per Male, %)") + 
        ggtitle("Alcohol Consumption of Female Relative to Male by Region") + 
        scale_fill_hue(c=45, l=80)



# plots for alcohol consumption by income
alcohol_consumption_by_income <- 
        ggplot(gen_alc_income2, aes(x = Income_Class, y = indicator, fill = Income_Class)) + 
        geom_bar(stat = "identity", col = "black") + 
        facet_grid(.~ Gender) + 
        ylab("Alcohol Consumption per Capita (Liters)") +
        xlab("Income") + 
        ggtitle("Alcohol Consumption by Country of Income and Gender in 2016") + 
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(),
              panel.background = element_rect(fill = "white"), 
              panel.grid.major.y = element_line("grey"),
              panel.grid.major.x = element_blank()) +
        scale_fill_brewer(palette="Spectral")


alcohol_consumption_by_income_by_gender <-
        ggplot(gen_alc_income4, aes(x = Income_Class, y = Female_per_Male, fill = Income_Class)) + 
        geom_bar(stat = "identity", col = "black") + 
        ylab("Relative Alcohol Consumption (Female per Male, %)") + 
        xlab("Income") + 
        ggtitle("Alcohol Consumption of Female Relative to Male by Income") +
        theme(axis.text.x = element_blank(), 
              axis.ticks.x = element_blank(),
              panel.background = element_rect(fill = "white"), 
              panel.grid.major.y = element_line("grey"),
              panel.grid.major.x = element_blank()) +
        scale_fill_hue(c=45, l=80)

# plots for alcohol consumption by country 
alcohol_consumption_female_vs_male <-
        ggplot(gen_alc_country3, aes(x = Female, y = Male)) + 
        geom_point(alpha = 0.5, size = 2, col = "black") +
        geom_smooth(se = FALSE, method = "lm", col = "red") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        xlab("Female (Liters per Capita)") + 
        ylab("Male (Liters per Capita)") + 
        ggtitle("Gender Comparison on Alcohol Consumption in 187 Countries (2016)")

# R^2 = 0.96
gender_cor <- cor(gen_alc_country3$Female, gen_alc_country3$Male)
             