library(tidyverse)
library(ggplot2)
h <- head
s <- summary  
g <- glimpse  
t <- tail

gen <- read.csv("Gender_StatsData.csv", stringsAsFactors = FALSE)
indicator <- unique(gen$Indicator.Name)
country <- unique(gen$?..Country.Name)
gen1 <- gen %>% 
        rename(Country = ?..Country.Name)


topics_education <- c(
        "GDP (current US$)",
        "GDP per capita (Current US$)",
        "Government expenditure on education, total (% of GDP)",
        "Children out of school, primary, female",
        "Children out of school, primary, male",
        "Literacy rate, adult female (% of females ages 15 and above)",                                                                                 
        "Literacy rate, adult male (% of males ages 15 and above)",
        "Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)",
        "Educational attainment, at least Bachelor's or equivalent, population 25+, male (%) (cumulative)",                                             
        "Educational attainment, at least completed lower secondary, population 25+, female (%) (cumulative)",                                          
        "Educational attainment, at least completed lower secondary, population 25+, male (%) (cumulative)",                                            
        "Educational attainment, at least completed post-secondary, population 25+, female (%) (cumulative)",                                           
        "Educational attainment, at least completed post-secondary, population 25+, male (%) (cumulative)",                                             
        "Educational attainment, at least completed primary, population 25+ years, female (%) (cumulative)",                                            
        "Educational attainment, at least completed primary, population 25+ years, male (%) (cumulative)",                                              
        "Educational attainment, at least completed short-cycle tertiary, population 25+, female (%) (cumulative)",                                     
        "Educational attainment, at least completed short-cycle tertiary, population 25+, male (%) (cumulative)",                                       
        "Educational attainment, at least completed upper secondary, population 25+, female (%) (cumulative)",                                          
        "Educational attainment, at least completed upper secondary, population 25+, male (%) (cumulative)",                                            
        "Educational attainment, at least Master's or equivalent, population 25+, female (%) (cumulative)",                                             
        "Educational attainment, at least Master's or equivalent, population 25+, male (%) (cumulative)",                                               
        "Educational attainment, Doctoral or equivalent, population 25+, female (%) (cumulative)",                                                      
        "Educational attainment, Doctoral or equivalent, population 25+, male (%) (cumulative)")

topics_employment <- c(
        "GDP (current US$)",
        "GDP per capita (Current US$)",
        "Employment in agriculture, female (% of female employment) (modeled ILO estimate)",                                                            
        "Employment in agriculture, male (% of male employment) (modeled ILO estimate)",                                                                
        "Employment in industry, female (% of female employment) (modeled ILO estimate)",                                                               
        "Employment in industry, male (% of male employment) (modeled ILO estimate)",                                                                   
        "Employment in services, female (% of female employment) (modeled ILO estimate)",                                                               
        "Employment in services, male (% of male employment) (modeled ILO estimate)",
        "Proportion of seats held by women in national parliaments (%)",
        "Labor force with advanced education, female (% of female working-age population with advanced education)",                                     
        "Labor force with advanced education, male (% of male working-age population with advanced education)",                                         
        "Labor force with basic education, female (% of female working-age population with basic education)",                                           
        "Labor force with basic education, male (% of male working-age population with basic education)",                                               
        "Labor force with intermediate education, female (% of female working-age population with intermediate education)",                             
        "Labor force with intermediate education, male (% of male working-age population with intermediate education)",
        "Labor force, female",                                                                                                                          
        "Labor force, female (% of total labor force)",                                                                                                 
        "Labor force, total", 
        "Unemployment with advanced education, female (% of female labor force with advanced education)",                                               
        "Unemployment with advanced education, male (% of male labor force with advanced education)",                                                   
        "Unemployment with basic education, female (% of female labor force with basic education)",                                                     
        "Unemployment with basic education, male (% of male labor force with basic education)",                                                         
        "Unemployment with intermediate education, female (% of female labor force with intermediate education)",                                       
        "Unemployment with intermediate education, male (% of male labor force with intermediate education)",                                           
        "Unemployment, female (% of female labor force) (modeled ILO estimate)",                                                                        
        "Unemployment, female (% of female labor force) (national estimate)",                                                                           
        "Unemployment, male (% of male labor force) (modeled ILO estimate)",                                                                            
        "Unemployment, male (% of male labor force) (national estimate)", 
        "Vulnerable employment, female (% of female employment) (modeled ILO estimate)",                                                                
        "Vulnerable employment, male (% of male employment) (modeled ILO estimate)",  
        "Length of paid maternity leave (days)",
        "Maternity leave benefits (% of wages paid)",
        "Mothers are guaranteed an equivalent position after maternity leave (1=yes; 0=no)")

topics_life <- c("GDP (current US$)",
                 "GDP per capita (Current US$)",
                 "Age at first marriage, female", 
                 "Age at first marriage, male",
                 "Birth rate, crude (per 1,000 people)",
                 "Death rate, crude (per 1,000 people)",
                 "Completeness of birth registration, female (%)",
                 "Completeness of birth registration, male (%)",
                 "Suicide mortality rate, female (per 100,000 female population)",
                 "Suicide mortality rate, male (per 100,000 male population)",
                 "Fertility rate, total (births per woman)",
                 "Wanted fertility rate (births per woman)" ,
                 "Sex ratio at birth (male births per female births)",
                 "Proportion of time spent on unpaid domestic and care work, female (% of 24 hour day)",                                                         
                 "Proportion of time spent on unpaid domestic and care work, male (% of 24 hour day)", 
                 "Total alcohol consumption per capita, female (liters of pure alcohol, projected estimates, female 15+ years of age)",
                 "Total alcohol consumption per capita, male (liters of pure alcohol, projected estimates, male 15+ years of age)"
)