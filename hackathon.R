  library(tidyverse)
  library(readxl)
  library(ggpubr)


#------------------------------ 
# Loading the necessary data
# and making into tidy datasets
#------------------------------
  
  #Johns Hopkins
  jh_covid <- as_tibble(read.csv("CSVs/JohnsHopkinsCOVIDDataSept30.csv"))
  
  #Election Results
  e_results <- as_tibble(read.csv("CSVs/Combined20162020ElectionResults.csv"))
  e_results$won <- with(e_results, ifelse(e_results$dem.perc20 > 50.00, 1, 2))
  results20 <- select(e_results, c(2,3,5))
  
  #ACS 5 year survey
  acs <- as_tibble(read.csv("CSVs/ACS5Year2014-2019.csv"))
  
  #Survey Monkey
  smData <- as_tibble(read.csv("CSVs/SMData.csv"))
  
  #isolating data of interest
  personal <- smData %>% select(county, 
                                state, 
                                SMDate, 
                                wear_mask_how_often, 
                                news_mc_fox, 
                                news_mc_cnn,
                                election_fair,
                                describe_personal_economic_situation,
                                practing_social_distancing)
 
  total_county <- count(personal, county) 
  
  #separating values based on percent self described economic status
  selfEcon <- personal %>%
    group_by(county,
             'self_report' = describe_personal_economic_situation) %>% 
    summarize("selfReport" = n())
  selfEcon <- left_join(selfEcon, total_county) %>% 
    mutate(percent = round((selfReport/n)*100,1))
  #SV Note: Decided not to look into this analysis as the time I had to do this
  #would not be sufficient for a proper deep dive
  
  #separating values based on percent wears masks Never - Every Time
  wearMask <- personal %>% 
    group_by(county,
             'mask' = wear_mask_how_often,
             'psd' = practing_social_distancing) %>% 
    summarize("wears" = n()) 
  
  #adding total population by county to mask wearing data from survey monkey
  wearMask <- left_join(wearMask, total_county) %>% 
    mutate(percent = round((wears/n)*100,1))
  
  x <- group_by(wearMask, mask, percent) %>% 
    filter(percent == max(percent))

#----------------------------------------------------
# Starting analysis of the data
# focusing on whether we see a correlation between low mask 
# wearing but 
#----------------------------------------------------
  #Calculating covid cases per 1000 people
  covidCases <- jh_covid %>% 
    left_join(acs, by = 'county') %>% 
    select(c(2,4,5,7,11,16,17,18,19),) %>% 
    mutate(casePer1000 = round((cases*1000)/population,1)) %>% 
    select(c(1,10)) 
  
  
  # wearMask <- left_join(wearMask, covidCases, by = 'county') %>% 
  #   left_join(results20, by = 'county')
 
  #adding factors for levels of likert scale
  wearMask$mask <- factor(wearMask$mask, 
                       levels = c("Every time",
                                  "Most of the time",
                                  "Some of the time",
                                  "Never",
                                  "No answer"))
  wearMask$psd <- factor(wearMask$psd, 
                          levels = c("Very much",
                                     "Somewhat",
                                     "Not at all",
                                     "No answer"))
  
  #including social distancing data by county for those who socially distance a lot
  socialDistance <- wearMask %>% filter(psd == 'Very much' & mask != "No answer")
  
  #filtering to isolate those who wear masks every or most of the time
  wearMask <- wearMask %>% filter(
    (mask == "Every time" | mask == "Most of the time") & 
      is.na(psd) == FALSE &
      psd != "No answer")
  

  #-------------------------------------------
  # Making the maps using ggplot and exporting
  # onto a pdf
  #-------------------------------------------
   
  analysis1 <- ggplot(mapping = aes(x = percent, y = casePer1000)) +
    geom_jitter(socialDistance,
                mapping = aes(percent,casePer1000, color = factor(mask))) + 
    stat_smooth(socialDistance, mapping = aes(percent,casePer1000), 
                method = "lm", fullrange = TRUE, se = FALSE) +
   facet_wrap(~mask) +
    xlab('Percent of total respondents who social distance "Very Much who wear a mask:') + 
    ylab('Total COVID cases per 1000 people') + theme_wsj() +
    labs(color = NULL)
  
  analysis2 <- ggplot(mapping = aes(x = percent, y = casePer1000)) + 
    geom_jitter(wearMask,
                mapping = aes(percent,casePer1000, color = factor(psd))) + 
    stat_smooth(wearMask, mapping = aes(percent,casePer1000), 
                method = "lm", fullrange = TRUE, se = FALSE) +  
    facet_wrap(~psd, nrow = 2) +
    xlab('Percent of total respondents who social distance "Very Much who wear a mask:') + 
    ylab('Total COVID cases per 1000 people') + theme_wsj()+
    labs(color = NULL)
  
  analysis <- ggarrange(
    analysis1, analysis2, labels = c("High social distancing with variable mask wearing",
                                     "High mask wearing with variable social distancing")
  )
  
  analysis
  
  ggexport(analysis, 'hackathon_viz.pdf', width = 20, height = 8)
  


