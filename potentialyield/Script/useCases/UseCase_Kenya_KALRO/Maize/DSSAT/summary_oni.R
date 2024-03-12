library(tidyverse)
library(dplyr)
library(purrr)
#### Read ONI index #####
oni <- read_table("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/Landing/DSSAT/oni.txt")

results <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/result/DSSAT/AOI/useCase_Kenya_KALRO_Maize_AOI_season_11.RDS")

month_mapping <- c(
  'DJF'= 'January',
  'JFM'= 'February',
  'FMA'= 'March',
  'MAM'= 'April',
  'AMJ'= 'May',
  'MJJ'= 'June',
  'JJA'= 'July',
  'JAS'= 'August',
  'ASO'= 'September',
  'SON'= 'October',
  'OND'= 'November',
  'NDJ'= 'December'
 )
oni$month <- month_mapping[oni$SEAS]
oni$date <- as.Date(paste0(oni$YR, "-", oni$month, "-01"), format="%Y-%B-%d")



get_average_variable <- function(initial_date, final_date, variable, data) {
  filtered_data <- data %>%
    filter(date >= as.Date(initial_date) & date <= as.Date(final_date))
  
  avg_variable <- median(filtered_data[[variable]], na.rm = TRUE)
  
  return(data.frame(initial_date = initial_date, final_date = final_date, avg_variable = avg_variable))
}

date_ranges <- data.frame (
  initial_date = results$PDAT, 
    final_date = results$HDAT
)

anom_result <- pmap_dfr(date_ranges, get_average_variable, variable = "ANOM", data = oni)

results_oni <-bind_cols(results,anom_result)

results_oni$ENSO <- ifelse(results_oni$avg_variable > 0.5,"Niño",
                            ifelse(results_oni$avg_variable< -0.5,"Niña", "Neutral"))

saveRDS(results_oni,"~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/result/DSSAT/AOI/useCase_Kenya_KALRO_Maize_AOI_season_11_ONI.RDS")

results_oni <- readRDS("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_Kenya_KALRO/Maize/result/DSSAT/AOI/useCase_Kenya_KALRO_Maize_AOI_season_11_ONI.RDS")

results_oni  <- na.omit(results_oni) 

results_oni %>% group_by(variety,TNAM,ENSO) %>%
  ggplot(aes(x=TNAM, y=HWAH,color=ENSO)) +
  geom_boxplot(size=1.5)+ facet_wrap(~variety)+
  scale_colour_brewer(palette = "Dark2")+
  theme_bw()+ 
  theme(legend.position = "bottom",
        axis.text=element_text(size=14, colour="black", angle=45),
        axis.title = element_text(face="bold", size=18),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=16),
        panel.grid = element_blank(),
        strip.text = element_text(colour = "black", face="bold", size = 14, vjust=0.1),
        strip.background = element_blank()) +
  ylab(expression(bold('Yield'~(kg~ha^{-1})))) + xlab("PLanting time")

head(results_oni)
