library(readxl)
library(ggplot2)
library(ggnewscale)
library(ggpubr)


##############################################
# ALL TAXA
##############################################


# HEAT MAP

CTD_data <- read_excel("Documents/Antarctica_Project/Result_Table/CTD_B_downcast_12-15_clean.xlsx", 
        col_types = c("date", "numeric", "numeric"))

sample_dates <- c("2012-11-27", "2013-02-08", "2013-12-27", 
                  "2014-01-23", "2014-02-03", "2014-02-10", 
                  "2014-02-28", "2014-03-04", "2014-12-01", 
                  "2014-12-11", "2015-01-12", "2015-01-19", 
                  "2015-02-09", "2015-02-23", "2015-03-09")

# For some reason the Dates are doubles so I convert to a Char 
CTD_data$Date <- as.character(CTD_data$Date)

# Filter rows out by dates
CTD_crop <- CTD_data %>% filter(CTD_data$Date %in% sample_dates)

long_CTD_crop <- CTD_crop %>%
  pivot_longer(cols = -Date, 
               names_to = "Environmental", 
               values_to = "Value")

heat_map <- ggplot() +
  geom_raster(data = filter(long_CTD_crop, Environmental == "Temperature..C"), aes(x = Date, y = Environmental, fill = Value)) +
  scale_fill_gradient2(low = "white", mid = "orangered2", high = "darkslategrey", midpoint = median(CTD_crop$Temperature..C), name = "Temperature...C.") +
  new_scale_fill() +
  
  geom_raster(data = filter(long_CTD_crop, Environmental == "Salinity..ppt"), aes(x = Date, y = Environmental, fill = Value)) +
  scale_fill_gradient2(low = "white", mid = "gold2", high = "darkslategrey", midpoint = median(CTD_crop$Salinity..ppt), name = "Salinity..ppt") +
  new_scale_fill() +
  
  theme(axis.text = element_text(angle = -45))

heat_map

# TIME COURSE

all_taxa <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
        sheet = "Sheet11", col_types = c("date", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"))

all_taxa <- all_taxa %>% mutate(Date = as.character(Date))
long_all_taxa <- all_taxa %>%
        pivot_longer(cols = -Date, 
        names_to = "Taxa", 
        values_to = "Counts")

long_all_taxa_log <- long_all_taxa

long_all_taxa_log$Counts <- log10(long_all_taxa_log$Counts) 

time_course_all_taxa<- ggplot(long_all_taxa_log, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course All Taxa and Heat Map (Log)") + ylab("Log of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 

# NO LOG

time_course_all_taxa<- ggplot(long_all_taxa, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course All Taxa and Heat Map (Percent)") + ylab("Percent of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 

# Combining all plots

ggarrange(time_course_all_taxa, heat_map, heights = c(2, 1), ncol = 1, nrow = 2, align = "hv", hjust = 10)+
  theme(plot.margin = margin(0,0,50,0, "pt")) 


##############################################
# Most rep classes
##############################################

rep_classes <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                       sheet = "Sheet7", col_types = c("date", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric"))

rep_classes <- rep_classes %>% mutate(Date = as.character(Date))
long_rep_classes <- rep_classes %>%
  pivot_longer(cols = -Date, 
               names_to = "Taxa", 
               values_to = "Counts")

long_rep_classes_log <- long_rep_classes

long_rep_classes_log$Counts <- log10(long_rep_classes_log$Counts) 

time_course_most_rep<- ggplot(long_rep_classes_log, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course Most Represented Classes and Heat Map (Log)") + ylab("Log of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 

# NO LOG

time_course_most_rep<- ggplot(long_rep_classes, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course Most Represented Classes and Heat Map (Percent)") + ylab("Percent of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 


# Combining all plots

ggarrange(time_course_most_rep, heat_map, heights = c(2, 1), ncol = 1, nrow = 2, align = "hv", hjust = 10)+
  theme(plot.margin = margin(0,0,50,0, "pt"))

##############################################
# Low Represented Classes
##############################################

low_rep <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                          sheet = "Sheet12", col_types = c("date", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric",
                                                          "numeric"))

low_rep <- low_rep %>% mutate(Date = as.character(Date))
long_low_rep <- low_rep %>%
        pivot_longer(cols = -Date, 
        names_to = "Taxa", 
        values_to = "Counts")

long_low_rep_log <- long_low_rep

long_low_rep_log$Counts <- log10(long_low_rep_log$Counts) 

time_course_low_rep<- ggplot(long_low_rep_log, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course Least Represented Classes and Heat Map (Log)") + ylab("Log of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 

# NO LOG

time_course_low_rep<- ggplot(long_low_rep, aes(x=Date, y=Counts, group=Taxa, color=Taxa)) +
  geom_line(linewidth=1.4) +
  ggtitle("Time Course Least Represented Classes and Heat Map (Percent)") + ylab("Percent of Read Counts") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank()) 

# Combining all plots

ggarrange(time_course_low_rep, heat_map, heights = c(2, 1), ncol = 1, nrow = 2, align = "hv", hjust = 10)+
  theme(plot.margin = margin(0,0,50,0, "pt"))
