library(ggplot2)
library(reshape2)
library(readxl)
library(tidyverse)

# ALL TAXA

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

ggplot(long_all_taxa, aes(fill=Taxa, y=Counts, x=Date)) + 
  geom_bar(position="fill", stat="identity", colour="black") +  
  # angling the text 
  theme(axis.text = element_text(angle = -45)) +
  ggtitle("Stacked Bar Plot All Phyla") + ylab("Percent of total")

# MOST REPRESENTED CLASSES

rep_classes <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                 sheet = "Sheet7", col_types = c("date", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric"))
rep_classes <- rep_classes %>% mutate(Date = as.character(Date))
long_rep_classes <- rep_classes %>%
  pivot_longer(cols = -Date, 
               names_to = "Taxa", 
               values_to = "Counts")

ggplot(long_rep_classes, aes(fill=Taxa, y=Counts, x=Date)) + 
  geom_bar(position="fill", stat="identity", colour="black") +  
  # angling the text 
  theme(axis.text = element_text(angle = -45)) +
  ggtitle("Stacked Bar Plot Most Represented Classes") + ylab("Percent of total")

# Low Represented Phyla

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

ggplot(long_low_rep, aes(fill=Taxa, y=Counts, x=Date)) + 
  geom_bar(position="fill", stat="identity", colour="black") +  
  # angling the text 
  theme(axis.text = element_text(angle = -45)) +
  ggtitle("Stacked Bar Plot Least Represented Classes") + ylab("Percent of total")
