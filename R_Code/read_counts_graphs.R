library(ggplot2)
library(xlsx)

read_counts <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                          sheet = "Sheet3", col_types = c("date", 
                          "numeric", "numeric", "numeric"))

df <- read_counts

df$Date <- as.Date(df$Date)

options(scipen = 999)

# Line Plot

ggplot(data = read_counts, aes(x = Date, y = TOTAL)) + geom_line()

# Bar Plot

ggplot(data=df, aes(x=Date, y=TOTAL)) +
  geom_bar(stat="identity", fill="steelblue") + ylab("Reads") +
  theme_minimal() + ggtitle("Total Read Counts per Sample") 

