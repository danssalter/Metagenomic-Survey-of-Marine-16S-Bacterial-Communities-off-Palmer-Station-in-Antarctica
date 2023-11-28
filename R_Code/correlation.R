library(openxlsx)
write.xlsx(CTD_crop,"/Users/dsalter/Documents/Antarctica_Project/Result_Table/CTD_crop.xlsx")

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}


# ALL TAXA

dat <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                  sheet = "Sheet14", col_types = c( "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))

#insig

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 1,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


#sig = 0.05

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# MOST REPRESENTED


dat <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                  sheet = "Sheet15", col_types = c( "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric"))

#insig

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 1,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


#sig = 0.05

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


# LEAST REPRESENTED


dat <- read_excel("Documents/Antarctica_Project/Result_Table/SummaryTableExcel_edited.xlsx", 
                  sheet = "Sheet16", col_types = c( "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))

#insig

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 1,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)


#sig = 0.05

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)
