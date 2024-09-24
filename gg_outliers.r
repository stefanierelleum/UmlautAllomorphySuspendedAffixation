library(ggstatsplot)

ggbetweenstats(
  data = dat,
  x = matching,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
  messages = FALSE
)

OutVals_matching = boxplot(dat$Rating_in_relation_to_mean~dat$matching)$out


ggbetweenstats(
  data = dat,
  x = frequency,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
  messages = FALSE
)

OutVals_frequency = boxplot(dat$Rating_in_relation_to_mean~dat$frequency)$out

match_sub_dat <- subset(dat, dat$matching == 'match',
                        select = c(Rating_in_relation_to_mean, ex2_allomorph, question, variable))

ggbetweenstats(
  data = match_sub_dat,
  x = ex2_allomorph,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
  messages = FALSE
)

OutVals_match = boxplot(match_sub_dat$Rating_in_relation_to_mean~match_sub_dat$ex2_allomorph)$out

mis_match_sub_dat <- subset(dat, dat$matching == 'mismatch',
                        select = c(Rating_in_relation_to_mean, ex2_order, question, variable))

ggbetweenstats(
  data = mis_match_sub_dat,
  x = ex2_order,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
  messages = FALSE
)

OutVals_mismatch = boxplot(mis_match_sub_dat$Rating_in_relation_to_mean~mis_match_sub_dat$ex2_order)$out



#Q <- quantile(match_sub_dat$Rating_in_relation_to_mean, probs=c(.25, .75), na.rm = FALSE)
#iqr <- IQR(match_sub_dat$Rating_in_relation_to_mean)

#up <-  Q[2]+1.5*iqr # Upper Range  
#low<- Q[1]-1.5*iqr # Lower Range

#eliminated<- subset(match_sub_dat, match_sub_dat$Rating_in_relation_to_mean > (Q[1] - 1.5*iqr) & match_sub_dat$Rating_in_relation_to_mean < (Q[2]+1.5*iqr))

#ggbetweenstats(eliminated, condition, Rating_in_relation_to_mean, outlier.tagging = TRUE, outlier.label = question)

#^eliminates nothing...

#plot with mean instead of median
#plot <- ggplot(data = match_sub_dat, aes(y = Rating_in_relation_to_mean, x = condition)) + 
#geom_boxplot(aes(middle = mean(Rating_in_relation_to_mean)))

#plot

#ggbetweenstats(match_sub_dat, condition, Rating_in_relation_to_mean, outlier.tagging = TRUE,  outlier.label = question)