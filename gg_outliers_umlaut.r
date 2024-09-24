library(ggstatsplot)

# to remove violin plot
#violin.args = list(width = 0)

#where are the outliers???

ggbetweenstats(
  data = dat_umlaut,
  x = ex1_umlaut,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
  outlier.color = "blue",
  messages = FALSE
)

OutVals_ex1_umlaut = boxplot(dat_umlaut$Rating_in_relation_to_mean~dat_umlaut$ex1_umlaut)$out

no_umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$ex1_umlaut == 'no umlaut', select = c(Rating_in_relation_to_mean, question, variable))
summary(no_umlaut_sub_dat)

umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$ex1_umlaut == 'umlaut',
                         select = c(Rating_in_relation_to_mean, number_of_umlaut, question, variable))
summary(umlaut_sub_dat)


ggbetweenstats(
  data = dat_umlaut,
  x = frequency,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
)

OutVals_frequency = boxplot(dat_umlaut$Rating_in_relation_to_mean~dat_umlaut$frequency)$out


complex_umlaut_sub_dat <- subset(dat_umlaut, frequency == 'complex', select = c(Rating_in_relation_to_mean, ex1_umlaut, number_of_umlaut, ex1_order, question, variable))
summary(complex_umlaut_sub_dat)

simple_umlaut_sub_dat <- subset(dat_umlaut, frequency == 'simple', select = c(Rating_in_relation_to_mean, ex1_umlaut, number_of_umlaut, ex1_order, question, variable))
summary(simple_umlaut_sub_dat)

ggbetweenstats(
  data = umlaut_sub_dat,
  x = number_of_umlaut,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
)

OutVals_number = boxplot(umlaut_sub_dat$Rating_in_relation_to_mean~umlaut_sub_dat$number_of_umlaut)$out

#OutVals = boxplot(dat$Rating_in_relation_to_mean~dat$matching)$out
#rows <- which(dat$Rating_in_relation_to_mean %in% OutVals)
#outliers <- dat[rows, ]
#write.csv(outliers, '/home/stefaniemueller/Dokumente/IGRA/suspended_affixation/experiment_evaluation/outliers.csv')


ein_umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$number_of_umlaut == 'ein_umlaut',
                            select = c(Rating_in_relation_to_mean, ex1_order, question, variable))

summary(ein_umlaut_sub_dat$Rating_in_relation_to_mean)



zwei_umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$number_of_umlaut == 'zwei_umlaut', select = c(Rating_in_relation_to_mean, ex1_order, question, variable))
summary(zwei_umlaut_sub_dat)

ggbetweenstats(
  data = ein_umlaut_sub_dat,
  x = ex1_order,
  y = Rating_in_relation_to_mean,
  plot.type = "box",
  results.subtitle = FALSE,
  violin.args = list(width = 0),
  outlier.tagging = TRUE,
  outlier.label = question,
  outlier.coef = 1.5,
)

OutVals_order = boxplot(ein_umlaut_sub_dat$Rating_in_relation_to_mean~ein_umlaut_sub_dat$ex1_order)$out


first_umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$ex1_order == 'umlaut first', select = c(Rating_in_relation_to_mean, question, variable))

summary(first_umlaut_sub_dat)

second_umlaut_sub_dat <- subset(dat_umlaut, dat_umlaut$ex1_order == 'umlaut second', select = c(Rating_in_relation_to_mean, question, variable))
summary(second_umlaut_sub_dat)