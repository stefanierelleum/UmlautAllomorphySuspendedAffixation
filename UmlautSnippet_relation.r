library(languageR)
library(lme4)

dat_umlaut$condition <- gsub("'", '', dat_umlaut$condition) 

lm1 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency * 
              #predict the rating on the basis of frequency plus its interaction with
              dat_umlaut$ex1_umlaut
            #whether or not at least one umlaut is in the conjunction
            + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))
#including random corrections for participant and item
#linear regression intercept with the axis is a probability distribution
#predicting the Rating_ ~ perdictor/independent variables
#* star sign computes interaction + plus sign doesn't
# : variable on the right is nested indside the one on the left

#summary(lm1) #will describe the model to you

lm2 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency + 
              #predict the rating on the basis of frequency plus 
              dat_umlaut$ex1_umlaut
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))
#including random corrections for participant and item

anova(lm1,lm2) #compare the models to see if the interaction was significant

#the difference between model 3 and 4 is the presence/absence of 'dat_umlaut$ex1_umlaut' so is there an effect of no_umlaut vs. umlaut?

lm3 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency + dat_umlaut$ex1_umlaut + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))

lm4 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))

anova(lm3, lm4) #compare the models to see if the effect of ex1_umlaut was significant

#the difference between model 5 and 6 is the presence/absence of 'dat_umlaut$frequency' so is there an effect of frequency?

lm5 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$ex1_umlaut + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))

lm6 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency + dat_umlaut$ex1_umlaut +  (1|dat_umlaut$variable) + (1|dat_umlaut$condition))

anova(lm5,lm6) #compare the models to see if the effect of frequency was significant

lm7 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency +
              #predict the rating on the basis of frequency plus
              dat_umlaut$ex1_umlaut + dat_umlaut$ex1_umlaut:dat_umlaut$number_of_umlaut
            #umlaut vs. no_umlaut, plus one or two umlauts
            + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))


lm8 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$frequency + 
              #predict the rating on the basis of frequency plus 
              dat_umlaut$ex1_umlaut
            #umlaut vs. no_umlaut
            + (1|dat_umlaut$variable) + (1|dat_umlaut$condition))
#including random corrections for participant and item

anova(lm7,lm8) #compare the models to see if the effect of the number_of_umlaut s was significant

#lm10 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ frequency * ex1_umlaut +
               #predict the rating on the basis of frequency plus
           #    (frequency | number_of_umlaut:ex1_order) + (frequency | number_of_umlaut), data=dat_umlaut)

#lm11 <- lmer(dat_umlaut$Rating_in_relation_to_mean ~ frequency * ex1_umlaut +
               #predict the rating on the basis of frequency plus
            #   (frequency | number_of_umlaut), data=dat_umlaut)

#summary(lm10)

#anova(lm10,lm11)


#summary(lm1)

am1 <- aov(dat_umlaut$Rating_in_relation_to_mean ~ dat_umlaut$ex1_umlaut / dat_umlaut$number_of_umlaut / dat_umlaut$ex1_order + Error(dat_umlaut$variable + dat_umlaut$condition))
summary(am1)

#collapsing number and order
dat_umlaut_um <- dat_umlaut
dat_umlaut_um$um1 <- 0
dat_umlaut_um$um1[dat_umlaut_um$number_of_umlaut == "zwei_umlaut"] <- 1
dat_umlaut_um$um1[dat_umlaut_um$ex1_order == "umlaut first"] <- 1
dat_umlaut_um$um2 <- 0
dat_umlaut_um$um2[dat_umlaut_um$ex1_order == "umlaut second"] <- 1
dat_umlaut_um$um2[dat_umlaut_um$number_of_umlaut == "zwei_umlaut"] <- 1

lmum2 <- lmer(Rating_in_relation_to_mean ~ um1 * um2 + (1 | variable) + (1 | condition), data = dat_umlaut_um)
lmum1 <- lmer(Rating_in_relation_to_mean ~ um1 + um2 + (1 | variable) + (1 | condition), data = dat_umlaut_um)
anova(lmum1,lmum2) #interction of um1 and um2
lmum3 <- lmer(Rating_in_relation_to_mean ~ um1 + (1 | variable) + (1 | condition), data = dat_umlaut_um)
anova(lmum1,lmum3) #effect of um2
lmum4 <- lmer(Rating_in_relation_to_mean ~ um2 + (1 | variable) + (1 | condition), data = dat_umlaut_um)
anova(lmum1,lmum4) #effect of um1

