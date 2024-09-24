library(languageR)
library(lme4)

dat$condition <- gsub("'", '', dat$condition) 

lm1 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency * 
              #predict the rating on the basis of frequency plus its interaction with
              dat$matching + dat$matching:dat$ex2_allomorph 
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item
#linear regression intercept with the axis is a probability distribution
#predicting the Rating_ ~ perdictor/independent variables
#* star sign computes interaction + plus sign doesn't
# : variable on the right is nested indside the one on the left

#summary(lm1) #will describe the model to you

lm2 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency + 
              #predict the rating on the basis of frequency plus 
              dat$matching + dat$matching:dat$ex2_allomorph 
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item

anova(lm1,lm2) #compare the models to see if the interaction was significant

#the difference between model 3 and 4 is the presence/absence of 'dat$matching' so we investigate the effect of matching?

lm3 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency + dat$matching + (1|dat$variable) + (1|dat$condition))

lm4 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency + (1|dat$variable) + (1|dat$condition))

anova(lm4, lm3)

#the difference between model 5 and 6 is the presence/absence of 'dat$frequency' so we investigate the effect of frequency?

lm5 <- lmer(dat$Rating_in_relation_to_mean ~ dat$matching + (1|dat$variable) + (1|dat$condition))

lm6 <- lmer(dat$Rating_in_relation_to_mean ~ dat$matching + dat$frequency + (1|dat$variable) + (1|dat$condition))

anova(lm5,lm6)

lm7 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency +
              #predict the rating on the basis of frequency plus its interaction with
              dat$matching + dat$matching:dat$ex2_allomorph 
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item
#linear regression intercept with the axis is a probability distribution
#predicting the Rating_ ~ perdictor/independent variables
#* star sign computes interaction + plus sign doesn't
# : variable on the right is nested indside the one on the left

#summary(lm1) #will describe the model to you

lm8 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency + 
              #predict the rating on the basis of frequency plus 
              dat$matching
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item

anova(lm7,lm8)

lm9 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency +
              #predict the rating on the basis of frequency plus its interaction with
              dat$matching + dat$matching:dat$ex2_order 
            #form match/mismatch, and the interaction thereof with the allomorph used
            + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item
#linear regression intercept with the axis is a probability distribution
#predicting the Rating_ ~ perdictor/independent variables
#* star sign computes interaction + plus sign doesn't
# : variable on the right is nested indside the one on the left

#summary(lm1) #will describe the model to you

lm10 <- lmer(dat$Rating_in_relation_to_mean ~ dat$frequency + 
               #predict the rating on the basis of frequency plus 
               dat$matching
             #form match/mismatch, and the interaction thereof with the allomorph used
             + (1|dat$variable) + (1|dat$condition))
#including random corrections for participant and item

anova(lm9,lm10)


#summary(lm1)
