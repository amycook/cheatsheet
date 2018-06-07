## lme4

library('lme4')
library('lattice')

example <- read.table("C:/Users/cook79166/OneDrive/shared files/Statslearningcourse/linear_mixed_effects/example.txt",
                 header = T)

#first try plain anova
linear<-lm(SCIENCE~URBAN, example)
summary(linear)
#yˆ= −1.25+ 0.83(urban)+ r
plot(SCIENCE~URBAN, example)
abline(linear)
plot(linear)
names(linear)
linear$coefficients

# now fit generalised linear mixed effects model

fm.null <- lmer(SCIENCE~ 1 + (1|GROUP), example)
summary(fm.null)
# y = 10.688 + N(0, 25.531)*GROUP + r 
# y = 10.688 + u_oj + r 

# plot residuals for each group?
bwplot(GROUP~resid(fm.null), example)

#compare group means to group intercepts
data.frame("mean" = with(example, tapply(SCIENCE, GROUP, mean)),
           "coefs" = coef(fm.null)[[1]] %>% .$'(Intercept)' %>% round(2))

# add the 'urban' predictor
fm1<-lmer(SCIENCE ~ URBAN + (1|GROUP), example)
summary(fm1)
# URBAN instructs A single global estimate for the effect (slope) of URBAN
# (1|GROUP) instructs Random effect intercepts for GROUP (i.e. for each level of GROUP, that level's 
#           intercept's deviation from the global intercept)
coef(fm1)
# intercepts for each group
# constant global slope for URBAN (fixed effect)

# compare models
anova(fm.null, fm1)
r.squaredGLMM(fm.null)
r.squaredGLMM(fm1)

# graph data and slopes

lines = data.frame("GROUP" = rownames(coef(fm1)$GROUP),
                   "Intercept" = coef(fm1)$GROUP$'(Intercept)',
                   "URBAN" = coef(fm1)$GROUP$'URBAN')

ggplot(example, aes(x = URBAN, y = SCIENCE)) + 
        geom_point() +
        facet_wrap(~GROUP) +
        geom_abline(data = lines,
                    aes(intercept = Intercept, slope = URBAN))

# now model URBAN slope for each GROUP
fm2<-lmer(SCIENCE ~ URBAN + (URBAN|GROUP), example)
# additionally estimates The effect of URBAN within each level of GROUP (more specifically, 
# the degree to which the URBAN effect within a given level deviates from the global effect of URBAN), 
# while enforcing a zero correlation between the intercept deviations and URBAN effect deviations across 
# levels of GROUP.
summary(fm2)
coef(fm2)  
r.squaredGLMM(fm2)
anova(fm2, fm1)
# looks like it has automatically found coefficients for GROUP.

#plot
lines = data.frame("GROUP" = rownames(coef(fm2)$GROUP),
                   "Intercept" = coef(fm2)$GROUP$'(Intercept)',
                   "URBAN" = coef(fm2)$GROUP$'URBAN')

ggplot(example, aes(x = URBAN, y = SCIENCE)) + 
        geom_point() +
        facet_wrap(~GROUP) +
        geom_abline(data = lines,
                    aes(intercept = Intercept, slope = URBAN))
anova(fm.null, fm1, fm2)
qqmath(~resid(fm2))
xyplot(resid(fm2) ~ fitted(fm2))
abline(h = 0)
xyplot(resid(fm2) ~ fitted(fm2))



