library(tidyverse)
library(lme4)
library(car)
library(gghalves)
library(effects)
library(emmeans)

rm(list=ls())
rice <- read_csv("rice.csv")
rice$AMF <- factor(rice$AMF)
rice$NIT <- factor(rice$NIT, levels=c(0,1), labels=c("low","high"))
rice$PHOS <- factor(rice$PHOS, levels=c(0,1), labels=c("low","high"))
rice$POT <- factor(rice$POT)

m1 <- lm(MASS ~ AMF * NIT * PHOS, data=rice)
# summary(m1)
Anova(m1)

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

m2 <- lm(MASS ~ AMF + NIT + PHOS + AMF:NIT + AMF:PHOS + NIT:PHOS, data=rice)
anova(m1, m2) # no sig diff

par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

m3 <- lm(MASS ~ AMF + NIT + PHOS + AMF:NIT + NIT:PHOS, data=rice)
anova(m2, m3) # no sig diff

par(mfrow=c(2,2))
plot(m3)
par(mfrow=c(1,1))

summary(m3)
Anova(m3)

# raw data
ggplot(data=rice, mapping=aes(y=MASS, x=AMF, col=AMF)) +
  geom_boxplot(width=0.3, position=position_nudge(0.25)) +
  geom_half_point(side='l', alpha=0.5,) +
  facet_grid(NIT ~ PHOS, labeller=label_both) +
  theme_bw() +
  labs(x="AMF Treatment", y="Average MASS of seed yield / kg")

# plot model
# pred <- rice[,c("AMF", "NIT", "PHOS")]
# predOut <- predict(m3, newdata=pred, interval="confidence")
# pred <- cbind(pred, predOut)
# wrong! only use this for continous data

m3Pred <- emmeans(m3, ~ AMF + NIT + PHOS + AMF:NIT)
m3Pred <- as_tibble(m3Pred)

# m3Pred %>% 
#   unite("COMBI", c(NIT, PHOS))

plot(allEffects(m3), multiline=TRUE, ci.style="bars")


m3Pred$COMBI <- with(m3Pred, paste("NIT: ", NIT, ", PHOS: ", PHOS, sep=""))
m3Pred$COMBI2 <- with(m3Pred, paste("NIT: ", NIT, ", \nPHOS: ", PHOS, sep=""))
# m3Pred$COMBI <- with(m3Pred, paste(AMF, " ", NIT, "NIT, ", PHOS, "PHOS", sep=""))

rice$COMBI <- with(rice, paste("NIT: ", NIT, ", PHOS: ", PHOS, sep=""))
rice$COMBI2 <- with(rice, paste("NIT: ", NIT, ", \nPHOS: ", PHOS, sep=""))


ggplot(data=m3Pred) +
  geom_point(
    mapping=aes(y=emmean, x=AMF, col=COMBI),
    position=position_dodge(width=0.6),
    alpha=0.6
  ) +
  geom_errorbar(
    mapping=aes(ymin=lower.CL, ymax=upper.CL, x=AMF, col=COMBI),
    position=position_dodge(width=0.6),
    width=0.3, alpha=0.6
  ) +
  geom_point(data=rice, 
     mapping=aes(y=MASS, x=AMF, col=COMBI),
     position=position_jitterdodge(dodge.width=0.6, jitter.width=0.2),
     size=0.5, alpha=0.3) +
  theme_bw() +
  labs(
    x = "AMF Treatment",
    y = "Average MASS of seed yield / kg"
  ) +
  scale_color_discrete(name="Nutrient Combination") +
  theme(
    legend.text=element_text(size=7)
  )



# ggplot(data=m3Pred) +
#   geom_point(
#     mapping=aes(y=emmean, x=AMF, col=COMBI),
#     position=position_dodge(width=0.5),
#     alpha=0.6
#   ) +
#   geom_errorbar(
#     mapping=aes(ymin=lower.CL, ymax=upper.CL, x=AMF, col=COMBI),
#     position=position_dodge(width=0.5),
#     width=0.3, alpha=0.6
#   ) +
#   geom_jitter(data=rice, 
#              mapping=aes(y=MASS, x=AMF, col=COMBI),
#              size=0.5, alpha=0.3) +
#   facet_grid(NIT ~ PHOS) +
#   theme_bw() +
#   labs(
#     x = "AMF Treatment",
#     y = "Average MASS of seed yield / kg"
#   ) +
#   scale_color_discrete(name="Nutrient Combination") +
#   theme(
#     legend.text=element_text(size=7)
#   )



ggplot(data=m3Pred) +
  geom_point(
    mapping=aes(y=emmean, x=COMBI2, col=AMF),
    position=position_dodge(width=0.5),
    alpha=0.6
  ) +
  geom_errorbar(
    mapping=aes(ymin=lower.CL, ymax=upper.CL, x=COMBI2, col=AMF),
    position=position_dodge(width=0.5),
    width=0.3, alpha=0.6
  ) +
  geom_point(data=rice, 
    mapping=aes(y=MASS, x=COMBI2, col=AMF),
    position=position_jitterdodge(jitter.width=0.2, dodge.width=0.5),
    size=0.5, alpha=0.3) +
  theme_bw() +
  labs(
    x = "Combination of NIT & PHOS treatment",
    y = "Average MASS of seed yield / kg"
  ) +
  scale_color_discrete(name="AMF Treatment") +
  theme(
    legend.title=element_text(size=9),
    legend.text=element_text(size=7),
    axis.text.x=element_text(angle=90)
  )
