#Lab 10: ANOVA (by hand)
require(here)

#Code Template
rm(list = ls())

rope = read.csv(here("data", "rope.csv"))

rope$rope.type = factor(x=rope$rope.type)
levels(rope$rope.type)
      
n_obs = length(rope$rope.type)
n_groups = length(levels(rope$rope.type))
  
#To find total sum of squares           
mean_cut=mean(rope$p.cut)
res_cut=mean_cut-rope$p.cut

ss_tot = sum(res_cut^2)
    
df_tot = n_obs-1


agg_resids=aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x){x-mean(x)})

str(agg_resids)

agg_sq_resids=aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x){sum((x-mean(x))^2)})

str(agg_sq_resids)
                        
ss_within = sum(agg_sq_resids$x)
df_within = 115
          
ss_among = ss_tot - ss_within
df_among = n_groups-1
                       

ms_among  = ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)
                        
f_ratio = ms_among/ms_within
f_pval = 1-pf(f_ratio, df_among, df_within)
pf(f_ratio, df_among, df_within, lower.tail = FALSE)

#ANOVA
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$Sum Sq
anova_fit_1$"Sum Sq"

#Lab Questions 
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)



##Lab Questions
#Q3
bartlett.test(p.cut ~ rope.type, data=rope)

#Q5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#Q7
aggregate(p.cut ~ rope.type, data=rope, FUN=mean)

0.36714 +(-0.10164)


#Week 12 Reading Q
	-1.7+0*0.043+0*0.192+0*-0.027 
-1.7+10*0.043+30*0.192+20*-0.027 

require(palmerpenguins)
summary(penguins)
head(penguins)
