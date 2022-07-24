install.packages("seminr")
require(seminr)
attach(data_for_semPLS)
View(data_for_semPLS)
# Define measurements with familiar terms: 
# reflective, multi-item constructs, etc.

#===========================================
#===========================================

measurements = constructs(
  reflective("Quality",multi_items("quality", 1:6)),
  reflective("Loyalty",multi_items("loyalty", 1:5)),
  reflective("Satisfaction",multi_items("sat", 1:4)),
  reflective("Intention",multi_items("beh_int", 1:6))
)
#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure = relationships(
  paths(from = "Quality", to = c("Satisfaction","Intention","Loyalty")),
  paths(from = "Satisfaction", to = c("Intention","Loyalty"))  )
#paths(from = c("Quality","Satisfaction"), to = c("Intention","Loyalty")),
plot(structure)

#=========================================================
#=========================================================

#[1] CFA Model- Exact same results of IBM AMOS CFA
?estimate_cfa
cfa_model <- estimate_cfa(data_for_semPLS,
                          measurements)

summary(cfa_model)
p1=print(summary(cfa_model),digits = 3)
p1
print(p1$model)
print(p1$descriptives,digits = 3)   #Descriptive
print(p1$loadings$coefficients,digits = 3)       #Factor Loading
print(p1$loadings$significance,digits = 3)
print(p1$quality,digits = 3)        #Model Fit Indicators
print(p1$quality$fit$all,digits = 3)

# Descriptive

cc=print(p1$descriptives$correlations$items,digits = 3)
cc
round(cc[1:6,1:6],2)
round(cc[7:11,7:11],2)
round(cc[12:15,12:15],2)
round(cc[16:21,16:21],2)
# Adding one pair of error terms

mi1=item_errors("quality1","quality3")
mi1
cfa_model1 <- estimate_cfa(data_for_semPLS,
                          measurements, item_associations =mi1)

p2=summary(cfa_model1)
p2
print(p2$quality$fit$all, digits = 3)

# Adding more than one pair of error terms
mi2=item_errors(c("quality1","quality2"),
                c("quality1","quality3"))
mi2
cfa_model2 <- estimate_cfa(data_for_semPLS,
                           measurements, item_associations =mi2)

p3=summary(cfa_model2)
p3
print(p3$quality$fit$all, digits = 3)