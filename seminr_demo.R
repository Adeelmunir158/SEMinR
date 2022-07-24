
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

plot(cfa_model, cex=3, col=5)

#===========================================
#===========================================

#[2] CB-SEM Model- Exact same results of IBM AMOS SEM
set.seed(12345)
cbsem_model <- estimate_cbsem(data_for_semPLS, 
                              measurements,
                              structure)

p2=print(summary(cbsem_model),digits = 3)
p2$paths
print(p2$quality,digits = 3)
print(p2$descriptives,digits = 3)
print(p2$loadings,digits = 3)
print(p2$paths,digits = 3)
p2$paths$coefficients

#===========================================
#===========================================

#[3] CONSISTENT PLS-SEM Model- Exact same results of SmartPLSc

pls_model=estimate_pls(data_for_semPLS, 
                       measurements,
                       structure,
                       inner_weights = path_weighting)
p3=summary(pls_model)




print(p3$descriptives,digits = 3)
print(p3$paths,digits = 3)
print(p3$total_effects,digits = 3)
print(p3$total_indirect_effects,digits = 3)
print(p3$loadings,digits = 3)
print(p3$reliability,digits = 3)

plot_scores(pls_model, )
b=bootstrap_model(pls_model, nboot = 500)
s=summary(b)
s
s$bootstrapped_total_paths

#===========================================
#===========================================

#[4] PLS-SEM Composite Model- Exact same results of SmartPLS

measurements_pls = constructs(
  composite("Quality",multi_items("quality", 1:6), weights =mode_A),
  composite("Loyalty",multi_items("loyalty", 1:5)),
  composite("Satisfaction",multi_items("sat", 1:4)),
  composite("Intention",multi_items("beh_int", 1:6))
)


#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure_pls = relationships(
  paths(from = "Quality", to = "Satisfaction"),
  paths(from = c("Quality", "Satisfaction"), to = c("Intention","Loyalty"))
)

plot(structure_pls)
#------------------------------------------

pls_sem_model=estimate_pls(data_for_semPLS, 
                       measurements_pls,
                       structure_pls,
                       inner_weights = path_weighting )
p4=summary(pls_sem_model)
p4

#All Output:
#---------------
#Descriptives
print(p4$descriptives,digits = 3)

#Loading (For reflective Constructs)
print(p4$loadings,digits = 3)

#Weights (For formative constructs)
print(p4$weights,digits = 3)

#Validity
print(p4$validity,digits = 3)

#Reliability
print(p4$reliability,digits = 3)

#Structural Paths
print(p4$paths,digits = 3)
print(p4$fSquare,digits = 3)

#Total Effect and Indirect Effect
print(p4$total_effects,digits = 3)
print(p4$total_indirect_effects,digits = 3)

#AIC BIC
print(p4$it_criteria,digits = 3)

#Construct Scores
print(p4$composite_scores,digits = 3)


p5=bootstrap_model(pls_sem_model,nboot = 500)
s2=summary(p5)

#===========================================
#===========================================