require(seminr)
attach(data)
View(data)
# Define measurements with familiar terms: 
# reflective, multi-item constructs, etc.

#===========================================
#===========================================


#PLS-SEM Composite Model- Exact same results of SmartPLS

measurements_pls = constructs(
  composite("Quality",multi_items("quality", 1:6), weights =mode_A),
  composite("Loyalty",multi_items("loyalty", 1:5), weights =mode_A),
  composite("Satisfaction",multi_items("sat", 1:4), weights =mode_A),
  composite("Intention",multi_items("beh_int", 1:6), weights =mode_A)
)


#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure_pls = relationships(
  paths(from = "Quality", to = "Satisfaction"),
  paths(from = c("Quality", "Satisfaction"), to = c("Intention","Loyalty")),
  paths(from = "Loyalty", to = "Intention")
)

plot(structure_pls)
#------------------------------------------

pls_sem_model=estimate_pls(data, 
                       measurements_pls,
                       structure_pls,
                       inner_weights = path_weighting )

plot(pls_sem_model, cex=3)

p4=summary(pls_sem_model)


#All Output:
#---------------
#Descriptives
p4$descriptives

#Loading (For reflective Constructs)
p4$loadings

#Weights (For formative constructs)
p4$weights

#Validity
p4$validity

#Reliability
p4$reliability

#Structural Paths
p4$paths
p4$fSquare

#----------------------------
#AIC BIC
p4$it_criteria

#----------------------------
#Total Effect and Indirect Effect

#Direct Effect
p4$paths

#Indirect Effect
p4$total_indirect_effects

#Total Effect
p4$total_effects

#----------------------------
#Construct Scores
score=p4$composite_scores
quality=score[,1]
loyal=score[,2]
sat=score[,3]

#----------------------------
#Bootstrapping
p5=bootstrap_model(pls_sem_model,nboot = 1000)
s2=summary(p5)

#Mediation Analysis


#Direct Effect
s2$bootstrapped_paths


#Indirect Effect
specific_effect_significance(p5,
  from = "Quality",
  through = "Satisfaction",
  to= "Loyalty",
  alpha = 0.05)


#Serial Mediation
specific_effect_significance(p5,
  from = "Quality",
  through = c("Satisfaction","Loyalty"),
  to= "Intention",
  alpha = 0.05)



plot(p5)