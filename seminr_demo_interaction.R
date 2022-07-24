require(seminr)
attach(data_for_semPLS)
View(data_for_semPLS)
# Define measurements with familiar terms: 
# reflective, multi-item constructs, etc.

#=========================================================
#=========================================================

#PLS-SEM Composite Model- Exact same results of SmartPLS

measurements_pls = constructs(
  composite("Quality",multi_items("quality", 1:6)),
  composite("Loyalty",multi_items("loyalty", 1:5)),
  composite("Satisfaction",multi_items("sat", 1:4)),
  #composite("Intention",multi_items("beh_int", 1:6)),
  interaction_term(iv="Quality", moderator = "Satisfaction")
)

#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure_pls = relationships(
  paths(from = c("Quality", "Satisfaction"), to = "Loyalty"),
  paths(from = "Quality*Satisfaction", to = "Loyalty")
  )

plot(structure_pls)
#------------------------------------------

pls_sem_model=estimate_pls(data_for_semPLS, 
                       measurements_pls,
                       structure_pls,
                       inner_weights = path_weighting )


p1=bootstrap_model(pls_sem_model,nboot = 500)
s=summary(p1)
s
s$bootstrapped_paths


#Slope Analysis
?slope_analysis
slope_analysis(pls_sem_model,
               dv="Loyalty",
               moderator="Satisfaction",
               iv="Quality",
               leg_place = "bottomright")
                #Legend Position
             


#==============================================

#All Output:
#---------------
p=summary(pls_sem_model)

#Descriptives
print(p$descriptives,digits = 3)

#Loading (For reflective Constructs)
print(p$loadings,digits = 3)

#Weights (For formative constructs)
print(p$weights,digits = 3)

#Validity
print(p$validity,digits = 3)

#Reliability
print(p$reliability,digits = 3)

#Structural Paths
print(p$paths,digits = 3)
print(p$fSquare,digits = 3)

#Total Effect and Indirect Effect
print(p$total_effects,digits = 3)
print(p$total_indirect_effects,digits = 3)

#AIC BIC
print(p$it_criteria,digits = 3)

#Construct Scores
print(p$composite_scores,digits = 3)



#===========================================
#===========================================