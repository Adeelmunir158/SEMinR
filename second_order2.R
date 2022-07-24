require(seminr)
attach(mix)

#===========================================

measurements = constructs(
  composite("Intrinsic",multi_items("Intrin",1:3)),
  composite("Extrinsic",multi_items("Extrin",1:3)),
  higher_composite("Comm",c("Intrinsic","Extrinsic"), method = "two stage"),  
  
  composite("Reliability",multi_items("Reliab",1:3)),
  composite("Empathy",multi_items("Emp",1:3)),
  composite("Responsiveness",multi_items("Respo",1:3)),
  higher_composite("PSQ",c("Reliability","Empathy","Responsiveness"), method = "two stage"),  
  
  composite("Retention",multi_items("Reten",1:5))
  
  )

#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure = relationships(
  paths(from = "Comm", to = c("PSQ","Retention")),
  paths(from = "PSQ", to= "Retention")      
                          )

plot(structure)
#===========================================

pls_model = estimate_pls(mix,
                         measurements,
                         structure)
s=summary(pls_model)

s
#Validity
s$validity

#Reliability
s$reliability

#Loadings
s$loadings

#Structural Model
s$paths
s$total_effects
s$total_indirect_effects
s$vif_antecedents
s$fSquare
s$it_criteria
s$composite_scores
# Bootstrap
p5=bootstrap_model(pls_model,nboot = 500)
s2=summary(p5)
s2

#===========================================