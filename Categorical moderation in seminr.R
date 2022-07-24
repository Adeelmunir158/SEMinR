require(seminr)
attach(demo)
View(demo)
# Define measurements with familiar terms: 
# reflective, multi-item constructs, etc.

#=========================================================
#=========================================================

#PLS-SEM Composite Model- Exact same results of SmartPLS

measurements_pls = constructs(
  composite("Quality",multi_items("quality", 1:6)),
  composite("Loyalty",multi_items("loyalty", 1:5)),
  composite("Satisfaction",multi_items("sat", 1:4)),
  composite("Intention",multi_items("beh_int", 1:6)))
  


#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure_pls = relationships(
  paths(from = "Quality", to = "Satisfaction"),
  paths(from = c("Quality", "Satisfaction"), to = c("Intention","Loyalty"))
)

plot(structure_pls)


#------------------------------------------

pls_sem_model=estimate_pls(demo, 
                           measurements_pls,
                           structure_pls,
                           inner_weights = path_weighting )
?
pp=estimate_pls_mga(pls_sem_model, gender<2)
pp
