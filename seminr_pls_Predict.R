
install.packages("seminr")
require(seminr)
attach(data_for_seminr)
View(data_for_seminr)
# Define measurements with familiar terms: 
# reflective, multi-item constructs, etc.

#===========================================
#===========================================

measurements = constructs(
  composite("Quality",multi_items("quality", 1:6)),
  composite("Loyalty",multi_items("loyalty", 1:5)),
  composite("Satisfaction",multi_items("sat", 1:4))
                          )
#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs
structure = relationships(
  paths(from = "Quality", to = c("Satisfaction","Loyalty")),
  paths(from = "Loyalty", to = c("Satisfaction")))
#paths(from = c("Quality","Satisfaction"), to = c("Intention","Loyalty")),
plot(structure)

#=========================================================
#=========================================================

# Run the model

e=estimate_pls(data_for_seminr,measurements, structure)


x=summary(e)
x

# PLS prdict

p=predict_pls(e, technique = predict_DA, noFolds = 10)

pp=summary(p)
pp


# Chck whether to go for RMSE OR MAE
# USE RMSE for symmetric distribution
 
par(mfrow=c(1,5))

plot(pp, indicator = "loyalty1")
plot(pp, indicator = "loyalty2")
plot(pp, indicator = "loyalty3")
plot(pp, indicator = "loyalty4")
plot(pp, indicator = "loyalty5")
par(mfrow=c(1,1))


# Compare the LM values with RMSE
pp$PLS_out_of_sample
pp$LM_out_of_sample
# Difference between LM and RMSE

z=pp$PLS_out_of_sample-pp$LM_in_sample
z
# Extract 1st row to get the value of RMSE
#Get second tow to extract the value of MAE
round(z[1,],3)


# Interpretation
#If all the items of the construct have low RMSE or MAE values than the
#LM values then high predictability.

#If majority of items of the construct have low RMSE or MAE values than the
#LM values then average predictability.

#If few of the items of the construct have low RMSE or MAE values than the
#LM values then low predictability.

#If none of the items of the construct have low RMSE or MAE values than the
#LM values then no predictability.

