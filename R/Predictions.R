#Predictions 

make_prediction <- function(Newdata, model){
  Newdata$pred <- predict(model, Newdata, level = 0)
  Designmat <- model.matrix(formula(model)[-2], Newdata)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  Newdata$SE <- sqrt(predvar) 
  return(Newdata)
}


make_prettyplot <- function(dat, xaxis, yaxis, Newdata, prediction, ColorVariable = NULL, SE, line_type = NULL, shape = NULL){
  cmult = 1.96 #make CI
  ggplot(dat, aes(x = {{xaxis}} , y = {{yaxis}}))+ 
    geom_point(alpha = 0.5) +
    geom_line(Newdata, mapping = aes(x = {{xaxis}}, y = {{prediction}}, color = {{ColorVariable}}, shape = {{shape}}), linetype = line_type) +
    geom_ribbon(Newdata, mapping = aes(y= {{prediction}}, ymin = {{prediction}} - cmult * {{SE}}, ymax = {{prediction}} + cmult * {{SE}}, fill = {{ColorVariable}}), alpha = 0.2) +
    theme(line = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank())
}
