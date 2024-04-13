#load("../../../data/wage2015_subsample_inference.Rdata")
# Limpiar todo en R
rm(list = ls())

load("C:/Users/Erzo/Documents/GitHub/CausalAI-Course/data/wage2015_subsample_inference.Rdata")
attach(data)
dim(data)


############
#install.packages("xtable")

library(xtable)

Z <- data[data$ad == 1 | data$scl == 1 | data$clg == 1, colnames(data) %in% c("lwage","sex","shs","hsg","scl","clg","ad","ne","mw","so","we","exp1")]
attach(Z)
Z_female <- Z[sex == 1, ]
Z_male <- Z[sex == 0, ]

table <- matrix(0, 12, 3)
table[1:12,1]   <- as.numeric(lapply(Z,mean))
table[1:12,2]   <- as.numeric(lapply(Z_male,mean))
table[1:12,3]   <- as.numeric(lapply(Z_female,mean))
rownames(table) <- c("Log Wage","Sex","Less then High School","High School Graduate","Some College","Gollage Graduate","Advanced Degree", "Northeast","Midwest","South","West","Experience")
colnames(table) <- c("All","Men","Women")
tab<- xtable(table, digits = 4)
tab
colnames(table) <- c("All","Men","Women")
tab<- xtable(table, digits = 4)
tab
##########
mean(Z_female$lwage)-mean(Z_male$lwage)
#########

library(sandwich)
nocontrol.fit <- lm(lwage ~ sex)
nocontrol.est <- summary(nocontrol.fit)$coef["sex",1]
HCV.coefs <- vcovHC(nocontrol.fit, type = 'HC');
nocontrol.se <- sqrt(diag(HCV.coefs))[2] # Estimated std errors

# print unconditional effect of gender and the corresponding standard error
cat ("The estimated gender coefficient is",nocontrol.est," and the corresponding robust standard error is",nocontrol.se) 


############################################################### Ols regression with controls
data1 <- data[data$ad == 1 | data$scl == 1 | data$clg == 1, colnames(data)]
attach(data1)

flex <- lwage ~ sex + (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we)

control.fit <- lm(flex, data1=data1)
control.est <- summary(control.fit)$coef[2,1]

summary(control.fit)

cat("Coefficient for OLS with controls", control.est)

HCV.coefs <- vcovHC(control.fit, type = 'HC');
control.se <- sqrt(diag(HCV.coefs))[2] # Estimated std errors
################################################################ Partialling-Out using ols
flex.y <- lwage ~  (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for Y
flex.d <- sex ~ (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for D

# partialling-out the linear effect of W from Y
t.Y <- lm(flex.y, data1=data1)$res
# partialling-out the linear effect of W from D
t.D <- lm(flex.d, data1=data1)$res

# regression of Y on D after partialling-out the effect of W
partial.fit <- lm(t.Y~t.D)
partial.est <- summary(partial.fit)$coef[2,1]

cat("Coefficient for D via partialling-out", partial.est)

# standard error
HCV.coefs <- vcovHC(partial.fit, type = 'HC')
partial.se <- sqrt(diag(HCV.coefs))[2]

# confidence interval
confint(partial.fit)[2,]
#########################################################################LASSO
#install.packages("hdm")
library(hdm)

# models
flex.y <- lwage ~  (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for Y
flex.d <- sex ~ (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for D

# partialling-out the linear effect of W from Y
t.Y <- rlasso(flex.y, data1=data1)$res
# partialling-out the linear effect of W from D
t.D <- rlasso(flex.d, data1=data1)$res

# regression of Y on D after partialling-out the effect of W
partial.lasso.fit <- lm(t.Y~t.D)
partial.lasso.est <- summary(partial.lasso.fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial.lasso.est)

# standard error
HCV.coefs <- vcovHC(partial.lasso.fit, type = 'HC')
partial.lasso.se <- sqrt(diag(HCV.coefs))[2]

###############################################################
table<- matrix(0, 4, 2)
table[1,1]<- nocontrol.est  
table[1,2]<- nocontrol.se   
table[2,1]<- control.est
table[2,2]<- control.se    
table[3,1]<- partial.est  
table[3,2]<- partial.se  
table[4,1]<-  partial.lasso.est
table[4,2]<- partial.lasso.se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("Without controls", "full reg", "partial reg", "partial reg via lasso")	
tab<- xtable(table, digits=c(3, 3, 4))
tab
###############################################################EXTRA FLEXIBLE MODEL
attach(data1)
extraflex <- lwage ~ sex + (exp1+exp2+exp3+exp4+scl+clg+ad+occ2+ind2+mw+so+we)^2

control.fit <- lm(extraflex, data1=data1)
#summary(control.fit)
control.est <- summary(control.fit)$coef[2,1]


cat("Number of Extra-Flex Controls", length(control.fit$coef)-1, "\n")


cat("Coefficient for OLS with extra flex controls", control.est)


#summary(control.fit)
HCV.coefs <- vcovHC(control.fit, type = 'HC');

n= length(wage); p =length(control.fit$coef);

control.se <- sqrt(diag(HCV.coefs))[2]*sqrt(n/(n-p)) # Estimated std errors

###
library(hdm)

# models
extraflex.y <- lwage ~  (exp1+exp2+exp3+exp4+shs+hsg+scl+clg+occ2+ind2+mw+so+we)^2 # model for Y
extraflex.d <- sex ~ (exp1+exp2+exp3+exp4+shs+hsg+scl+clg+occ2+ind2+mw+so+we)^2 # model for D

# partialling-out the linear effect of W from Y
t.Y <- rlasso(extraflex.y, data1=data1)$res
# partialling-out the linear effect of W from D
t.D <- rlasso(extraflex.d, data1=data1)$res

# regression of Y on D after partialling-out the effect of W
partial.lasso.fit <- lm(t.Y~t.D)
partial.lasso.est <- summary(partial.lasso.fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial.lasso.est)

# standard error
HCV.coefs <- vcovHC(partial.lasso.fit, type = 'HC')
partial.lasso.se <- sqrt(diag(HCV.coefs))[2]


table<- matrix(0, 2, 2)
table[1,1]<- control.est
table[1,2]<- control.se    
table[2,1]<-  partial.lasso.est
table[2,2]<- partial.lasso.se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("full reg","partial reg via lasso")	
tab<- xtable(table, digits=c(3, 3, 4))
tab

print(tab, type="latex")

#############################################################
#Use appropiate plots (i.e hist, barplots, scatter plots , pie plots, etc) 
#to describe main varaibles (wage, log-wage, sex, some college, college graduate, 
#avdanced degree, Experience)

###################
library(ggplot2)
###################
ggplot(data1, aes(x = "", y = wage)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  labs(title = "Gráfico de Violín para Salarios", y = "Salario") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Eliminar etiquetas del eje x
        axis.ticks.x = element_blank()) # Eliminar marcas del eje x

###################### Lorenz Curve for Salaries by Deciles for the subgroup of individuals who accessed higher education ######################

# Sort wages in ascending order
wage_sorted <- sort(wage)
# Compute the cumulative sum of sorted wages
cumulative_sum <- cumsum(wage_sorted)
# Calculate the deciles of the cumulative sum of wages
deciles <- quantile(cumulative_sum, probs = seq(0, 1, by = 0.1))

# Create a vector for the cumulative fraction of the population for x-axis
population_fraction <- seq(0, 1, by = 0.1)

# Calculate the cumulative fraction of salaries
salary_fraction <- quantile(deciles/sum(wage), probs = seq(0, 1, by = 0.1))

# Plot the Lorenz curve
plot(population_fraction, salary_fraction, type = "l", lwd = 2, col = "blue",
     main = "Lorenz Curve for Higher Education Salaries",
     xlab = "Cumulative fraction of the population",
     ylab = "Cumulative fraction of salaries")
abline(0, 1, lty = 2, col = "red")  # Equality line



########################### Histogram for lwage ###########################

ggplot(data1, aes(x = lwage)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Salaries with Density Function", x = "Lwage", y = "Density") +
  theme_minimal()


############################Graph bar sex
total_observations <- nrow(data1)
proportions <- prop.table(table(data1$sex)) * 100
plot_data <- data.frame(sex = factor(names(proportions), labels = c("Male", "Female")), proportion = as.numeric(proportions))

ggplot(plot_data, aes(x = sex, y = proportion, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Male and Female Individuals with Higher Education",
       x = "Sex (0 = Male, 1 = Female)", y = "Percentage",
       fill = "Sex") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), vjust = -0.5, size = 3) +
  geom_text(aes(x = 1.5, y = max(plot_data$proportion) * 0.9,
                label = paste("Total observations:", total_observations)), 
            hjust = 0.5, size = 3) +
  guides(fill = guide_legend(title = "Sex", reverse = TRUE))


########################Education Status"Analysis of individuals who accessed higher education"
library(dplyr)
library(tidyr)
library(ggplot2)

data1 <- data1 %>%
  mutate(Education_Status = case_when(
    scl == 1 ~ "Some College",
    clg == 1 ~ "College Graduate",
    ad == 1 ~ "Advanced Degree"
  ))

edu_freq <- data1 %>%
  count(Education_Status)
total_obs <- sum(edu_freq$n)
edu_freq <- edu_freq %>%
  mutate(Percentage = n / total_obs * 100)

# Crear el gráfico de pastel
ggplot(edu_freq, aes(x = "", y = Percentage, fill = Education_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  labs(title = "Education Status Distribution",
       x = NULL, y = NULL,
       fill = "Education Status",
       caption = paste("Total observations:", total_obs)) +
  scale_y_continuous(labels = scales::percent_format()) +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) + 
  theme_void() +
  theme(legend.position = "right")  

#######################################Experience
ggplot(data1, aes(y = exp1)) +
  geom_boxplot() +
  labs(title = "Distribution of Experience in Individuals with Higher Education",
       y = "Experience (exp1)")
##################################################COEF PLOT
attach(data1)
library(coefplot)
library(gridExtra)
flex.y <- lwage ~  (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for Y
flex.d <- sex ~ (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we) # model for D
# partialling-out the linear effect of W from Y
t.Y <- lm(flex.y, data1=data1)$res
# partialling-out the linear effect of W from D
t.D <- lm(flex.d, data1=data1)$res

# Ajustar los modelos
nocontrol.fit <- lm(lwage ~ sex)
control.fit <- lm(lwage ~ sex + (exp1+exp2+exp3+exp4)*(scl+clg+ad+occ2+ind2+mw+so+we))
partial.fit <- lm(t.Y ~ t.D)



# Graficar los coeficientes estimados con límites en el eje x ajustados
plot_nocontrol <- coefplot(nocontrol.fit, coefficients = "sex")+labs(title = "No Controls") + xlim(-0.15, 0)
plot_control <- coefplot(control.fit, coefficients = "sex")+labs(title = "Controls")  + xlim(-0.15, 0)
plot_partial <- coefplot(partial.fit, coefficients = "t.D")+labs(title = "Partialling out")  + xlim(-0.15, 0)

# Unir los gráficos verticalmente
combined_plot <- grid.arrange(plot_nocontrol, plot_control, plot_partial, nrow = 3)




# The coefficient associated with the gender variable, which indicates the prediction of being female on salary, is initially 
# negative. This suggests that, on average, women have lower salaries than men. However, after adding these controls, 
# such as work experience or educational level, the negative coefficient associated with the gender variable becomes 
# less negative.
# 
# This change in the gender coefficient could be explained by the fact that the control variables are capturing
# some of the variability in salaries that was previously incorrectly attributed to gender. This suggests 
# that additional factors, beyond gender, are influencing salaries, and the impact of gender on salaries 
# is less pronounced once these other variables are taken into account.Besides, both FWL and including control 
# variables in the regression model yield coefficient estimates for the variable of interest that reflect its net
# impact on the dependent variable, once the effects of other explanatory variables have been taken into account.

############################################################################################

# Generar valores de exp4 para predecir las medias de lwage con más puntos
exp4_seq <- seq(min(data1$exp4), max(data1$exp4), length.out = 500)

# Ajustar el modelo LOESS 
loess_model <- loess(lwage ~ (exp4 + sex), data = data1, span = 0.9)

# Crear un nuevo data frame con los valores de exp4 para predecir
new_data <- data.frame(exp4 = exp4_seq, sex = 1)

# Predecir las medias de lwage utilizando el modelo loess
lwage_mean_pred <- predict(loess_model, newdata = new_data)

# Calcula la media de lwage para cada valor único de exp4 solo para mujeres
mean_lwage_women <- tapply(subset(data1, sex == 1)$lwage, subset(data1, sex == 1)$exp4, mean)

# Graficar ambas relaciones en un solo gráfico
plot(exp4_seq, lwage_mean_pred,
     type = "l",  # Tipo de gráfico: línea
     col = "red", # Color de la línea para el modelo LOESS
     lwd = 2,     # Grosor de la línea para el modelo LOESS
     xlab = "exp4",
     ylab = "Mean lwage",
     main = "Mean lwage vs exp4 with Loess Smoothing and Mean lwage vs exp4 (Women only)",
     xlim = c(0, 40),  # Limitar los valores en el eje x de 0 a 40
     ylim = c(2.6, 3.4))   # Ajustar la escala del eje y de 2 a 4
lines(as.numeric(names(mean_lwage_women)), mean_lwage_women, col = "blue", lwd = 2)  # Agregar la relación de media de lwage vs exp4
legend("topright", legend = c("Loess Smoothing", "Mean lwage (Women only)"), col = c("red", "blue"), lty = 1, lwd = 2)



# Generar valores de exp4 para predecir las medias de lwage con más puntos
exp4_seq <- seq(min(data1$exp4), max(data1$exp4), length.out = 500)

# Ajustar el modelo LOESS 
loess_model_men <- loess(lwage ~ (exp4 + sex), data = data1, span = 0.9)

# Crear un nuevo data frame con los valores de exp4 para predecir
new_data_men <- data.frame(exp4 = exp4_seq, sex = 0)  # Solo varones

# Predecir las medias de lwage utilizando el modelo loess para varones
lwage_mean_pred_men <- predict(loess_model_men, newdata = new_data_men)

# Calcula la media de lwage para cada valor único de exp4 solo para varones
mean_lwage_men <- tapply(subset(data1, sex == 0)$lwage, subset(data1, sex == 0)$exp4, mean)

# Graficar ambas relaciones en un solo gráfico
plot(exp4_seq, lwage_mean_pred_men,
     type = "l",  # Tipo de gráfico: línea
     col = "red", # Color de la línea para el modelo LOESS
     lwd = 2,     # Grosor de la línea para el modelo LOESS
     xlab = "exp4",
     ylab = "Mean lwage",
     main = "Mean lwage vs exp4 with Loess Smoothing and Mean lwage vs exp4 (Men only)",
     xlim = c(0, 40),  # Limitar los valores en el eje x de 0 a 40
     ylim = c(2.6, 3.4))   # Ajustar la escala del eje y de 2 a 4
lines(as.numeric(names(mean_lwage_men)), mean_lwage_men, col = "blue", lwd = 2)  # Agregar la relación de media de lwage vs exp4 para varones
legend("topright", legend = c("Loess Smoothing", "Mean lwage (Men only)"), col = c("red", "blue"), lty = 1, lwd = 2)
