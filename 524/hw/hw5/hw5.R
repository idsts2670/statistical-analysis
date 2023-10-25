# library
library("tidyverse")
library("car")
library("ggplot2")
library("MASS")
library("fs")
library("moments")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 7.2
# data setup
# Given data
z1 <- c(10, 5, 7, 19, 11, 18)
z2 <- c(2, 3, 3, 6, 7, 9)
y <- c(15, 9, 3, 25, 7, 13)
# Standardize the data
z1_star <- scale(z1, center=TRUE, scale=TRUE)
z2_star <- scale(z2, center=TRUE, scale=TRUE)
y_star <- scale(y, center=TRUE, scale=TRUE)
Z_star <- cbind(z1_star, z2_star)
beta_star <- solve(t(Z_star) %*% Z_star) %*% t(Z_star) %*% y_star

solve((t(Z) %*% Z))
Z_star <- matrix(data = c(-0.292, -1.166, -0.816, 1.283, -0.117, 1.108, -1.088, -0.725, -0.725, 0.363, 0.725, 1.451), nrow=6, ncol=2)
y_star <- matrix(data = c(0.391, -0.391, -1.174, 1.695, -0.652, 0.130), nrow=6, ncol=1)

beta <- solve(t(Z_star) %*% Z_star) %*% t(Z_star) %*% y_star


# 7.25

## data setup
col_names <- c("y1", "y2", "z1", "z2", "z3", "z4", "z5")
df <-  read.table(file.path(main_path, "hw5/T7-6.txt"), header=FALSE, col.names=col_names)
write.csv(df, file = file.path(main_path, "hw5/dataset_hw5.csv"))

# (a)
## (i)
mod1 <- lm(y1 ~ z1 + z2 + z3 + z4 + z5, data = df)
summary(mod1)

### model selection with AIC
mod1_step <- step(mod1, direction="both", scope=list(upper = ~z1+z2+z3+z4+z5, lower=~1), trace = TRUE)
summary(mod1_step)

### model selection with ANOVA
mod2 <- lm(y1 ~ z1 + z2 + z3, data = df)
mod3 <- lm(y1 ~ z1 + z2, data = df)
anova(mod3, mod2, mod1)

## (ii) residual analysis
resid(mod1_step)
resid(mod3)
### residual vs fitted plot
plot(fitted(mod3), resid(mod3), main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "grey")

### create Q-Q plot for residuals
qqnorm(resid(mod3))
### add a straight diagonal line to the plot
qqline(resid(mod3))

## (iii)
### create a new data frame with the values of the predictors
new_data <- data.frame(z1 = 1, z2 = 1200, z3 = 140, z4 = 70, z5 = 85)
predicted_y1 <- predict(mod3, newdata = new_data, interval = "prediction", level = 0.95)

### form a Z matrix with the predicted values and the confidence intervals
Z <- as.matrix(cbind(1, df$z1, df$z2, df$z3, df$z4, df$z5))
### compute Z^T * Z
ZtZ <- t(Z) %*% Z
### compute the inverse of Z^T * Z
ZtZ_inv <- solve(ZtZ)
### compute Z_0 * (Z^T * Z)^{-1} * Z_0^T
Z_0 <- matrix(c(1, 1, 1200, 140, 70, 85), ncol=1)
Z_0tZ_0 <- t(Z_0) %*% ZtZ_inv %*% Z_0


# (b)
## (i)
### compute the predicted value of y_2
mod1 <- lm(y2 ~ z1 + z2 + z3 + z4 + z5, data = df)
summary(mod1)
### model selection with ANOVA
mod2 <- lm(y2 ~ z1 + z2 + z3, data = df)
mod3 <- lm(y2 ~ z1 + z2, data = df)
anova(mod3, mod2, mod1)
summary(mod3)

## (ii) residual analysis
resid(mod3)
### residual vs fitted plot
plot(fitted(mod3), resid(mod3), main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "grey")

### create Q-Q plot for residuals
qqnorm(resid(mod3))
### add a straight diagonal line to the plot
qqline(resid(mod3))

## (iii)
### create a new data frame with the values of the predictors
new_data <- data.frame(z1 = 1, z2 = 1200, z3 = 140, z4 = 70, z5 = 85)
predicted_y1 <- predict(mod3, newdata = new_data, interval = "prediction", level = 0.95)


# (c)
## (i)
### MANOVA test
mod_manova <- manova(cbind(y1, y2) ~ z1 + z2 + z3 + z4 + z5, data = df)
### using Pillai's trace
summary(mod_manova, test = "Pillai")
### type II MANOVA test
Anova(mod_manova, test.statistic = "Pillai", type = "II")
### update the model
mod_manova2 <- manova(cbind(y1, y2) ~ z1 + z2, data = df)
### type II MANOVA test
Anova(mod_manova2, test.statistic = "Pillai", type = "II")

## (ii)
### compute the Mahalanobis distances of the residuals
residuals_matrix <- residuals(mod_manova2)
MDs <- mahalanobis(residuals_matrix, center=colMeans(residuals_matrix), cov=cov(residuals_matrix))
### order the Mahalanobis distances and compute the expected chi-squared quantiles
n <- length(MDs)
p <- ncol(residuals_matrix)
sorted_MDs <- sort(MDs)
expected <- qchisq((1:n) / (n + 1), df = p)

# plot the results
plot(expected, sorted_MDs,
    xlab = "Expected Chi-squared",
    ylab = "Ordered Squared Mahalanobis Distances",
    main = "Chi-squared Plot for Residuals")
abline(0, 1, col = "grey")  # line of equality for reference

## (iii)
# extract residuals
mod_y1 <- lm(y1 ~ z1 + z2, data = df)
mod_y2 <- lm(y2 ~ z1 + z2, data = df)
residuals_y1 <- residuals(mod_y1)
residuals_y2 <- residuals(mod_y2)
# calculate the covariance matrix of the residuals
cov_matrix <- cov(cbind(residuals_y1, residuals_y2))
# Generate the confidence ellipse
conf_ellipse <- ellipse(center=c(mean(df$y1), mean(df$y2)), shape=cov_matrix, radius=2*sqrt(2*log(2)), levels=0.95)
# generate the prediction ellipse
# note: You typically add the standard errors of the prediction to the covariance matrix for prediction ellipses
se_y1 <- sqrt(sum(residuals_y1^2)/(length(residuals_y1)-2))
se_y2 <- sqrt(sum(residuals_y2^2)/(length(residuals_y2)-2))
pred_cov_matrix <- cov_matrix + matrix(c(se_y1^2, 0, 0, se_y2^2), 2, 2)
pred_ellipse <- ellipse(center=c(mean(df$y1), mean(df$y2)), shape=pred_cov_matrix, radius=2*sqrt(2*log(2)), levels=0.95)

# Plot the ellipses
# Determine range for y1
x_range <- range(c(df$y1, conf_ellipse[,1], pred_ellipse[,1]))
# Determine range for y2
y_range <- range(c(df$y2, conf_ellipse[,2], pred_ellipse[,2]))
plot(df$y1, df$y2, xlab="y1", ylab="y2", main="Confidence, Prediction Ellipses & Prediction Intervals", xlim=x_range, ylim=y_range)
lines(conf_ellipse, col="grey", lwd=2)
lines(pred_ellipse, col="black", lwd=2)
legend("topright", legend=c("Confidence Ellipse", "Prediction Ellipse"), col=c("grey", "black"), lwd=2)

radius_95 <- sqrt(qchisq(0.95, df=2))
ellipse(center=c(mean(df$y1), mean(df$y2)), shape=pred_cov_matrix, radius=radius_95, col="red", lwd=2)
draw.ellipse(x=mu1, y=mu4, a=sqrt(lambda1)*sqrt(0.206), b=sqrt(lambda2)*sqrt(0.206), angle=angle, border="black", lwd=2)

pred_ellipse_coords <- ellipse(cov=pred_cov_matrix, level=0.95)

# Intervals for y1 and y2
y1_interval <- c(154.040, 1763.054)
y2_interval <- c(-9.234, 1517.37)

# Add a rectangular region to the existing plot
rect(
    xleft = y1_interval[1], ybottom = y2_interval[1],
    xright= y1_interval[2], ytop = y2_interval[2],
    col=rgb(0,0,0.4,0.2), border="white"
    )

# Fit the regression models (as you've done)
mod_y1 <- lm(y1 ~ z1 + z2, data = df)
mod_y2 <- lm(y2 ~ z1 + z2, data = df)

# Extract residuals (as you've done)
residuals_y1 <- residuals(mod_y1)
residuals_y2 <- residuals(mod_y2)

# Calculate the covariance matrix of the residuals (as you've done)
cov_matrix <- cov(cbind(residuals_y1, residuals_y2))

# Calculate the standard errors of prediction for each response
se_pred_y1 <- sqrt(sum(residuals(mod_y1)^2) / (length(residuals(mod_y1)) - 2))
se_pred_y2 <- sqrt(sum(residuals(mod_y2)^2) / (length(residuals(mod_y2)) - 2))
# Adjust the cov_matrix to get pred_cov_matrix
pred_cov_matrix <- cov_matrix + matrix(c(se_pred_y1^2, 0, 0, se_pred_y2^2), 2, 2)
pred_ellipse_coords <- ellipse(cov=pred_cov_matrix, level=0.95)



# uhoon's code
predictionEllipse <- function(mod, newdata, level = 0.95, ggplot = TRUE){
    # labels
    lev_lbl <- paste0(level * 100, "%")
    resps <- colnames(mod$coefficients)
    title <- paste(lev_lbl, "confidence ellipse for", resps[1], "and", resps[2])
    
    # prediction
    p <- predict(mod, newdata)
    
    # center of ellipse
    cent <- c(p[1,1],p[1,2])
    
    # shape of ellipse
    Z <- model.matrix(mod)
    Y <- mod$model[[1]]
    n <- nrow(Y)
    m <- ncol(Y)
    r <- ncol(Z) - 1
    S <- crossprod(resid(mod))/(n-r-1)
    
    # radius of circle generating the ellipse
    # see Johnson and Wichern (2007), p. 399
    tt <- terms(mod)
    Terms <- delete.response(tt)
    mf <- model.frame(Terms, newdata, na.action = na.pass,
                      xlev = mod$xlevels)
    z0 <- model.matrix(Terms, mf, contrasts.arg = mod$contrasts)
    rad <- sqrt((m*(n-r-1)/(n-r-m)) * qf(level,m,n-r-m) *
                    z0 %% solve(t(Z)%%Z) %*% t(z0))
    
    # generate ellipse using ellipse function in car package
    ell_points <- car::ellipse(center = c(cent), shape = S,
                               radius = c(rad), draw = FALSE)
    
    # ggplot2 plot
    if(ggplot){
        ell_points_df <- as.data.frame(ell_points)
        ggplot2::ggplot(ell_points_df, ggplot2::aes(.data[["x"]], .data[["y"]])) +
            ggplot2::geom_path() +
            ggplot2::geom_point(ggplot2::aes(x = .data[[resps[1]]], 
                                             y = .data[[resps[2]]]),
                                data = data.frame(p)) +
            ggplot2::labs(x = resps[1], y = resps[2],
                          title = title)
    } else {
        # base R plot
        plot(ell_points, type = "l",
             xlab = resps[1], ylab = resps[2],
             main = title)
        points(x = cent[1], y = cent[2])
    }
}

nd <- data.frame(z1 = 1, z2 = 1200)
predictionEllipse(mod = select, newdata = nd)