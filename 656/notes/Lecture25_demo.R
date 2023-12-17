library(ggplot2)
library(shiny)

sq_exp_kern <- function(x,y=NULL, len_sc = 1, sigma = .0001) {
  len_x <- length(x)
  if(is.null(y)) {
       cv <- matrix(x,nrow=len_x,ncol=len_x)
       cv <- cv - t(cv)
       cv <- cv^2/(2*len_sc)
       cv <- exp(-cv) + diag(sigma,len_x)
    } else {
         len_y <- length(y)
         cv <- matrix(x,nrow=len_x,ncol=len_y)
         cv <- cv - t(matrix(y,nrow=len_y,ncol=len_x))
         cv <- exp(-cv^2/(2*len_sc))
    }
  return(cv)
}

updt_cv <- function(input) {
  cv_xy <<- sq_exp_kern(tm, points$x, len_sc = len_sc)
  cv_yy <<- sq_exp_kern(points$x, len_sc = len_sc, sigma = op_noise)

  icv_yy  <- solve(cv_yy)
  cv_pr   <<- cv - cv_xy %*% icv_yy %*% t(cv_xy)
  mean_pr <<- cv_xy %*% icv_yy %*% points$y
  chol_cv <<- chol(cv_pr)
}

tm <- seq(0,10,.1)
len <- length(tm)

len_sc   <- 1
op_noise <- .1

points <- data.frame(x=vector("numeric", 0), y=vector("numeric", 0))

cv <- sq_exp_kern(tm)
cv_pr <- cv
chol_cv <- chol(cv)
mean_pr <- rep(0,length(tm))

runApp(list(
# ui = pageWithSidebar(
  ui = fluidPage(

    headerPanel("Gaussian process regression"),

    sidebarPanel(
      sliderInput("len_scl",
                  "Lengthscale:",
                  min = .1,
                  max = 10,
                  value = 1),

      sliderInput("op_noise",
                  "Log10 of Output noise variance:",
                  min = -3,
                  max = 2,
                  step = 0.1,
                  value = -1)
    ),

    mainPanel(
      plotOutput("distPlot", click = "plot_click")
    )
  ),
  server =function(input, output, session) {
    autoInvalidate <- reactiveTimer(500, session)

    output$distPlot <- renderPlot({
      autoInvalidate()
      # generate an rnorm distribution and plot it
      ulim <- 4
      ymx <- pmin(ulim, mean_pr + 3*sqrt(diag(cv_pr)))
      ymn <- pmax(-ulim, mean_pr - 3*sqrt(diag(cv_pr)))
      ggplot() + geom_point(data=points, aes(x=x,y=y),color='blue',size=3) +
                 geom_ribbon(data = data.frame(ymx=ymx, ymn = ymn,x=tm),
                               aes(x=x,ymax=ymx,ymin=ymn), color='red',alpha=.2) +
                 geom_line(data = data.frame(y=mean_pr + t(chol_cv) %*% rnorm(len), x = tm),
                            aes(x=x,y=y), color='red',size=2) + ylim(c(-ulim,ulim))
    })

    observeEvent(input$plot_click, {
      points[nrow(points)+1,] <<- c(input$plot_click$x, input$plot_click$y)
      updt_cv(input)
    })

    observeEvent(input$len_scl, {

      len_sc <<- input$len_scl
      cv    <<- sq_exp_kern(tm, len_sc = len_sc)

      if(nrow(points) > 0) {
        updt_cv(input)
      } else chol_cv <<- chol(cv)
    })


    observeEvent(input$op_noise, {

      op_noise <<- 10^(input$op_noise)
      cv    <<- sq_exp_kern(tm, len_sc = len_sc)

      if(nrow(points) > 0) {
        updt_cv(input)
      } else chol_cv <<- chol(cv)
    })

 }
))
