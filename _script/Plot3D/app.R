#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(simrel)
library(pls)
library(tidyverse)
library(shiny)
library(rgl)
library(shinythemes)

## ---- Simulation ----
set.seed(999)
sobj <- simrel(
    n = 1000,
    p = 2,
    q = 2,
    relpos = 2,
    gamma = 0.9,
    R2 = 0.8,
    type = "univariate"
)
df <- with(sobj, tibble(y = Y[,1], x1 = X[,1], x2 = X[,2]))
# pcr_scores <- bind_cols(df, as_tibble(unclass(scores(pcr(y ~ x1 + x2, data = df)))))
# pls_scores <- bind_cols(df, as_tibble(unclass(scores(plsr(y ~ x1 + x2, data = df)))))
# scores_df <- bind_rows(pcr = pcr_scores, pls = pls_scores, .id = "Method")

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
    theme = shinytheme("yeti"),
    shiny::pageWithSidebar(
        headerPanel = NULL,
        sidebarPanel = sidebarPanel(
            width = 4,
            h3("Comparison of PCR vs PLS"),
            sliderInput("ncomp",
                         "Number of Components",
                         min = 1,
                         max = 2,
                         step = 1,
                         value = 1),
            selectInput("method",
                        "Method",
                        size = 2,
                        selectize = FALSE,
                        list("Principal Component Regression" = "pcr",
                             "Partial Least Squares" = "plsr"))
        ),
        mainPanel = mainPanel(
            rglwidgetOutput("scatter3d")
        )
    )
)   

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scatter3d <- renderRglwidget({
        options(rgl.useNULL=TRUE)
        ## ---- 3D Plot ----
        # x, y, z variables
        x <- df$x1
        y <- df$x2
        z <- df$y
        # Compute the linear regression (z = ax + by + d)
        fit <- get(input$method)(z ~ x + y)
        # predict values on regular xy grid
        grid.lines = 26
        x.pred <- seq(min(x), max(x), length.out = grid.lines)
        y.pred <- seq(min(y), max(y), length.out = grid.lines)
        xy <- expand.grid( x = x.pred, y = y.pred)
        z.pred <- matrix(predict(fit, newdata = xy, ncomp = input$ncomp), 
                         nrow = grid.lines, ncol = grid.lines)
        # fitted points for droplines to surface
        fitpoints <- predict(fit, ncomp = input$ncomp)
        rgl::view3d(theta = 0, phi = -70, fov = 20, zoom = 0.8)
        rgl::plot3d(x, y, z, col = "navyblue",type ="s", 
                    radius = 0.05, aspect = TRUE,
                    xlab = "X1", ylab = "X2", zlab = "Y")
        rgl::persp3d(x.pred, y.pred, z.pred, add = TRUE, col = "#efefef",
                     front = "lines", back = "lines")
        rgl::rglwidget()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
