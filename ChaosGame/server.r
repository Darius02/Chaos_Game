if (!require("shiny"))
    install.packages("shiny")
if(!require("shape"))
    install.packages("shape")

library(shiny)
library(shape)

tri.gen <- function(distanta) {
    dist <- distanta
    
    
    # matricea matrice_colturi are coloanele puse astfel: nr de ordine a coltului, coordonata x a coltului, coordonata y a coltului
    matrice_colturi <- matrix(NA, ncol = 3, nrow = 3)
    
    #initializare matrice matrice_colturi pentru triunghi
    
    matrice_colturi[1, ] <- c(1, 0, 0)
    matrice_colturi[2, ] <- c(2, 0.5, sqrt(3) / 2)
    matrice_colturi[3, ] <- c(3, 1, 0)
    
    # sirul ordine_colturi contine nr de ordine a coltului urmator
    ordine_colturi <- runif(5000)
    ordine_colturi[which(ordine_colturi > 2 / 3)] <- 3
    ordine_colturi[which(1 / 3 < ordine_colturi & ordine_colturi <= 2 / 3)] <- 2
    ordine_colturi[which(ordine_colturi <= 1 / 3)] <- 1
    
    #coordonate_puncte: matrice ptr coordonatele punctelor ce se vor desena
    
    coordonate_puncte <- matrix(NA, ncol = 2, nrow = 5001)
    colnames(coordonate_puncte) <- c("x", "y") #pentru ggvis
    
    #primul punct(cel random)
    
    coordonate_puncte[1, ] <- c(runif(1), runif(1) * sqrt(3) / 2)
    
    
    #restul punctelor
    for (i in 1:5000) {
        row <- i + 1
        #spot -> la ce colt se duce
        spot <- which(matrice_colturi[, 1] == ordine_colturi[i])
        coordonate_puncte[row, ] <- c(dist * matrice_colturi[spot, 2] + (1 - dist) * coordonate_puncte[i, 1], dist * matrice_colturi[spot, 3] + (1 - dist) * coordonate_puncte[i, 2])
    }
    return(list(matrice_colturi, ordine_colturi, coordonate_puncte))
}

sqr.gen <- function(distanta) {
    dist <- distanta
    
    
    
    # matricea matrice_colturi are coloanele puse astfel: nr de ordine a coltului, coordonata x a coltului, coordonata y a coltului
    matrice_colturi <- matrix(NA, ncol = 3, nrow = 8)
    
    #initializare matrice matrice_colturi pentru patrat
    
    matrice_colturi[1, ] <- c(1, 0.0, 0.0)
    matrice_colturi[2, ] <- c(2, 0.5, 0.0)
    matrice_colturi[3, ] <- c(3, 1.0, 0.0)
    matrice_colturi[4, ] <- c(4, 1.0, 0.5)
    matrice_colturi[5, ] <- c(5, 1.0, 1.0)
    matrice_colturi[6, ] <- c(6, 0.5, 1.0)
    matrice_colturi[7, ] <- c(7, 0.0, 1.0)
    matrice_colturi[8, ] <- c(8, 0.0, 0.5)
    
    # sirul ordine_colturi contine nr de ordine a coltului urmator
    ordine_colturi <- runif(5000)
    ordine_colturi[which(ordine_colturi > 7 / 8)] <- 8
    ordine_colturi[which(6 / 8 < ordine_colturi & ordine_colturi <= 7 / 8)] <- 7
    ordine_colturi[which(5 / 8 < ordine_colturi & ordine_colturi <= 6 / 8)] <- 6
    ordine_colturi[which(4 / 8 < ordine_colturi & ordine_colturi <= 5 / 8)] <- 5
    ordine_colturi[which(3 / 8 < ordine_colturi & ordine_colturi <= 4 / 8)] <- 4
    ordine_colturi[which(2 / 8 < ordine_colturi & ordine_colturi <= 3 / 8)] <- 3
    ordine_colturi[which(1 / 8 < ordine_colturi & ordine_colturi <= 2 / 8)] <- 2
    ordine_colturi[which(ordine_colturi <= 1 / 8)] <- 1
    
    #coordonate_puncte: matrice ptr coordonatele punctelor ce se vor desena
    
    coordonate_puncte <- matrix(NA, ncol = 2, nrow = 5001)
    colnames(coordonate_puncte) <- c("x", "y") #pentru ggvis
    
    #primul punct(cel random)
    coordonate_puncte[1, ] <- c(runif(1), runif(1))
    
    #restul punctelor
    for (i in 1:5000) {
        row <- i + 1
        #spot -> la ce colt se duce
        spot <- which(matrice_colturi[, 1] == ordine_colturi[i])
        coordonate_puncte[row, ] <- c(dist * matrice_colturi[spot, 2] + (1 - dist) * coordonate_puncte[i, 1], dist * matrice_colturi[spot, 3] + (1 - dist) * coordonate_puncte[i, 2])
    }
    
    return(list(matrice_colturi, ordine_colturi, coordonate_puncte))
}

pent.gen <- function(distanta) {
    dist <- distanta
    
    # matricea matrice_colturi are coloanele puse astfel: nr de ordine a coltului, coordonata x a coltului, coordonata y a coltului
    matrice_colturi <- matrix(NA, ncol = 3, nrow = 5)
    
    #initializare matrice matrice_colturi pentru pentagon
    
    matrice_colturi[1, ] <- c(1, 0, 1)
    matrice_colturi[2, ] <- c(2, 0.25 * (sqrt(10 + 2 * sqrt(5))), 0.25 * (sqrt(5) - 1))
    matrice_colturi[3, ] <- c(3, 0.25 * (sqrt(10 - 2 * sqrt(5))), -0.25 * (sqrt(5) + 1))
    matrice_colturi[4, ] <- c(4, -0.25 * (sqrt(10 - 2 * sqrt(5))), -0.25 * (sqrt(5) + 1))
    matrice_colturi[5, ] <- c(5, -0.25 * (sqrt(10 + 2 * sqrt(5))), 0.25 * (sqrt(5) - 1))
    
    # sirul ordine_colturi contine nr de ordine a coltului urmator
    ordine_colturi <- runif(5000)
    ordine_colturi[which(ordine_colturi > 4 / 5)] <- 5
    ordine_colturi[which(3 / 5 < ordine_colturi & ordine_colturi <= 4 / 5)] <- 4
    ordine_colturi[which(2 / 5 < ordine_colturi & ordine_colturi <= 3 / 5)] <- 3
    ordine_colturi[which(1 / 5 < ordine_colturi & ordine_colturi <= 2 / 5)] <- 2
    ordine_colturi[which(ordine_colturi <= 1 / 5)] <- 1
    
    #coordonate_puncte: matrice ptr coordonatele punctelor ce se vor desena
    
    coordonate_puncte <- matrix(NA, ncol = 2, nrow = 5001)
    colnames(coordonate_puncte) <- c("x", "y") #pentru ggvis
    
    #primul punct(cel random)
    coordonate_puncte[1, ] <- c(runif(1, -0.25 * (sqrt(10 + 2 * sqrt(5))), 0.25 * (sqrt(10 + 2 * sqrt(5)))), runif(1, -0.25 * (sqrt(5) + 1), 1))
    
    #restul punctelor
    for (i in 1:5000) {
        row <- i + 1
        #spot -> la ce colt se duce
        spot <- which(matrice_colturi[, 1] == ordine_colturi[i])
        coordonate_puncte[row, ] <- c(dist * matrice_colturi[spot, 2] + (1 - dist) * coordonate_puncte[i, 1], dist * matrice_colturi[spot, 3] + (1 - dist) * coordonate_puncte[i, 2])
    }
    
    return(list(matrice_colturi, ordine_colturi, coordonate_puncte))
}

hex.gen <- function(distanta) {
    dist <- distanta
    
    # matricea matrice_colturi are coloanele puse astfel: nr de ordine a coltului, coordonata x a coltului, coordonata y a coltului
    matrice_colturi <- matrix(NA, ncol = 3, nrow = 6)
    
    matrice_colturi[1, ] <- c(1, 0, 1)
    matrice_colturi[2, ] <- c(2, sqrt(3) / 2, 0.5)
    matrice_colturi[3, ] <- c(3, sqrt(3) / 2, -0.5)
    matrice_colturi[4, ] <- c(4, 0, -1)
    matrice_colturi[5, ] <- c(5, -sqrt(3) / 2, -0.5)
    matrice_colturi[6, ] <- c(6, -sqrt(3) / 2, 0.5)
    
    # sirul ordine_colturi contine nr de ordine a coltului urmator
    ordine_colturi <- runif(5000)
    ordine_colturi[which(ordine_colturi > 5 / 6)] <- 6
    ordine_colturi[which(4 / 6 < ordine_colturi & ordine_colturi <= 5 / 6)] <- 5
    ordine_colturi[which(3 / 6 < ordine_colturi & ordine_colturi <= 4 / 6)] <- 4
    ordine_colturi[which(2 / 6 < ordine_colturi & ordine_colturi <= 3 / 6)] <- 3
    ordine_colturi[which(1 / 6 < ordine_colturi & ordine_colturi <= 2 / 6)] <- 2
    ordine_colturi[which(ordine_colturi <= 1 / 6)] <- 1
    
    #coordonate_puncte: matrice ptr coordonatele punctelor ce se vor desena
    
    coordonate_puncte <- matrix(NA, ncol = 2, nrow = 5001)
    colnames(coordonate_puncte) <- c("x", "y") #pentru ggvis
    
    #primul punct(cel random)
    coordonate_puncte[1, ] <- c(runif(1, -sqrt(3) / 2, sqrt(3) / 2), runif(1, -1, 1))
    
    for (i in 1:5000) {
        row <- i + 1
        #spot -> la ce colt se duce
        spot <- which(matrice_colturi[, 1] == ordine_colturi[i])
        coordonate_puncte[row, ] <- c(dist * matrice_colturi[spot, 2] + (1 - dist) * coordonate_puncte[i, 1], dist * matrice_colturi[spot, 3] + (1 - dist) * coordonate_puncte[i, 2])
    }
    
    return(list(matrice_colturi, ordine_colturi, coordonate_puncte))
}

shinyServer(function(input, output, session) {
    
    output$my.init <- renderUI({
        sliderInput(
            inputId = "init",
            "Numărul de puncte:",
            min = 1,
            max = 5000,
            step = 1,
            value = 1,
            animate = animationOptions(
                interval = input$speed,
                playButton = div(
                    actionButton(
                        inputId = "shape",
                        label = "START",
                        style = "background-color: #6699ff; width: 80px; color: white"
                    ),
                    style = "margin-top: 20px; margin-bottom: -10px; display: flex; justify-content: center;"
                ),
                pauseButton = div(
                    actionButton(
                        inputId = "shape",
                        label = "STOP",
                        style = "background-color: #6699ff; width: 80px; color: white"
                    ),
                    style = "margin-top: 20px; margin-bottom: -10px; display: flex; justify-content: center;"
                )
            )
        )
    })
    
    output$my.extend <- renderUI({
        sliderInput(
            "extend",
            "Numărul de puncte:",
            min = 1,
            max = 5000,
            step = input$steps,
            value = 1,
            animate = animationOptions(
                interval = input$speed,
                playButton = div(
                    actionButton(
                        inputId = "shape",
                        label = "START",
                        style = "background-color: #6699ff; width: 80px; color: white"
                    ),
                    style = "margin-top: 20px; margin-bottom: -10px; display: flex; justify-content: center;"
                ),
                pauseButton = div(
                    actionButton(
                        inputId = "shape",
                        label = "STOP",
                        style = "background-color: #6699ff; width: 80px; color: white"
                    ),
                    style = "margin-top: 20px; margin-bottom: -10px; display: flex; justify-content: center;"
                )
            )
        )
    })
    
    all.list <- reactive({
        if (input$shape == "tri") {
            return(tri.gen(input$dist.tri * (input$gen > -1)))
        }
        if (input$shape == "sqr") {
            return(sqr.gen(input$dist.sqr * (input$gen > -1)))
        }
        if (input$shape == "pent") {
            return(pent.gen(input$dist.pent * (input$gen > -1)))
        }
        if (input$shape == "hex") {
            return(hex.gen(input$dist.hex * (input$gen > -1)))
        }
    })
    
    
    # initPlot
    
    output$initPlot <- renderPlot({
        
        #loci -> matrice_colturi
        matrice_colturi      <- all.list()[[1]]
        
        #vertices -> ordine_colturi
        ordine_colturi  <- all.list()[[2]]
        
        #coords -> coordonate_puncte
        coordonate_puncte    <- all.list()[[3]]
        
        # Triangle:INIT
        
        if (input$shape == "tri") {
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(0, 1),
                ylim = c(0, sqrt(3) / 2),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
            
            
            if (!is.null(input$init)) {
                if (input$init == 1) {
                    points(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2],
                        pch = 20,
                        cex = 3,
                        col = "black"
                    )
                    
                    if (coordonate_puncte[1, 1] >= 0.5 & coordonate_puncte[1, 2] <= sqrt(3) / 4) {
                        text(
                            coordonate_puncte[1, 1],
                            coordonate_puncte[1, 2] + 0.04,
                            "Punct Inițial",
                            col = "black",
                            pos = 2
                        )
                    }
                    if (coordonate_puncte[1, 1] >= 0.5 & coordonate_puncte[1, 2] > sqrt(3) / 4) {
                        text(
                            coordonate_puncte[1, 1],
                            coordonate_puncte[1, 2] - 0.04,
                            "Punct Inițial",
                            col = "black",
                            pos = 2
                        )
                    }
                    if (coordonate_puncte[1, 1] < 0.5 & coordonate_puncte[1, 2] > sqrt(3) / 4) {
                        text(
                            coordonate_puncte[1, 1],
                            coordonate_puncte[1, 2] - 0.04,
                            "Punct Inițial",
                            col = "black",
                            pos = 4
                        )
                    }
                    if (coordonate_puncte[1, 1] < 0.5 & coordonate_puncte[1, 2] <= sqrt(3) / 4) {
                        text(
                            coordonate_puncte[1, 1],
                            coordonate_puncte[1, 2] + 0.04,
                            "Punct Inițial",
                            col = "black",
                            pos = 4
                        )
                    }
                }
            }
        }
        
        # Square:INIT
        
        if (input$shape == "sqr") {
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(0, 1),
                ylim = c(0, 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
            
            if (input$init == 1) {
                points(
                    coordonate_puncte[1, 1],
                    coordonate_puncte[1, 2],
                    pch = 20,
                    cex = 3,
                    col = "black"
                )
                
                if (coordonate_puncte[1, 1] >= 0.5 & coordonate_puncte[1, 2] <= 0.5) {
                    # DREAPTA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] >= 0.5 & coordonate_puncte[1, 2] > 0.5) {
                    # DREAPTA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] < 0.5 & coordonate_puncte[1, 2] > 0.5) {
                    # STANGA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
                if (coordonate_puncte[1, 1] < 0.5 & coordonate_puncte[1, 2] <= 0.5) {
                    # STANGA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
            }
        }
        
        # Pentagon:INIT
        
        if (input$shape == "pent") {
            
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(-0.25 * (sqrt(10 + 2 * sqrt(5))), 0.25 * (sqrt(10 + 2 * sqrt(5)))),
                ylim = c(-0.25 * (sqrt(5) + 1), 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
            
            if (input$init == 1) {
                points(
                    coordonate_puncte[1, 1],
                    coordonate_puncte[1, 2],
                    pch = 20,
                    cex = 3,
                    col = "black"
                )
                
                if (coordonate_puncte[1, 1] >= 0 & coordonate_puncte[1, 2] <= 0) {
                    # DREAPTA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] >= 0 & coordonate_puncte[1, 2] > 0) {
                    # DREAPTA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] < 0 & coordonate_puncte[1, 2] > 0) {
                    # STANGA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
                if (coordonate_puncte[1, 1] < 0 & coordonate_puncte[1, 2] <= 0) {
                    # STANGA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
            }
        }
        
        
        # Hexagon:INIT
        
        if (input$shape == "hex") {
            
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(-sqrt(3) / 2, sqrt(3) / 2),
                ylim = c(-1, 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
            
            if (input$init == 1) {
                points(
                    coordonate_puncte[1, 1],
                    coordonate_puncte[1, 2],
                    pch = 20,
                    cex = 3,
                    col = "black"
                )
                
                if (coordonate_puncte[1, 1] >= 0 & coordonate_puncte[1, 2] <= 0) {
                    # DREAPTA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] >= 0 & coordonate_puncte[1, 2] > 0) {
                    # DREAPTA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 2
                    )
                }
                if (coordonate_puncte[1, 1] < 0 & coordonate_puncte[1, 2] > 0) {
                    # STANGA SUS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] - 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
                if (coordonate_puncte[1, 1] < 0 & coordonate_puncte[1, 2] <= 0) {
                    # STANGA JOS
                    text(
                        coordonate_puncte[1, 1],
                        coordonate_puncte[1, 2] + 0.04,
                        "Punct Inițial",
                        col = "black",
                        pos = 4
                    )
                }
            }
        }
        
        # APPLIED TO ALL
        
        if (!is.null(input$init)) {
            if (input$init != 1) {
                points(
                    coordonate_puncte[1:input$init - 1, 1],
                    coordonate_puncte[1:input$init - 1, 2],
                    pch = 20,
                    cex = 1,
                    col = "black"
                )
                
                points(
                    coordonate_puncte[input$init - 1, 1],
                    coordonate_puncte[input$init - 1, 2],
                    pch = 20,
                    cex = 2.75,
                    col = "black"
                )
                points(
                    coordonate_puncte[input$init, 1],
                    coordonate_puncte[input$init, 2],
                    pch = 21,
                    cex = 3,
                    col = "black",
                    bg = "white"
                )
                points(
                    coordonate_puncte[input$init, 1],
                    coordonate_puncte[input$init, 2],
                    pch = 20,
                    cex = 2.75,
                    col = "black"
                )
                
                x0 <- coordonate_puncte[input$init - 1, 1]
                y0 <- coordonate_puncte[input$init - 1, 2]
                x1 <- coordonate_puncte[input$init, 1]
                y1 <- coordonate_puncte[input$init, 2]
                
                Arrows((.6 * x0 + .4 * x1),
                       (.6 * y0 + .4 * y1),
                       (.4 * x0 + .6 * x1),
                       (.4 * y0 + .6 * y1),
                       col = "orange",
                       lwd = 2
                )
                
                v.x <- matrice_colturi[matrice_colturi[, 1] == ordine_colturi[input$init - 1], 2]
                v.y <- matrice_colturi[matrice_colturi[, 1] == ordine_colturi[input$init - 1], 3]
                
                points(v.x,
                       v.y,
                       pch = 1,
                       cex = 4,
                       lwd = 2)
                points(v.x,
                       v.y,
                       pch = 1,
                       cex = 3,
                       lwd = 2)
            }
        }
        
        points(matrice_colturi[, 2],
               matrice_colturi[, 3],
               pch = 20,
               cex = 2,
               col = "orange")
        
        
    }) # initPlot's renderPlot
    
    # extendPlot
    
    output$extendPlot <- renderPlot({
        matrice_colturi      <- all.list()[[1]]
        ordine_colturi  <- all.list()[[2]]
        coordonate_puncte    <- all.list()[[3]]
        
        # Triangle:EXTEND
        
        if (input$shape == "tri") {
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(0, 1),
                ylim = c(0, sqrt(3) / 2),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
        }
        
        # Square:EXTEND
        
        if (input$shape == "sqr") {
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(0, 1),
                ylim = c(0, 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
        }
        
        # Pentagon:EXTEND
        
        if (input$shape == "pent") {
            
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(-0.25 * (sqrt(10 + 2 * sqrt(5))), 0.25 * (sqrt(10 + 2 * sqrt(5)))),
                ylim = c(-0.25 * (sqrt(5) + 1), 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
        }
        
        # Hexagon:EXTEND
        
        if (input$shape == "hex") {
            
            par(mar = c(0.5, 0.5, 0.5, 0.5))
            plot(
                0,
                0,
                xlim = c(-sqrt(3) / 2, sqrt(3) / 2),
                ylim = c(-1, 1),
                col = 0,
                yaxt = "n",
                xaxt = "n",
                xlab = "",
                ylab = "",
                bty = "n"
            )
        }
        
        if (!is.null(input$extend)) {
            if (input$extend != 0) {
                points(
                    coordonate_puncte[1:input$extend, 1],
                    coordonate_puncte[1:input$extend, 2],
                    pch = 20,
                    cex = 1,
                    col = "black"
                )
            }
        }
        
        points(matrice_colturi[, 2],
               matrice_colturi[, 3],
               pch = 20,
               cex = 2,
               col = "orange")
        
        
    })
    
})