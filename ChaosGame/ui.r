if (!require("shiny"))
    install.packages("shiny")

library(shiny)

shinyUI(
    fluidPage(
        style = "width: 100%; display: flex; flex-direction : column; align-items : center",
        HTML("<style>p { font-size: 18px }</style>"),
        fluidRow(
            h1("CHAOS GAME", style = "color: #1E1BE2; font-weight: 600; margin-top: 30px")   
        ),
        fluidRow(
            style = "width: 80%; min-width: 940px; max-width: 1200px; margin-top: 50px; margin-bottom: 30px",
            h2("Simulare", style = "text-decoration: underline"),
            HTML(
                "<p>&emsp;Pentru a începe o simulare apăsați pe butonul <b>START</b>. Înainte 
                de a face asta, puteți alege un <b>poligon</b> de la care să pornească simularea, de exemplu triunghiul sau 
                pătratul. Puteți seta unde este adăugat un punct între punctul curent și cel de interes, puteți seta 
                numărul de puncte generate într-un pas și timpul de așteptare între pași. Când este setat la <b>un singur 
                punct într-un pas,</b> va afișa constant punctul curent și direcția în care va fi generat următorul punct. 
                Butonul <b>GENEREAZĂ</b> va adăuga un punct cu coordonate alese aleator de la care să înceapă simularea. 
                Când un parametru este modificat, simularea se va opri și va reveni la starea inițială.</p>",
            )
        ),
        fluidRow(
            style = "margin-top: 30px",
            column(
                4,
                wellPanel(
                    
                    selectizeInput(
                        'shape',
                        label = "",
                        choices = list(
                            'Triunghi' = 'tri',
                            'Pătrat' = 'sqr',
                            'Pentagon' = 'pent',
                            'Hexagon' = 'hex'
                        ),
                        selected = 'tri'
                    ),
                    
                    conditionalPanel(
                        condition = " input.shape == 'tri' ",
                        sliderInput(
                            "dist.tri",
                            label = "Raportul distanței până la punctul de interes:",
                            min = 0.01,
                            max = 0.99,
                            step = 0.01,
                            value = 0.5
                        ),
                        p("Valoarea implicită este de 0.5", 
                          style="margin-top: -15px; margin-bottom: 15px; text-align: center; font-size: 13px"
                        )
                    ),
                    
                    conditionalPanel(
                        condition = " input.shape == 'sqr' ",
                        sliderInput(
                            "dist.sqr",
                            label = "Raportul distanței până la punctul de interes:",
                            min = 0.01,
                            max = 0.99,
                            step = 0.01,
                            value = 0.67
                        ),
                        p("Valoarea implicită este de 0.67", 
                          style="margin-top: -15px; margin-bottom: 15px; text-align: center; font-size: 13px"
                        )
                    ),
                    
                    conditionalPanel(
                        condition = " input.shape == 'pent' ",
                        sliderInput(
                            "dist.pent",
                            label = "Raportul distanței până la punctul de interes:",
                            min = 0.01,
                            max = 0.99,
                            step = 0.01,
                            value = 0.63
                        ),
                        p("Valoarea implicită este de 0.63", 
                          style="margin-top: -15px; margin-bottom: 15px; text-align: center; font-size: 13px"
                        )
                    ),
                    
                    conditionalPanel(
                        condition = " input.shape == 'hex' ",
                        sliderInput(
                            "dist.hex",
                            label = "Raportul distanței până la punctul de interes:",
                            min = 0.01,
                            max = 0.99,
                            step = 0.01,
                            value = 0.67
                        ),
                        p("Valoarea implicită este de 0.67", 
                          style="margin-top: -15px; margin-bottom: 15px; text-align: center; font-size: 13px"
                        )
                    ),
                    
                    sliderInput(
                        "steps",
                        label = "Puncte generate într-un singur pas:",
                        min = 1, 
                        max = 100,
                        step= 1,
                        value = 1
                    ),
                    
                    sliderInput(
                        "speed",
                        label = "Timp intre pași (in ms):",
                        min = 10, 
                        max = 1000, 
                        step = 10,
                        value = 100
                    ),
                    
                    conditionalPanel(
                        condition = "input.steps == 1",
                        uiOutput("my.init")
                    ),
                    
                    conditionalPanel(
                        condition = "input.steps > 1",
                        uiOutput("my.extend")
                    ),
                    
                    div(
                        actionButton(
                            inputId = "gen",
                            label = "GENEREAZĂ",
                            style = "background-color: #6699ff; width: 120px; color: white"
                        ),
                        style = "margin-top: 10px; display: flex; justify-content: center;"
                    )
                    
                ) #wellPanel
            ), #column
            column(
                8,
                style = "width: 610px; border: thick double #32a1ce",
                div(
                    conditionalPanel(
                        condition = "input.steps == 1",
                        plotOutput("initPlot", height = "610px")
                    ),
                    conditionalPanel(
                        condition = "input.steps > 1",
                        plotOutput("extendPlot", height = "610px")
                    )
                )
            ) #column
        ), #fluidRow
        fluidRow(
            style = "width: 80%; min-width: 940px; max-width: 1200px; margin-top: 50px; margin-bottom: 20px",
            h2("Algoritm", style = "text-decoration: underline"),
            HTML(
                "<p>&emsp;<b>Chaos Game</b>(Jocul Haosului) este o metodă de a genera <b>fractali</b>, folosind un poligon
                și un punct ales aleator. Fractalul este creat prin generarea repetitivă de puncte după anumite reguli.
                Este ales aleator un vârf al poligonului și se adaugă un punct între acest vârf și punctul curent 
                (care pentru început este cel generat aleator). Punctul este adăugat în funcție de un raport dat, 
                care rămâne constant pentru toate generările. După ce un punct este creat, acela devine punctul curent
                și se repetă procesul. Un exemplu ar fi pentru triunghi, cu un raport de 1/2, acest algoritm creează 
                <b>triunghiul lui Sierpinski.</b></p>",
            )
        ),
        fluidRow(
            tags$img(height = 300, width = 300,src = "Sierpinski.png"),
            p("Triunghiul lui Sierpinski", style = "text-align: center")
        ),
        fluidRow(
            style = "width: 80%; min-width: 940px; max-width: 1200px; margin-top: 50px; margin-bottom: 300px",
            h2("Fractali", style = "text-decoration: underline"),
            HTML(
                "<p>&emsp;Un <b>fractal</b> este o figură geometrică care poate fi divizată în părți, astfel încât fiecare
                dintre acestea să fie o copie miniaturală a întregului. Deoarece par identici la orice nivel de magnificare, 
                fractalii sunt considerați ca fiind infinit complecși. Alte exemple simple de fractali sunt covorul lui
                Sierpinski, buretele lui Menger si curba dragon.</p>"
            )
        )
    ) #fluidPage
) #shinyUI