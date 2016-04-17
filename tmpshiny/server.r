library(ggplot2)

df <- iris

shinyServer( function(input, output, session) {

    output$xvar <- renderUI({
        selectInput("xvar", label = "X variable", choices = names(df),
                    selected = names(df)[1])
    })
    output$xtrans <- renderUI({
        selectInput("xtrans", label = "X transformation", choices = c('None', 'log10', 'sqrt'),
                    selected = 'None')
    })
    output$yvar <- renderUI({
        selectInput("yvar", label = "Y variable", choices = names(df),
                    selected = names(df)[2])
    })
    output$ytrans <- renderUI({
        selectInput("ytrans", label = "Y transformation", choices = c('None', 'log10', 'sqrt'),
                    selected = 'None')
    })
    output$groupvar <- renderUI({
        selectInput("groupvar", label = "Group variable", choices = c(names(df), '<None>'),
                    selected = names(df)[3])
    })
    output$panelvar <- renderUI({
        selectInput("panelvar", label = "Panel variable", choices = c(names(df), '<None>'),
                    selected = names(df)[4])
    })

    plotdata <- reactive({
        if (!is.null(input$xvar)) 
            df$tmpX <- df[[input$xvar]]
        if (!is.null(input$yvar))
            df$tmpY <- df[[input$yvar]]
        if (!is.null(input$groupvar))
            if (input$groupvar != '<None>')
                df$tmpGroup <- df[[input$groupvar]]  else  df$tmpGroup <- NA
        if (!is.null(input$panelvar))
            if (input$panelvar != '<None>')
                df$tmpPanel <- df[[input$panelvar]]  else  df$tmpPanel <- NA
        return(df)
    })
    
    output$theplot <- renderPlot({
        dat <- plotdata()
        if ('tmpX' %in% names(dat) & 'tmpY' %in% names(dat) & 'tmpGroup' %in% names(dat) &
            'tmpPanel' %in% names(dat)) {
            if (input$groupvar != '<None>') {
                g <- ggplot(dat, aes(x = tmpX, y = tmpY, group = tmpGroup, colour = tmpGroup))
            } else g <- ggplot(dat, aes(x = tmpX, y = tmpY))

            g <- g + geom_point(alpha = 0.5)

            if (input$panelvar != '<None>')
                g <- g + facet_wrap(~ tmpPanel)

            if (input$xtrans == 'log10')
                g <- g + scale_x_log10()
            if (input$ytrans == 'log10')
                g <- g + scale_y_log10()

            if (input$xtrans == 'sqrt')
                g <- g + scale_x_sqrt()
            if (input$ytrans == 'sqrt')
                g <- g + scale_y_sqrt()
            
            g <- g + labs(x = input$xvar, y = input$yvar)
            
            return(g)
        }
    }
    )
}
)
