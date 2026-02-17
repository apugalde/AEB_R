#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)
library(flexdashboard)
library(shinythemes)
require(pracma)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Generate normal density data for plotting
  den.d <- reactive({
    xrange <- range(input$h.m, input$d.m)
    max.sd <- max(input$h.sd,input$d.sd)
    x <- seq(xrange[1] - 3*max.sd,xrange[2] + 3*max.sd,0.1)
    list(
      'x' = x,
      'h.y' = dnorm(x,input$h.m,input$h.sd)*(100-input$prev),
      'd.y' = dnorm(x,input$d.m,input$d.sd)*input$prev,
      'rng' = range(x),
      'h.ir' = qnorm(c(0.025,0.975),input$h.m,input$h.sd)
    )
  })
  
  # Generate data for histogram plotting
  hist.d <- reactive({
    n <- 10000
    p <- input$prev/100
    h.vals <- rnorm(n*(1-p),input$h.m,input$h.sd)
    d.vals <- rnorm(n*p,input$d.m,input$d.sd)
    bks <- hist(c(h.vals,d.vals),70,plot=F)$breaks
    list(
      'h.vals' = h.vals,
      'd.vals' = d.vals,
      'bks' = bks,
      'rng' = range(bks),
      'h.ir' = quantile(h.vals, c(0.025,0.975))
    )
  })
  
  # Calculate sen and spec
  sen.d <- reactive({
    if (input$tab == 'Parametric') {
      sen <- 1-pnorm(input$p.c,input$d.m,input$d.sd)
      spec <- pnorm(input$p.c,input$h.m,input$h.sd)
      } else {
      sen <- sum(hist.d()$d.vals >= input$p.c)/length(hist.d()$d.vals)
      spec <- sum(hist.d()$h.vals < input$p.c)/length(hist.d()$h.vals) 
      }
    if (input$h.m > input$d.m){
      sen <- 1-sen
      spec <- 1-spec
    }
    list(
      'sen'=sen,
      'spec'=spec
    )
  })
  
  # Calculate ROC curve (parametric)
  roc.p <- reactive({
    roc.bks <- den.d()$x
    roc.sen <- 1-pnorm(roc.bks,input$d.m,input$d.sd)
    roc.1esp <- 1-pnorm(roc.bks,input$h.m,input$h.sd)
    if (input$h.m <= input$d.m){
      roc.sen <- rev(roc.sen)
      roc.1esp <- rev(roc.1esp)
      roc.bks <- rev(roc.bks)
    } else {
      roc.sen <- 1-roc.sen
      roc.1esp <- 1-roc.1esp
    }
    list(
      'roc.bks' = roc.bks,
      'roc.sen' = roc.sen,
      'roc.1esp' = roc.1esp 
    )
  })
  
  # Calculate ROC curve (non-parametric)
  roc.np <- reactive({
    pdata <- hist.d()
    roc.bks <- seq(pdata$rng[1],pdata$rng[2],0.1)
    roc.sen <- sapply(roc.bks, function(x) sum(pdata$d.vals >= x))/length(pdata$d.vals)
    roc.1esp <- sapply(roc.bks, function(x) sum(pdata$h.vals >= x))/length(pdata$h.vals)
    # Reverse if health > disease
    if (input$h.m <= input$d.m){
      roc.sen <- rev(roc.sen)
      roc.1esp <- rev(roc.1esp)
      roc.bks <- rev(roc.bks)
    } else {
      roc.sen <- 1-roc.sen
      roc.1esp <- 1-roc.1esp
    }
    list(
      'roc.bks' = roc.bks,
      'roc.sen' = roc.sen,
      'roc.1esp' = roc.1esp 
    )
  })
  
  
  # Update threshold point range
  observeEvent(den.d(),{
    updateSliderInput(inputId = 'p.c',step=0.1,
                      value=input$p.c,
                      min = den.d()$rng[1],
                      max= den.d()$rng[2])
  }
  )
  
  # Gauges
  output$Sen.g <- renderGauge({
    gauge(sen.d()$sen*100,min=0,max=100,
          sectors=gaugeSectors(c(70,100),c(40,70),c(0,40)))
  })
  
  output$Esp.g <- renderGauge({
    gauge(sen.d()$spec*100,min=0,max=100,
          sectors=gaugeSectors(c(70,100),c(40,70),c(0,40)))
  })
  
  output$Efi.g <- renderGauge({
    efi <- (sen.d()$sen + sen.d()$spec)*100/2
    gauge(efi,min=0,max=100,
          sectors=gaugeSectors(c(70,100),c(40,70),c(0,40)))
  })
  
  output$ppv.g <- renderGauge({
    p <- input$prev/100
    vals <- sen.d()
    ppv <-  (vals$sen*p) / (( (1-p)*(1-vals$spec) ) + (vals$sen*p))
    gauge(ppv*100,min=0,max=100,
          sectors=gaugeSectors(c(70,100),c(40,70),c(0,40)))
  }) 
  
  output$npv.g <- renderGauge({
    p <- input$prev/100
    vals <- sen.d()
    npv <-  (vals$spec*(1-p)) / (( (1-p)*vals$spec)  + ((1-vals$sen)*p))
    gauge(npv*100,min=0,max=100,
          sectors=gaugeSectors(c(70,100),c(40,70),c(0,40)))
  }) 
  
  # Plots for tab parametric
  observeEvent(
    {input$tab == 'parametric'},
    {
      output$distPlot <- renderPlotly({
        pdata <- den.d()
        plot_ly(height = 300,width = 300) %>%
          add_trace(x=pdata$x,y=pdata$h.y, type='scatter',mode='lines',fill='tozeroy',name='Sanos') %>%
          add_trace(x=pdata$x,y=pdata$d.y, type='scatter',mode='lines',fill='tozeroy',name='Enfermos') %>%
          add_segments(x=input$p.c,xend=input$p.c,y=0,yend=max(c(pdata$h.y,pdata$d.y)),showlegend = FALSE,
                       line=list(width=2,dash='solid',color='red'))%>%
          add_segments(x=pdata$h.ir,xend=pdata$h.ir,y=c(0,0),yend=rep(max(c(pdata$h.y,pdata$d.y)),2),showlegend = FALSE,
                       line=list(width=2,dash='dot',color='#4785FF'))%>%
          layout(xaxis = list(title='Concentración'),
                 yaxis= list(title='Densidad de probabilidad'),
                 showlegend=F)
      })
      
      output$senPlot <- renderPlotly({
        pdata <- roc.p()
        # AUC
        roc.lab <- list(
          x=0.6,y=0.3,
          showarrow = F,
          text=sprintf("<b>Area bajo la curva\n%.2f<b>",trapz(pdata$roc.1esp,pdata$roc.sen)))
        # Nearest point to actual threshold
        roc.pt <- which.min(abs(pdata$roc.bks - input$p.c))
        roc.pt <- c(pdata$roc.1esp[roc.pt],pdata$roc.sen[roc.pt])
        # Plot
        plot_ly(height = 300,width = 300) %>%
          add_trace(x=pdata$roc.1esp,y=pdata$roc.sen,type='scatter',mode='lines',
                    fill='tozeroy', fillcolor='#F2F3F5',line=list(shape='hv'))%>%
          add_markers(x=roc.pt[1],y=roc.pt[2],type='scatter',mode='marker',
                      marker=list(color='red',size=8, symbol='x-dot'))%>%
          layout(annotations=roc.lab, showlegend=F,
                 xaxis = list(title='1-especificidad',
                              range = c(-0.05,1.05),
                              tickvals= list(0,0.25,0.5,0.75,1)),
                 yaxis = list(title ='Sensibilidad',
                              scaleanchor = 'x',
                              scaleratio = 1,
                              range = c(-0.05,1.05),
                              tickvals= list(0,0.25,0.5,0.75,1)))
      })
    })  
  
  # Plots for non-parametric
  observeEvent(
    {input$tab == 'NonParametric'},
    {
      output$distPlot2 <- renderPlotly({
        pdata <- hist.d()
        h.cts = hist(pdata$h.vals,pdata$bks)$counts
        d.cts = hist(pdata$d.vals,pdata$bks)$counts
        # Plot (height = 250)
        plot_ly(height = 300,width = 300,line=list(shape='vh'))%>%
          add_trace(x=pdata$bks,y=c(h.cts,tail(h.cts,1)), name='Sanos',type='scatter',mode='lines',fill='tozeroy') %>%
          add_trace(x=pdata$bks,y=c(d.cts,tail(d.cts,1)),name='Enfermos',type='scatter',mode='lines',fill='tozeroy') %>%
          add_segments(x=input$p.c,xend=input$p.c,y=0,yend=max(c(h.cts,d.cts)),showlegend = FALSE,
                       line=list(width=2,dash='solid',color='red'))%>%
          add_segments(x=pdata$h.ir,xend=pdata$h.ir,y=c(0,0),yend=rep(max(c(h.cts,d.cts)),2),showlegend = FALSE,
                       line=list(width=2,dash='dot',color='#4785FF'))%>%
          layout(xaxis = list(title='Concentración'),
                 yaxis= list(title='Frecuencia'),
                 showlegend=F)
      })
      
      output$senPlot2 <- renderPlotly({
        pdata <- roc.np()
        # AUC
        roc.lab <- list(
          x=0.6,y=0.3,
          showarrow = F,
          text=sprintf("<b>Area bajo la curva\n%.2f<b>",trapz(pdata$roc.1esp,pdata$roc.sen)))
        roc.pt <- which.min(abs(pdata$roc.bks - input$p.c))
        roc.pt <- c(pdata$roc.1esp[roc.pt],pdata$roc.sen[roc.pt])
        # Plotting
        plot_ly(height = 300,width = 300) %>%
          add_trace(x=pdata$roc.1esp,y=pdata$roc.sen,type='scatter',mode='lines',
                    fill='tozeroy', fillcolor='#F2F3F5',line=list(shape='hv'))%>%
          add_markers(x=roc.pt[1],y=roc.pt[2],type='scatter',mode='marker',
                      marker=list(color='red',size=8, symbol='x-dot'))%>%
          layout(annotations=roc.lab, showlegend=F,
                 xaxis = list(title='1-especificidad',
                              range = c(-0.05,1.05)),
                 yaxis = list(title ='Sensibilidad',
                              scaleanchor = 'x',
                              scaleratio = 1,
                              range = c(-0.05,1.05)))
      })
    })
  
  output$mytab <- renderTable({
    if (input$tab == 'NonParametric'){
      pdata <- hist.d()
      vp <- sum(pdata$d.vals >= input$p.c)
      vn <- sum(pdata$h.vals < input$p.c)
      ma <- matrix(c(vn, length(pdata$d.vals)-vp,length(pdata$h.vals)-vn, vp),
                   ncol=2)
      } else {
      ma <- matrix(c('VN','FN','FP','VP'),ncol=2)    
      }
    dimnames(ma) <- list(c('Sano','Enfermo'),c('Negativo','Positivo'))
    ma
  },rownames=T)
}
