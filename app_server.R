server <- function(input, output) {
  output$dcm <- renderPlot({
    param <- param.dcm(inf.prob = ((99 - 9*(input$strictness)) / 100),
                       rec.rate = ifelse(input$symptomatic, 0.02, 0.14))
    init <- init.dcm(s.num = 999, i.num = 1, r.num = 0)
    control <- control.dcm(type = 'SIR', nsteps = 365, dt = 1)
    
    mod <- dcm(param, init, control)
    
    plt <- plot(mod) + title('Number of Susceptible, Infected, and Recovered Individuals in Different Scenarios using DCM')
    return(plt)
  })
  
  output$dcm_summary <- renderPlot({
    param <- param.dcm(inf.prob = ((99 - 9*(input$strictness)) / 100),
                       rec.rate = ifelse(input$symptomatic, 0.02, 0.14))
    init <- init.dcm(s.num = 999, i.num = 1, r.num = 0)
    control <- control.dcm(type = 'SIR', nsteps = 365, dt = 1)
    
    mod <- dcm(param, init, control)
    
    plt <- comp_plot(mod, at = 50)
    
    return(plt)
  })
  
  output$icm <- renderPlot({
    param <- param.icm(inf.prob = ((99 - 9*(input$strictness_icm)) / 100),
                       rec.rate = ifelse(input$symptomatic_icm, 0.02, 0.14))
    init <- init.icm(s.num = 999, i.num = 1, r.num = 0)
    control <- control.icm(type = 'SIR', nsteps = 365, dt = 1)
    
    mod <- icm(param, init, control)
    
    plt <- plot(mod, main = 'Number of Susceptible, Infected, and Recovered Individuals in Different Scenarios using ICM')
    return(plt)
  })
  
  output$icm_summary <- renderPlot({
    param <- param.icm(inf.prob = ((99 - 9*(input$strictness_icm)) / 100),
                       rec.rate = ifelse(input$symptomatic_icm, 0.02, 0.14))
    init <- init.icm(s.num = 999, i.num = 1, r.num = 0)
    control <- control.icm(type = 'SIR', nsteps = 365, dt = 1)
    
    mod <- icm(param, init, control)
    
    plt <- comp_plot(mod, at = 50)
    
    return(plt)
  })
}