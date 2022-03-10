# INTRODUCTION
intro_tab <- tabPanel(
  "Introduction",
  p(paste('In this app, we are building an interactive disease modeling simulation',
    'of COVID-19 spread. We are offering users with different widgets to dynamically',
    'change settings built into the models and compare the different outputs')),
  p(paste('Additionally, we will explore two different models, which include deterministic',
          'model and stochastic individual contact model. The following explains each model in more detail:')),
  br(),
  tags$ol(
    tags$li(strong('Deterministic, Compartmental Model (DCM)'),
            br(),
            p(paste("Deterministic compartmental models solve differential equations representing analytic",
                    "epidemic systems in continuous time. This model is called"),
                    em('deterministic'), paste('because the results are fixed functions based on the',
                    'input parameters and initial conditions, without any randomness, stochastic variability introduced in the process.',
                    'Furthermore, the term'), em('compartmental'), paste('here refers to grouping across',
                    'different traits and states in the disease.')),
            p(paste('In DCM, there are several cases that we can consider, namely Susceptible-Infected (SI) disease,',
                    'Susceptible-Infectious-Recovered (SIR), and Susceptible-Infectious-Susceptible (SIS) scenario. The differences between these three scenarios',
                    'can be interpreted and analyzed using different processes which are an addition to one another. For instance, SIR model',
                    'uses the basic SI model with an addition of recovery process which introduces another differential equation to solve. The similar thing goes', 
                    'with SIS model.'))
            ),
    tags$li(strong('Stochastic Individual Contact Model (ICM)'),
            br(),
            p(paste('Stochastic ICMs are essentially the agent-based microsimulation analogs to the DCMs. This means that',
                    'parameters are randomly drawn from distributions summarized by the given rates or probabilities, including Normal, Poisson, and Binomial distributions.',
                    'Additionally, ICMs are also simulated in discrete time unlike DCMs. Thus, within each time step,',
                    'everything happens as a series of processes rather than independent of one another. This, however, introduces',
                    'potential risks when time step used is large, which makes some transitions unable to be considered as independent process.',
                    'Units in ICMs are also presented as individual element rather than a whole group of population.')),
            p(paste('Furthermore, ICM uses very similar inputs like DCMs which are discussed below Thus, with the'),
            em('EpiModel,'), paste('we will be able to compare and contrast both models and visualize the different outputs to observe different insights.'))
            )
  ),
  br(),
  p(paste('For our analysis and visualization, we will be implementing the'), em('dcm()'),
    paste('function coming from the'), em('EpiModel'),
    paste('package. More information regarding the function and the model built within the package',
          'can be found'),
    a(href = 'http://statnet.org/tut/BasicDCMs.html',
      "here"), paste('for DCM and'),
    a(href = "http://statnet.org/tut/BasicICMs.html", 'here'), paste('for ICM.'),
    paste('The following arguments are what is used and crucial for our modeling throughout this app:')),
  tags$ul(
    tags$li(strong('param: '), paste('Epidemic model parameters to be used. This includes',
                                    'the transmission probability per act and the acts per person per unit time')),
    tags$li(strong('init: '), paste('Initial conditions for the model, including initial number susceptible and infected at time 1 (first observation)')),
    tags$li(strong('control: '), paste('Other structural model controls like model type and number of time steps for simulation'))
  ),
  br(),
  tags$img(class = 'image',
           src = 'https://www.clevelandclinic.org/healthinfo/ShowImage.ashx?PIC=4480',
           alt = 'covid-19',
           width = 500,
           height = 325,
           style = 'align::middle'),
  tags$footer(('___'))
)

dcm_tab <- tabPanel(
  'Deterministic Model',
  p(paste('This tab simulates various outputs of Deterministic Model from'),
    em('EpiModel'),
    paste('package using different set of inputs, controls, and initial conditions. Based on the'),
    a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7270519/", 'research on COVID-19 disease,'),
    paste('SIR model will be the most relevant mathematical models to the spread of the pandemic. As a result,',
          'we will implement this specific model among the other 2 mentioned in the introduction page.')),
  p('Here, we are allowing users to control the following metrics:'),
  tags$ul(
    tags$li(strong('Policy Strictness Rating: '),
            paste('This metric ranges from 0 to 10 with 0 indicating no strictness and 10 indicating high strictness',
                  'on policies, masks mandate, and social distancing. Changing this',
                  'value will enforce different probabilities of infection per transmissible act between',
                  'a susceptible and an infected person.'),
            a(href = 'https://journals.sagepub.com/doi/10.1177/0272989X211013306', '[Reference]')),
    tags$li(strong('Symptoms: '),
           paste('This metric presents two choices of symptoms that a group of infected individuals',
                 'are experiencing. This includes asymptomatic and symptomatic infection, which affects the',
                 'average rate of recovery used in the model'),
           a(href = 'https://www.nature.com/articles/s41592-020-0856-2', '[Reference]'))
  ),
  sidebarLayout(
    sidebarPanel(
      # first widget
      sliderInput('strictness',
                  label = 'Policy Strictness Rating',
                  min = 0,
                  max = 10,
                  value = 10,
                  step = 1),
      checkboxInput(inputId = 'symptomatic',
                    label = 'Symptomatic Infection?',
                    value = TRUE)
    ),
    mainPanel(plotOutput('dcm'))
  )
)

icm_tab <- tabPanel(
  'Stochastic Model',
  p(paste('Similar to DCM, this tab will use ICM to introduce',
          'random draws to rates and risks. Since our goal is to compare the',
          'results to DCM, we will use the same model type and metrics offered to control:')),
  tags$ul(
    tags$li(strong('Policy Strictness Rating: '),
            paste('This metric ranges from 0 to 10 with 0 indicating no strictness and 10 indicating high strictness',
                  'on policies, masks mandate, and social distancing. Changing this',
                  'value will enforce different probabilities of infection per transmissible act between',
                  'a susceptible and an infected person.'),
            a(href = 'https://journals.sagepub.com/doi/10.1177/0272989X211013306', '[Reference]')),
    tags$li(strong('Symptoms: '),
            paste('This metric presents two choices of symptoms that a group of infected individuals',
                  'are experiencing. This includes asymptomatic and symptomatic infection, which affects the',
                  'average rate of recovery used in the model'),
            a(href = 'https://www.nature.com/articles/s41592-020-0856-2', '[Reference]'))
  ),
  sidebarLayout(
    sidebarPanel(
      # first widget
      sliderInput('strictness_icm',
                  label = 'Policy Strictness Rating',
                  min = 0,
                  max = 10,
                  value = 10,
                  step = 1),
      checkboxInput(inputId = 'symptomatic_icm',
                    label = 'Symptomatic Infection?',
                    value = TRUE)
    ),
    mainPanel(plotOutput('icm'))
  )
)

summary_tab <- tabPanel(
  'Interpretation',
  p(paste('In this specific model comparison, we offered two widgets to contol the strictness of policy and',
          'whether a certain group has symptomatic infection. Based on the research done related to the first,',
          'we assumed that for each step of strictness, it follows a reduction or increase in the probability of infection',
          'by 9%. Additionally, for the second parameter, depending on whether a certain group has symptomatic infection,',
          'the recovery rate from COVID-19 varies from 2% for symptomatic to 14% for asymptomatic influence, as referenced'),
          a(href = 'https://www.cdc.gov/coronavirus/2019-ncov/hcp/duration-isolation.html', 'here.')),
  p(paste('Additionally, we use a sample of 999 individuals with 365 time steps to represent 1 year of COVID-19 pandemic. As discussed',
          'in the DCM tab, we are implementing the SIR model to predict the number of individuals',
          'who are susceptible to infection, are actively infected, or have recovered from infection at given time.',
          'This model is suitable and easy-to-use because it fits with the pandemic situation and we can approximate disease behavior by',
          'estimating a small number of parameters. However, due to its simplicity, there are some associated limitations which will be discussed in later section.')),
  h2('Deterministic Model (DCM)'),
  p(paste('From the DCM tab, we can observe that for a specific group of individuals where policy strictness is at 10,',
          'The number of infected and recovered individuals are less than number of susceptible individuals. When we control the widget in DCM tab to 0,',
          'we observe that number of individuals infected and recovered are now way larger than number of susceptible individuals. These scenarios totally make sense to me because',
          'the rate of infection or spread of COVID-19 disease depends on the strictness of policy.')),
  p(paste('Additionally, we can also control the widget in the DCM tab and see how the rate changes when infected group has asymptomatic or symptomatic infection.',
          'It is notable that with asymptomatic infections, the rate of COVID-19 spreads will be way lower because most of the infected individuals',
          'do not experience severe effects and thus, spreads rate is way less compared to symptomatic scenario.')),
  fluidRow(plotOutput('dcm_summary')),
  br(),
  h2('Stochastic Model (ICM)'),
  p(paste('Similarly to DCM, ICM produces very similar result. However, with the introduction of randomness in the rates',
          'and risks, we can observe from the summary below (make sure that parameters in both widgets from ICM and DCM tab are the same),',
          'the rate of COVID-19 infected, susceptible, and recovered individuals might more fluctuate. As a result, this makes the output variance',
          'to be large depending on the parameters set by the widget and also the randomness introduced to the model at different run.')),
  fluidRow(plotOutput('icm_summary')),
  br(),
  h2('Limitation'),
  p(paste('Using SIR model to fit COVID-19 disease spread is simple and easy to compute. However,',
          'this may oversimplify complex disease processes. One of the examples given from this'),
    a(href = 'https://jamanetwork.com/journals/jama/fullarticle/2766672', 'article'),
    paste('is that SIR may not incorporate the latent period when an individual is exposed to the disease.',
          'This is applicable to the COVID-19 disease because the spread of the disease and the realization of an individual',
          'having the symptoms vary across times. As a result, we may want to consider different models',
          'such as the SEIR model (with E denoting exposed but not yet contagious) to account for this time-dependent',
          'strategies.')),
  br(),
  p('In addition, our models also do not formally quantify uncertainty. For instance,',
    'the policy strictness rank from 0 to 10 and the time steps are point estimates',
    'which are single values that reflect the model guess.',
    'Without any precision, i.e. to how extent does a specific parameter is actually effective or infectious,',
    'our model will demonstrate uncertainty in projections. In the future, we are able to use parameter values from',
    'real, incoming data from different official websites to better provide this certainty in metrics used.')
)

ui <- navbarPage(theme = 'style.css',
  title = 'COVID-19 Spread Disease Modeling',
  intro_tab,
  dcm_tab,
  icm_tab,
  summary_tab
)