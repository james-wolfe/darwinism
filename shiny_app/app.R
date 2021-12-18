library(shiny)
library(haven)
library(tidyverse)
library(shinythemes)
library(ggrepel)
library(rsconnect)

ui <- navbarPage(
  "Mathematics of Evolution",
  tabPanel("Introduction",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Project Introduction"),
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h3("The Mathematics of Evolution"),
  
                                     p("This project both seeks to introduce some basic concepts in evolution through a mathematical
                                     lens and to introduce the role of mathematics in the formulation of the theory of evolution.
                                     Simple mathematical intuition was incredibly important for the formation of the theory,
                                     and mathematics today has been incredibly important still for novel 
                                     research in evolutionary biology. I will introduce some simple mathematical models of
                                     evolutionary processes (reproduction, selection, mutation), and give historical insight
                                     on some models and some key moments in the theory's history. I will end with a page on 
                                     the evolution of cooperation, something Darwin couldn't quite crack, and for which mathematics has
                                     proved useful lately.
                                       "),
                                     p("I will, for the sake of aesthetics and fun, use visuals, interactives,
                                     and animations to illustrate some insights. I spent most of my time coding these, and hopefully I will be rewarded
                                     with the reader's enjoyment of them.
                                       "))
                     ),
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h4("Quick note:"),
                                     p("I recommend viewing this project on a computer, full screen. 
                                       Additionally, some of the interactives are slow. I urge the reader to be patient.")
                              ))
                     )
           
  ),
  tabPanel("Beginnings",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Where it all started"),
                     fluidRow(style = 'padding:30px;',
                              column(width = 8,
                                     p("One could reasonably say the foundation of Darwin's theory came from the thought of
                                       an economist Thomas Robert Malthus in his paper 'Essay on the Principle
                                       of Population.' Malthus had a simple argument, mathematical in
                                       nature, which brought Darwin one big step closer to the theory of natural selection [2]."),
                                     p("Humans can theoretically reproduce at a geometric rate. Two parents could reasonably yield four children, and growth
                                       would look something like:"),
                                     withMathJax(),
                                     helpText('$$2,4,8,16,32,\\dots$$'),
                                     p("But in 100 generations, we'd have a population of size"),
                                     helpText('$$2^{100}\\approx 1.27\\times 10^{30}.$$'),
                                     p("We would be hard-pressed to find a population with this many humans.
                                       Malthus proposed there are checks on populations which deter
                                       them from actually growing at this rate: like early death, resource scarcity,
                                       and disease. There must be some overproduction of offspring, i.e. more humans are produced than can possibly
                                       survive and reproduce [4]. Otherwise, there would be a pile of humans up to the moon."),
                                     p("Darwin saw this argument could be applied to any other organism, and it is really the bedrock of the
                                     theory of natural selection. Why don't we see frogs everywhere we step, if they, too, could reproduce
                                     at an exponential rate? Darwin reasoned that
                                       there must be some struggle for survival given an overproduction of offspring, 
                                       and a key observation Darwin made upon reading Malthus's essay is that 
                                       the 'strongest' would survive, and the 'weakest' would be the ones to perish [2]."),
                                     p("Malthus's writing was incredibly prescient.
                                     Nowadays, we'd say populations have a carrying capacity, due to constraints of
                                       space and resources, for example. The acquisition of resources and space is more linear in nature,
                                       whereas population growth in an unchecked setting is exponential. These two events clash.
                                       The resources cannot keep up with population growth.
                                       Let us return to our geometric theory of reproduction, assuming two humans can produce four.
                                       If there were no checks, we'd have"),
                                     helpText("$$x_{t+1} = 2x_t,$$"),
                                     helpText("$$\\text{where } x_t := \\text{population size at generation }t. \\text{So,}$$"),
                                     helpText("$$x_t = x_02^t$$"),
                                     p("Let us create a model with a carrying capacity. Remember, the population cannot grow beyond the carrying
                                       capacity, and it would be reasonable to assume the population's growth slows down as it approaches the 
                                       carrying capacity. The following model is a reasonable satisfaction of these criteria:"),
                                     helpText("$$x_{t+1} = rx_t(1-x_t/K),$$"),
                                     helpText("$$\\text{or, if we assume } K = 1,$$"),
                                     helpText("$$x_{t+1} = rx_t(1-x_t),$$"),
                                     p("where r is the growth rate of the population and K is the carrying capacity of the population [6].
                                       This is known as the logistic difference equation. It is a famous equation in mathematical biology, discovered by
                                       the late renowned biologist Robert May, who brought it to mathematician James Yorke.
                                       In 1975, Tien-Yien Li and Yorke studied the equation in the now famous paper 'Period Three Implies Chaos' [10].
                                       The title references the notion of period in dynamical systems. If a point x has period three under some
                                       function f, then "),
                                     helpText("$$f(f(f(x))) = x, \\text{ and } f(x) \\ne x, f(f(x)) \\ne x$$"),
                                     p("(Here the function is the difference equation itself, i.e. f(x) = rx(1 - x).)
                                     The existence of a point with period 3 under a continuous function f implies the existence of points with
                                       arbitrary period size, which leads to chaotic behavior of the function [10]. This happens when 3.82 < r <= 4 for the 
                                       growth rate r [6]. (I urge the reader to try such r values in interactive below.) Robert May suggested, 
                                       and to my knowledge it has not been refuted, 
                                       that seemingly random noise when measuring population sizes may not be attributable to just 
                                       random environmental disturbances
                                       or measurement error, but rather it may be in large part attributable to population growth following 
                                       a mostly deterministic process [5]. 
                                       We started with Malthus's simple notion of a carrying capacity, and we
                                       have arrived at a stunning result, in only a few short steps (and almost 200 years later).
                                       "),
                                     p("Li and Yorke remind us that deterministic processes may lead to truly chaotic results. 
                                       The reader may use the following tool to visualize the growth of a population 
                                       according to this model. The chaotic and, frankly, confusing nature of the equation should become apparent
                                       if the reader plays around with enough inputs.")
                              ),
                              column(width = 1),
                              column(width = 3,
                                     h4("We don't have enough resources to have geometric rates of growth continue for long"),
                                     img(src = "malthus.png", height = 200),
                                     p("Image source: study.com"),
                                     p(" "),
                                     p(" "),
                                     p(" "),
                                     h4("Population growth with a carrying capacity"),
                                     p("Below is a modeled of the growth of several populations with a carrying 
                                     capacity. This is using a different model than the difference equation we look at. This model
                                     more or less assumes a population will reach and stay about at its carrying capacity, and
                                     it is stochastic. Oddly, it is more predictable than the difference equation.
                                     "),
                                     img(src = "animation.gif", height = 300))),
                     fluidRow(style = 'padding:30px;',
                              column(width = 9,
                                     h3("The Logistic Difference Equation"),
                                     h4("Modeling Population Growth"))),
                     fluidRow(style = 'padding:30px;',
                              column(width = 9,
                                     plotOutput("difPlot")),
                              column(width = 3,
                                     sliderInput("a", "Growth Rate (r)",
                                                 min = 0, max = 4, value = 3.10, step = 0.01
                                     ),
                                     sliderInput("x0", "Starting Population Abundance (x0)",
                                                 min = 0, max = 1, value = 0.04
                                     ))),
                     
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h3("Selection"),
                                     p("Let us now investigate the consequences of this struggle for survival. If
                                       indeed there is a finite number of organisms that can occupy an area, what
                                       determines who lives? Let's say a concrete number N of organisms reside in an area,
                                       and this number is fixed. Let us introduce the following model [6]. Suppose x is the
                                       frequency (population size divided by N) of type A in a population, and y is the frequency
                                       of type B. Then let us 
                                       define their growth rates as the following."),
                                     withMathJax(),
                                     helpText('$$\\frac{dx}{dt} = ax^c-\\phi x$$'),
                                     helpText('$$\\frac{dy}{dt} = by^c-\\phi y$$'),
                                     helpText('$$\\text{where } \\phi = ax^c + by^c \\text{ to ensure } x + y = 1.$$'),
                                     p("Since y = 1 - x, we can actually reduce this set of equations to"),
                                     helpText('$$\\frac{dx}{dt} = x(1-x)(ax^{c-1} - b(1-x)^{c-1}),$$'),
                                     p("which follows by straightforward computation.
                                     If the growth parameter c is equal to 1, then the growth rate of a type is a linear function of its
                                       frequency, and so the growth/decay is exponential. Unsurprisingly, if c > 1, the growth
                                       is superexponential, and if c < 1, the growth is subexponential."),
                                     p("This model has some weird, possibly counterintuitive properties. When c does not equal 1,
                                       we have the following fixed point between 0 and 1:"),
                                     helpText('$$x^* = \\frac{1}{1+(a/b)^{1/(c-1)}}.$$'),
                                     p("In fact, however, when growth is subexponential (c < 1), coexistence is virtually guaranteed.
                                       Each equilibrium x = 1 and y = 1 is unstable. However, this fixed point which lies between 0 and 1 is stable. Try, for example,
                                       a = 4, b = 1, c = 0.5, and watch x rapidly approach the fixed point 16/17. This is known as 'survival of all' [6]."),
                                     p("On the other hand, when c > 1, the fixed point x* in the set (0,1) is unstable, and the stable equilibria are 
                                       x = 0, x = 1. When x > x*, then A will outcompete B, whereas if x < x*, B will outcompete A. Therefore, A could be
                                       fitter than B, but if B starts in a sufficiently high proportion, B will outcompete A, and vice versa. This is known 
                                       as 'survival of the first' (as in, 'the type that established itself in an area first is favored to survive') [6]. To see this in action, try the
                                       inputs a = 4, b = 2, x0 = 0.33, c = 2 in the model below. And then try the same inputs with x0 = 0.34. It should not be difficult
                                       to guess the fixed point, although it can also be easily calculated,")
                                     
                                     
                                     
                                     )),
                     fluidRow(column(width = 8,
                                     h3("Modeling Selection"))),
                     fluidRow(style = 'padding:30px;',
                              column(width = 8,
                                     plotOutput("difPlot2")),
                              column(width = 1),
                              column(width = 3,
                                     sliderInput("a2", "Fitness of A (a)",
                                                 min = 0, max = 4, value = 2, step = 0.1
                                     ),
                                     sliderInput("b", "Fitness of B (b)",
                                                 min = 0, max = 4, value = 1.5, step = 0.1
                                     ),
                                     sliderInput("x02", "Initial Frequency of A (x0)",
                                                 min = 0, max = 1, value = 0.5, step = 0.01
                                     ),
                                     sliderInput("c", "Growth Parameter (c)",
                                                 min = 0, max = 2, value = 1, step = 0.01
                                     )))
           )
  ),
  tabPanel("Genetics",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Population Genetics"),
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h3("History and Background of Population Genetics"),
                                     p("Population genetics is the modern fusion of Darwinian natural selection
                                       and Mendelian genetics. Of course, our understanding of each has advanced since Darwin
                                       and Mendel."),
                                      p("We have looked at a model of selection, but what does
                                       this really tell us? If a type is selected for, what does that really mean?"),
                                       p("In essence, we assume a fitter phenotype is in large part due to a fitter genotype. 
                                       Then the fitter individual
                                       passes on their genes to the next generation. Eventually, these genes 'win' the Darwinian
                                       struggle [9].
                                       Thus, we often consider genes to be the vehicle for evolution [9]"),
                                     p("Darwin had a serious problem when he first introduced his theory, and in fact it was not
                                     resolved in his lifetime. He had no reasonable theory
                                       for how the inheritance of traits worked. The whole theory of natural selection, then, rested on 
                                       the (very fair) observation that offspring
                                       more or less resemble their parents. His inability to explain inheritance disturbed him, as it 
                                       did pose a serious threat to the theory [9].
                                       "),
                                     p("Blending inheritance was the thought of Darwin's day: that an offspring's phenotype 
                                       would be some combination of its parents' [9]. But this would rapidly reduce variation
                                       in a population, and variation is crucial for selection to occur. Acceptance 
                                       of the blending inheritance theory was a critical and fallacious assumption that the 
                                       objection to 
                                       Darwin's theory relied on. Mendel introduced
                                       what's now known as 'particulate' inheritance, whereby parents pass down discrete entities to 
                                       their offspring, and some combination of these discrete entities determines the phenotype.
                                       These discrete entities would not be lost in the process of continued reproduction, and 
                                       thus variation was preserved [9]."),
                                     p("Population genetics is traced to the 1920s and 1930s, when the work of R.A. Fisher,
                                       J.B.S. Haldane, and Sewall Wright was being produced. Despite the fact that the two ideas 
                                       today work in perfect harmony, Mendelians 
                                       and Darwinians were originally at odds: Mendelians couldn't accept the gradualist ideas
                                       of evolution, that changes were incredibly slow and happened over long periods of time [9].
                                       Mendelians understood a different version of inheritance, one that was more discrete. There is
                                       one type, and there is another, different type, and not a 
                                       continuum of types in the middle [9]. It was by the same logic, reversed, that Darwinians 
                                       could not accept Mendelian genetics."),
                                     p("Although Mendel did not have this vocabulary at the time, his basic idea was this. 
                                       There are two type of alleles: dominant and recessive. An individual has two alleles determining
                                       some trait. A dominant and a recessive allele together express the same external trait as two 
                                       dominant alleles, while two recessive alleles express a different trait. An individual passes down
                                       one allele (chosen randomly) to their offspring. There have been updates
                                       to this theory -- multiple genes may determine the expression of a trait, heterozygotes may not express 
                                       the same exact
                                       traits at dominant homozygotes -- but the basic idea remains intact [9]."),
                                     p("Fisher in particular made great strides to reconcile the two theories. He proposed this aforementioned
                                       idea that multiple discrete genes (again, they did not use this vocabulary at the time)
                                       may determine a trait in concert. If this was so, then the trait would basically follow a normal distribution
                                       among a population -- in other words, it would approximate a continuous spectrum [9], remaining 
                                       consistent with Mendel and Darwin's (main) ideas."),
                                     p("Today, population genetics, in spite of some critiques, is considered the cornerstone of modern 
                                       evolutionary biology [9].")
                                     )
                              ),
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h3("Describing Mutation, Selection, and Drift"),
                                     p("Math is integral to modern population genetics.
                                     We're going to look at some of its highly abstracted models (despite the fact that 
                                     some critiques of the field are that it is too abstract and theoretical [9]). We will strip some key concepts 
                                       down to their most basic versions, and we'll see what the math tells us."),
                                     h4("The Quasispecies Equation"),
                                     p("We won't dwell on the details too much, but let us briefly introduce the concept of a quasispecies.
                                       A quasispecies is a group of similar genomic sequences which mutate and which selection acts on [6].
                                       Suppose there are N different genomes in an infinitely large population, where"),
                                     withMathJax(),
                                     helpText('$$x_i := \\text{ the abundance of organisms with genome } i \\text{ in the population}$$'),
                                     helpText('$$f_i := \\text{ the fitness of genome } i$$'),
                                     helpText('$$q_{ij} := \\text{ the probability that replication of genome } i \\text{ results in genome } j.$$'),
                                     helpText('$$\\phi = \\sum_{i=1}^N x_if_i:= \\text{ the average fitness of the population}$$'),
                                     p("Then we have the quasispecies equation [6],"),
                                     withMathJax(),
                                     helpText('$$\\frac{dx_i}{dt} = \\sum_{j=1}^Nx_jf_jq_{ji} - \\phi x_i, \\text{   } i\\in\\{1,\\dots,N\\}$$'),
                                     p("For each i, we mulitply the current abundance of each type j, the fitness of j, and 
                                       the probability j replicates as i. Then, after we sum, we subtract by phi times the average fitness to maintain the same population size [6]."),
                                     p("An interesting consequence of this equation is that the fittest genome may not be the most adundant in equilibrium (this is what we see in the initial
                                     condition of the interactive plot below). 
                                     (Observe that, although this equation is meant to capture a stochastic process,
                                       it itself is a deterministic model.) I came up with values for the probability of mutation for each type to another, but the reader
                                       may still observe interesting patterns playing around with the fitness of each genome. Be sure that when you input, x1 + x2 + x3 <= 1.")
                              )
                     ),
                     fluidRow(style = 'padding:30px;',
                              column(width = 12,
                                     h3("The Quasispecies Equation in Action"),
                                     h4("The fittest genome doesn't always come out on top. Genome 4 rarely replicates as other genomes."))),
                     fluidRow(style = 'padding:30px;',
                              column(width = 7,
                                     plotOutput("quasispecies")
                                     ),
                              column(width = 1),
                              column(width = 2,
                                     sliderInput("f1", "Fitness of genome 1",
                                                 min = 0, max = 3, value = 1.18, step = 0.01
                                     ),
                                     sliderInput("f2", "Fitness of genome 2",
                                                 min = 0, max = 3, value = 1.07, step = 0.01
                                     ),
                                     sliderInput("f3", "Fitness of genome 3",
                                                 min = 0, max = 3, value = 1.06, step = 0.01
                                     ),
                                     sliderInput("f4", "Fitness of genome 4",
                                                 min = 0, max = 3, value = 1.15, step = 0.01
                                     )
                                     ),
                              column(width = 2,
                                     sliderInput("x10", "Init. abundance of genome 1",
                                                 min = 0, max = 1, value = 0.20, step = 0.01
                                     ),
                                     sliderInput("x20", "Init. abundance of genome 2",
                                                 min = 0, max = 1, value = 0.30, step = 0.01
                                     ),
                                     sliderInput("x30", "Init. abundance of genome 3",
                                                 min = 0, max = 1, value = 0.40, step = 0.01
                                     )
           )),
           fluidRow(style = 'padding:30px;',
                    column(width = 12,
                           h3("Drift and the Moran Process"),
                           p("Now let us move to a truly stochastic model. Suppose there are two types 
                           A and B with equal fitness in a finite population. We observe that in nature types will 
                           often fixate even with approximately equal fitness. Why?
                             Basically, because the population is finite. Let us take the simplest case,
                             and a hyperbolic example. In a population of size 2,
                             an individual of one type might get hit on the head with a cocunut, and the population
                             descends from the other individual immediately in the next generation. Stuff happens 
                             in finite populations. As the population
                             gets bigger, this becomes less likely (what are the odds 500 individuals of one type
                             will all get hit in the head with a coconut?). But the example need not be as extreme. It's possible
                             that by chance the organisms simply cannot reproduce for any number of reasons.
                             To model this, we will employ a model known as the Moran process [6]. It is simple. There
                             are N individuals in a population, where i are of type A and N - i are of type B. Each
                             round, we sample (with replacement) one individual to die and one to reproduce. The probability
                             an A individual will be chosen for either death or reproduction is i/N, and similarly with B the 
                             probability is (N-i)/N. When repeated for infinitely many rounds, the probability one type fixates
                             is 1. Eventually, in a finite population, one type comes out on top."),
                           p("We may do a similar process for arbitrarily many types. The probability of being chosen
                             for either death or reproduction is the population of the certain type divided by the total size of
                             the population. We see this process in the following interactive. Observe that the higher the population size,
                             the longer it takes to reach fixation."),
                           
                           
                           
                           )),
           fluidRow(style = 'padding:30px;',
                    column(width = 8,
                           h3("The Moran Process"))),
           fluidRow(style = 'padding:30px;',
                    column(width = 8,
                           plotOutput("moran")
                    ),
                    column(width = 1),
                    column(width = 3,
                           sliderInput("types", "Number of types",
                                       min = 1, max = 20, value = 10, step = 1
                           ),
                           sliderInput("N", "Number of each type in the population",
                                       min = 10, max = 50, value = 10, step = 10
                           ),
                        
                           sliderInput("reps", "Amount of time",
                                       min = 100, max = 10000, value = 1000, step = 100
                           )
                    )),
           fluidRow(style = 'padding:30px;',
                    column(width = 8)
                    ))),
  tabPanel("Games",
           fluidPage(theme = shinytheme("flatly"),
                     titlePanel("Why do we see cooperation in nature?"),
                     fluidRow(style = 'padding:30px;',
                              column(width = 11,
                                     p("Darwin briefly wrestled with this question in Chapter VII of 'The Origin of Species.'
                                       He considered the case of aphides, who voluntarily give up their honeydew to ants.
                                       But Darwin did not see this as an action which reduced the aphides' fitness; instead,
                                       he thought that the aphides no longer having their honeydew somehow helped them. 
                                       (In fact, aphides give up their honeydew in return for protection that ants provide.)
                                       His concession was that 'certain instincts cannot be considered as absolutely perfect' [3]."),
                                     p("Now, the evolution of cooperation is an active field of study. Before 1960, the field
                                       was not active, and cooperative acts were all chalked up to a theorized phenomenon known as 'group selection' [1],
                                       whereby natural selection acts on groups. However, 'group selection' theories have been largely tossed aside,
                                       and although specific versions of group selection theories have seen a reemergence lately, 
                                       their use as an explanation for cooperative
                                       behavior is still controversial [7]. A similar-sounding theory that differs in a crucial respect is known
                                       as 'kin selection,' which is much more widely accepted [7]. It is thought that if an organism cooperates (for our
                                       purposes, we take this to mean reducing one's fitness to increase another's) with their kin, they are still
                                       (consciously or not) increasing their shared genetic material's likelihood of survival [1]. But kin selection 
                                       cannot account for all cooperation. Another theory for a mechanism of cooperation, based on a game theoretic model
                                       and popularized by W.D. Hamilton and Robert Axelrod, recognizes that organisms often have repeated interactions,
                                       and don't know exactly how many times they'll interact in the future [1].
                                       The evolutionary advantage of the choice to free ride, then, is not as clear-cut. 
                                       They note that, in this case, always defecting (not cooperating) is still evolutionarily stable, in that if a population
                                       is always defecting, an individual trying any other strategy will not be able to get a higher 'payoff' in this interaction
                                       than the defectors get interacting with each other. But there are other evolutionarily stable strategies."),
                                     p("Axelrod and Hamilton focus on a class of strategies, known as reciprocal strategies, in the iterated prisoner's dilemma, which
                                     they use as a model for repeated biological interactions.
                                     Let us simply say the prisoner's dilemma is this: each player (there are two) can, in a given round,
                                     either incur a cost (c) to give the other player a benefit (b), or do nothing, where b > c > 0 (this is actually known as the donation game,
                                     a more specific version of the prisoner's dilemma).  The most famous of this class of reciprocal strategies, Tit-for-Tat,
                                       is simple: I cooperate at first. And then, whatever you do, I do unto you in our next interaction. They found that
                                       Tit-for-Tat outcompeted other strategies when all played each other in a simulated round-robin tournament, receiving the highest average payoff. It also
                                       was able to outcompete other strategies in an ecological version of this game where, after all strategies played each other, the strategies reproduced
                                       in proportion to their success. Tit-for-Tat reached fixation. If organisms are sufficiently likely to interact again, Tit-for-Tat is also evolutionarily 
                                       stable (without getting into the weeds, if the probability of interacting again is greater than c/b, then it's stable) [1].
                                       Although always defecting is evolutionarily stable, if a *couple* players playing Tit-for-Tat emerge, Tit-for-Tat can invade [1]."),
                                     p("So, this strategy of direct reciprocity is robust, in that it survives in an environment of lots of players doing many different things, it's stable (it can resist
                                     invasion by clusters of other strategies),
                                       and it can theoretically emerge in a population of defectors [1]. Now, direct reciprocity is accepted as a mechanism which
                                       has facilitated the evolution of cooperation [7]. Thankfully, for the theory, direct reciprocity is often a strategy we see employed in nature [1]."),
                                     p("So let's see this in action. You're going to repeatedly interact with me. I'm going to be playing a more forgiving version of Tit-for-Tat.
                                       You can set the probability you want to cooperate in a given round, given I cooperated/defected in the previous round.
                                       I include a line (arbitrarily) notating how well you need to do simply to survive. I am going to survive regardless, because I am playing with all my friends with the same strategy,
                                       and we all live blissfully in our cooperative bubble. There is a small probability of error, which may screw things up. Mistakes happen,
                                       and strategies should be able to account for them. You'll likely notice that the more cooperative you are, the better for your payoff.")
                                     
                                       )),
                     fluidRow(style = 'padding:30px;',
                              column(width = 5,
                                     h3("Enter the probability you'll cooperate 
                                        given the following event occurred in the previous round:"),
                                     sliderInput("p", "I cooperated",
                                                 min = 0, max = 1, value = 0.5
                                     ),
                                     sliderInput("q", "I defected",
                                                 min = 0, max = 1, value = 0.5
                                     ),
                              ),
                              column(width = 7,
                                     h4("Do nice [organisms] finish last?"),
                                     plotOutput("gamePlot"))
                     ),
                     fluidRow(style = 'padding:30px;',
                              column(width = 7,
                                     
                                     # Inputting a gif of new cases. I had to
                                     # ensure all my images were in a folder called
                                     # www.
                                     
                                     img(src = "output.gif", height = 600)),
                              column(width = 5,
                                     h3("Spatial Games"),
                                     p("Now let's forget everything we just learned. Let's say there
                                       is only such thing as someone who always cooperates, and someone
                                       who always defects. Represent an individual as a square on a 99 x 99 lattice. We use the rules of Nowak and May (1992) [8]. 
                                       Each round, each square on
                                       the lattice plays a one-shot prisoner's dilemma with each of its 8 neighbors and itself. Each square's
                                       payoff is the sum of the payoffs it received in these 9 interactions. The square, in the next round, takes the strategy of its
                                       highest performing neighbor. The blues are cooperators who
                                       were cooperators last round,
                                       the reds are defectors who were defectors last round, the greens are cooperators who were defectors last round,
                                       and the yellows are defectors who were cooperators in the last round. 
                                       This visualization is the result of dropping one defector in the middle of a lattice of cooperators. We see pockets of cooperators
                                       remain indefinitely, and depending on the dimensions of the lattice, they may remain forever [8]. 
                                       I want to emphasize to the reader that the 'cooperative' squares in this lattice are incredibly vulnerable to invasion.
                                       We know with different strategies in a repeated game, some 'nicer' strategies would not be exploited. In other words,
                                       this is a worst case scenario here.
                                       It is, of course, possible that some force other than reciprocity might
                                       facilitate cooperation. Here it is network structure. Indeed, network structure is now accepted as a facilitator of
                                       cooperation [7]. It is beautiful both in its idea, and how it plays out visually.
                                       
                                       ")
                              ))
                     
           )
  ),
  tabPanel("References", 
           titlePanel("Background and References"),
           h3("Project Background"),
           p("This is my final project for GENED 1004: Understanding Darwinism."),
           h3("Code"),
           p("Check out my GitHub repo", a("here",
                                          href = "https://github.com/james-wolfe/darwinism")),
           h3("References"),
           p("[1] Axelrod, R., & Hamilton, W. D. (1981). The evolution of cooperation. 
           Science (New York, N.Y.), 211(4489), 1390–1396. https://doi.org/10.1126/science.7466396"),
           p("[2] Browne, E. J. (1996). Charles Darwin: Voyaging. Princeton University Press."),
           p("[3] Darwin, C. (2009). The annotated origin: A facsimile of the first edition of “on the origin of species.” Belknap Press."),
           p("[4] Malthus, T. R. (2018). An essay on the principle of population as it affects 
           the future improvement of society (first edition). In The Economics of Population (pp. 219–222). Routledge."),
           p("[5] May, R. M. (1976). Simple mathematical models with very complicated dynamics. 
           Nature, 261(5560), 459–467. https://doi.org/10.1038/261459a0"),
           p("[6] Nowak, M. A. (2006a). Evolutionary dynamics: Exploring the equations of life. Belknap Press."),
           p("[7] Nowak, M. A. (2006b). Five rules for the evolution of cooperation. Science (New York, N.Y.), 314(5805), 1560–1563. 
           https://doi.org/10.1126/science.1133755"),
           p("[8] Nowak, M. A., & May, R. M. (1992). Evolutionary games and spatial chaos. 
             Nature, 359(6398), 826–829. https://doi.org/10.1038/359826a0"),
           p("[9] Okasha, S. (2016). Population Genetics. In E. N. Zalta (Ed.), 
             The Stanford Encyclopedia of Philosophy (Winter 2016). 
             Metaphysics Research Lab, Stanford University."),
           p("[10] Yorke, J. A., & Li, T.-Y. (1975). Period three implies chaos. The American Mathematical Monthly: 
             The Official Journal of the Mathematical Association of America, 82(10), 985–992. https://doi.org/10.1080/00029890.1975.11994008"),
           
  )
)

server <- function(input, output, session){
  
  output$gamePlot <- renderPlot({
    
    payoff <- function(x,y){
      if (x == 1 & y == 1){
        pf = 2
      }
      else if (x == 0 & y == 1){
        pf = 3
      }
      else if (x == 1 & y == 0){
        pf = -1
      }
      else{
        pf = 0
      }
      return(pf)
    }
    
    me = 1
    you = 1
    
    record = tibble(me = 1,you = 1)
    
    for (i in 1:100){
      x = runif(1)
      y = runif(1)
      z = runif(1)
      er = runif(1)
      if (er < 0.95){
        if (record[i,2] == 1){
          me = 1
        }
        else{
          if (z<1/20){
            me = 1
          }
          else{
            me = 0
          }
        }
        if (record[i,1] == 1){
          if (x < input$p){
            you = 1
          }
          else{
            you = 0
          }
        }
        else{
          if (x < input$q){
            you = 1
          }
          else{
            you = 0
          }
        }
      }
      else{
        if (x < 0.5){
          me = 1
        }
        else{
          me = 0
        }
        if (y < 0.5){
          you = 1
        }
        else{
          you = 0
        }
      }
      record = record %>% add_row(me = me,
                                  you = you)
    }
    
    payf = as.data.frame(tibble(me = 1:101, you = 1:101))
    mean_payf = as.data.frame(tibble(me = 1:101, you = 1:101))
    for (i in 1:101){
      payf[i,1] = payoff(record[i,1], record[i,2])
      payf[i,2] = payoff(record[i,2], record[i,1])
    }
    for (i in 1:101){
      mean_payf[i,1] = mean(payf$me[1:i])
      mean_payf[i,2] = mean(payf$you[1:i])
    }
    
    plt <- mean_payf %>%
      mutate(id = row_number()) %>%
      pivot_longer(cols = c(me,you), names_to = "player", values_to = "payoff") %>%
      ggplot(aes(x = id, y = payoff, color = player)) + 
      geom_line(size = 1.5) +
      geom_label(aes(x = 75,y=1.35,label = "Survival"),fill = "white",color = "black")+
      scale_color_manual(name = "Player",
                         values = c("navyblue","forestgreen"),
                         labels = c("Me", "You")) +
      ylim(-1,3) +
      geom_hline(yintercept = 1.2, linetype='dotted') +
      # annotate(geom="text", x=75, y=1.6, label="Survival") + 
      labs(x = "round",
           y = "average payoff") +
      theme_minimal()
    
    plt
    
  })
  
  output$difPlot <- renderPlot({
    a = input$a
    x0 = input$x0
    
    growth = tibble(x = 1:200, y = x0)
    
    for (i in 2:200){
      growth[i,2] = a*growth[i-1,2]*(1-growth[i-1,2])
    }
    
    growth %>%
      ggplot(aes(x = x, y = y)) +
      geom_line() + 
      theme_minimal() + 
      labs(x = "time",
           y = "abundance of population") + 
      ylim(0,1)
    
  })
  
  output$difPlot2 <- renderPlot({
    c = input$c
    x0 = input$x02
    a = input$a2
    b = input$b
    
    time = seq(from = 0,to = 100, by = 0.01)
    dt=time[2]-time[1]
    growth = as.data.frame(tibble(t = time, A = x0, B = 1-x0))
    
    for (i in 2:length(time)){
      growth[i,2] = growth[i-1,2] + dt*(a*(growth[i-1,2]^(c-1)) - b* (1-growth[i-1,2])^(c-1))*growth[i-1,2]*(1-growth[i-1,2])
      growth[i,3] = 1-growth[i,2]
    }
    
    growth %>%
      pivot_longer(cols = -t, names_to = "type", values_to = "frequency") %>%
      ggplot(aes(x = t, y = frequency, color = type)) +
      geom_line() + 
      theme_minimal() + 
      labs(x = "time",
           y = "abundance of population") + 
      ylim(0,1)
    
  })
  
  output$quasispecies <- renderPlot({
    
    one0 = input$x10
    two0 = input$x20
    three0 = input$x30
    four0 = 1 - one0 - two0 - three0
    f1 = input$f1
    f2 = input$f2
    f3 = input$f3
    f4 = input$f4
    fitness = c(f1,f2,f3,f4)
    
    mat = matrix(c(0.95, 0.02,  0.0,  0.01,
                   0.025, 0.96, 0.015,  0.0,
                   0.0,   0.02, 0.97, 0.01,
                   0.025,  0.0,  0.015, 0.98),
                 nrow = 4,
                 byrow = TRUE)
    
    time = seq(from = 0,to = 100, by = 0.01)
    
    results = data.frame(time = time,
                         one = one0,
                         two = two0,
                         three = three0,
                         four = four0)
    
    dt=time[2]-time[1]
    
    for (i in 1:(length(time)-1)){
      avg_fitness = 0
      for (j in 1:4){
        avg_fitness = avg_fitness + results[i,(j+1)]*fitness[j]
      }
      
      one_sum = 0
      two_sum = 0
      three_sum = 0
      four_sum = 0
      
      for (k in 1:4){
        one_sum = one_sum + results[i,(k+1)]*fitness[k]*mat[1,k]
        two_sum = two_sum + results[i,(k+1)]*fitness[k]*mat[2,k] 
        three_sum = three_sum + results[i,(k+1)]*fitness[k]*mat[3,k]
        four_sum = four_sum + results[i,(k+1)]*fitness[k]*mat[4,k]
      }
      
      one_sum = one_sum - avg_fitness*results[i,2]
      two_sum = two_sum - avg_fitness*results[i,3]
      three_sum = three_sum - avg_fitness*results[i,4]
      four_sum = four_sum - avg_fitness*results[i,5]
      
      results[(i+1), 2] = results[i,2] + dt*(one_sum)
      results[(i+1), 3] = results[i,3] + dt*(two_sum)
      results[(i+1), 4] = results[i,4] + dt*(three_sum)
      results[(i+1), 5] = results[i,5] + dt*(four_sum)
    }

    results %>% 
      pivot_longer(cols = !time, names_to = "genome", values_to = "freq") %>%
      ggplot(aes(x = time, y = freq, color = genome)) + 
      geom_line() +
      scale_color_manual(breaks = c("one","two","three","four"),
                         values = c("orangered","darkorchid2","turquoise","forestgreen")) +
      labs(y = "abundance") +
      theme_minimal()
  })
  
  output$moran <- renderPlot({
    N = input$N
    types = input$types
    reps = input$reps
    
    pop = tibble(x = rep(1:types,N)) 
    
    summary = tibble(time = rep(1:reps, each = types),
                     pop_sum = 10,
                     pp = rep(1:types, times = reps))
    
    for (i in 2:reps){
      death = sample(1:(N*types),size = 1)
      repr = sample(1:(N*types),size = 1)
      pop[death,1] = pop[repr,1]
      vec = pop %>%
        mutate(x = factor(x,
                          levels = 1:types)) %>%
        group_by(x,.drop = FALSE) %>%
        summarize(n = n())
      summary[(types*i-(types-1)):(types*i),2] = vec$n
    }
    
    summary %>% 
      ggplot(aes(x = time, y = pop_sum, fill = as.factor(pp)))+
      geom_area() + 
      labs(x = "time", y = "population size") + 
      guides(fill=guide_legend(title="type")) +
      theme_minimal()
  })
  
}



shinyApp(ui = ui, server = server)
