library(shiny)

dir <- "STA304-Margin-Notes-v3.1/Shiny-source/app"
#source("STA304-Margin-Notes-v3.1/Shiny-source/app/charts.R", local = TRUE)

source("charts.R", local = TRUE)

csv <- "data/dataset.csv"

#csv <- Sys.getenv("STA304_SURVEY_CSV", unset = file.path("data","The_Significant_Five_dataset.csv"))
#if (!file.exists(csv)) stop("Set STA304_SURVEY_CSV to the group's cleaned survey CSV.")

fields <- c("student_status", "budget_cad", "max_wtp_cad", "pay5_lateness", "buy20_shirt",
            "judgment_worry", "general_embarrassment", "communication_discomfort")

# Anna: I don't know why you are using read.csv() multiple times, it's more efficient to just
# load in the .csv to begin with.
header <- names(read.csv(csv, nrows = 0))
stopifnot(all(fields %in% header))
types <- ifelse(header == "student_status", "character", ifelse(header %in% fields, "numeric", "NULL"))
survey <- read.csv(csv, colClasses = types, check.names = FALSE)
stopifnot(nrow(survey) > 0, !anyNA(survey),
          all(survey$student_status %in% c("Domestic", "International")),
          all(as.matrix(survey[c("pay5_lateness", "buy20_shirt")]) %in% 0:1),
          all(as.matrix(survey[c("budget_cad", "max_wtp_cad")]) >= 0),
          all(as.matrix(survey[c("judgment_worry","general_embarrassment","communication_discomfort")]) %in% 1:10))
# The published EDA's estimates and intervals below apply to this exact source file.
is_eda_source <- identical(unname(tools::md5sum(csv)), "883f5b57ede66d2c368ec0b645b2f7ed")
scenes <- list(
  shirt = list(col = "buy20_shirt", title = "A clean shirt before presenting",
               prompt = "You are on campus 20 minutes before an important presentation when you notice a large stain on your shirt. Would you pay $20 to change into a new shirt immediately, before anyone sees it?",
               brief = "pay $20 for a replacement shirt", figure = "The replacement shirt"),
  late = list(col = "pay5_lateness", title = "Arriving late to a tutorial",
              prompt = "You arrive 15 minutes late to a tutorial. Would you pay $5 to check in silently through an app and avoid explaining your late arrival to the TA?",
              brief = "pay $5 to skip the explanation", figure = "The late arrival"))
rating_items <- c("Worry about judgment" = "judgment_worry", "General embarrassment" = "general_embarrassment",
                    "Communication discomfort" = "communication_discomfort")
rating_scales <- list(
  judgment_worry = c(low = "Strongly disagree", high = "Strongly agree"),
  general_embarrassment = c(low = "Not at all embarrassed", high = "Extremely embarrassed"),
  communication_discomfort = c(low = "Very comfortable", high = "Very uncomfortable"))
palettes <- list(print = c(ink="#282921", muted="#5f6053",line="#d2cdba",primary="#aa3726",secondary="#45665d",paper="#fbf8f0"),
                 coast = c(ink="#232e38",muted="#536372",line="#cbd0cb",primary="#275d91",secondary="#996222",paper="#fbf8f0"))
choices <- c("All respondents"="All","Domestic students"="Domestic","International students"="International")
pct <- function(k,n) sprintf("%.1f%%",100*k/n)
cash <- function(x) paste0("$",format(x,big.mark=",",trim=TRUE,scientific=FALSE))
eyebrow <- function(text) tags$span(class="eyebrow",text)
group_text <- function(x) if (x=="All") "All respondents" else paste(x,"students")
result <- function(title, body) tagList(tags$h3(title), tags$p(body))
report_note <- function(text) tags$div(class="report-note",eyebrow("Full study · both groups · 92 respondents"),tags$p(text))
make_tab <- function(value, title, n) tags$label(class="tab-label",
  tags$input(type="radio", name="view", value=value, checked=if (value=="scenes") "checked" else NULL),
  tags$span(tags$small(n),title))

ui <- fluidPage(lang = "en", title="Margin Notes — The price of saving face",
                tags$head(tags$link(rel="stylesheet",href="www/style.css"),tags$script(src="www/cover.js",defer=NA),
            tags$meta(name="description",content="Interactive findings from a survey of students' willingness to pay to avoid everyday embarrassment.")),
  tags$a(class="skip",href="#explorer","Skip to the figures"),
  tags$header(class="mast", tags$div(class="wrap mast-row",tags$a(class="wordmark",href="#cover",tags$i("///"),"Margin Notes"),
    tags$span(class="issue","Campus field studies"),tags$nav('aria-label'="Page sections",tags$a(href="#explorer","Figures"),tags$a(href="#method","Study notes")))),
  tags$main(
    tags$section(id="cover",class="wrap cover",
      tags$div(class="cover-meta",eyebrow("A field study in everyday embarrassment"),tags$span("Autumn 2026")),
      tags$div(class="spread",tags$div(class="lead-col",tags$h1("The price of",tags$br(),tags$em("saving face.")),
        tags$p(class="dek","A stained shirt before a presentation. A late arrival to class. What would students spend to avoid the awkward moment?"),
        tags$a(id="open-shirt",class="cta",href="#explorer","See who would pay $20",tags$span('aria-hidden'="true","↘"))),
        tags$figure(class="print",tags$img(src="shirt-print.webp",alt="Print illustration of a stained button-down shirt with a red price tag."),
          tags$figcaption(tags$span("The replacement shirt"),tags$b("CAD 20")))),
      tags$div(class="credit",tags$b("The Significant Five"),tags$span("University of Toronto Mississauga"),tags$span("STA304 · Survey research"))),
    tags$section(id="explorer",class="wrap explorer",tabindex="-1",
      tags$div(class="section-lead",tags$div(eyebrow("From the survey"),tags$h2("Explore the evidence.")),textOutput("sample_tag",inline=TRUE)),
      tags$p(class="caption", if(is_eda_source) "The student group filter updates the chart and its summary. Notes labelled “Full study” and the key findings use all 92 responses." else "Figures and descriptions follow the selected group."),
      tags$div(id="view",role="radiogroup",'aria-label'="Choose a figure",class="shiny-input-radiogroup shiny-input-container tabs",
        make_tab("budget","Monthly budget","01"),make_tab("scenes","Payment scenarios","02"),make_tab("ratings","Three ratings","03")),
      tags$div(class="controls",
        selectInput("group","Student group",choices,selectize=FALSE,width="100%"),
        conditionalPanel("input.view === 'budget'",selectInput("axis","Willingness-to-pay axis",c("Full range"="full","Focus on $0–$25"="25"),selectize=FALSE,width="100%")),
        conditionalPanel("input.view === 'scenes'",selectInput("scene","Survey scenario",c("$20 · Stained shirt"="shirt","$5 · Late arrival"="late"),selectize=FALSE,width="100%")),
        conditionalPanel("input.view === 'ratings'",selectInput("item","Survey item",rating_items,selectize=FALSE,width="100%")),
        selectInput("colour","Colour palette",c("Ink & vermilion"="print","Blue & ochre"="coast"),selectize=FALSE,width="100%")),
      conditionalPanel("input.view === 'budget'",
        tags$div(class="figure-grid",tags$div(class="figure-area",eyebrow("Figure 01 / Budget"),tags$h3("Do students with bigger budgets offer more?"),
          tags$p(class="caption","Students entered the most they would pay, in extra Canadian dollars, to avoid talking directly about a minor embarrassing situation on campus. The chart pairs that amount with their monthly budget after essentials. The $5 late-arrival and $20 shirt questions each asked for a yes or no."),
          plotOutput("scatter",height="340px"),
          tags$p(class="plot-key","Larger circles represent more responses at the same two amounts."),textOutput("zoom_note")),
          tags$aside(class="margin-reading",eyebrow("Reading the figure"),uiOutput("budget_read"),
            if(is_eda_source) report_note("The data give no clear evidence that the amount students would pay rises or falls consistently with their monthly budget (Spearman ρ = 0.138, Holm-adjusted p = 0.376).")))),
      conditionalPanel("input.view === 'scenes'",
        tags$div(class="figure-grid",tags$div(class="figure-area",eyebrow("Figure 02 / Payment intentions"),uiOutput("scene_figure")),
          tags$aside(class="margin-reading",eyebrow("Reading the figure"),uiOutput("scene_read")))),
      conditionalPanel("input.view === 'ratings'",
        tags$div(class="figure-grid",tags$div(class="figure-area",eyebrow("Figure 03 / Rating distributions"),tags$h3(textOutput("rating_title",inline=TRUE)),
          textOutput("rating_prompt"),uiOutput("rating_endpoints"),plotOutput("ratings",height="295px"),tags$p(class="plot-key","Each response contributes one rating to the selected item.")),
          tags$aside(class="margin-reading",eyebrow("Reading the figure"),uiOutput("rating_read"),
            if(is_eda_source) report_note("Cronbach’s α was 0.793 (95% bootstrap interval 0.688–0.861), a measure of how consistently the three ratings varied together. Establishing whether the questions measure a single trait, and how well they measure it, requires separate evidence.")))),
      tags$div(class="sr-only",role="status",'aria-live'="polite",textOutput("live_status"))),
    if (is_eda_source) tags$section(id="key-findings", class="wrap findings", 'aria-labelledby'="findings-title",
      eyebrow("Full study · 92 respondents"),
      tags$h2(id="findings-title", "Key findings."),
      tags$div(class="findings-grid",
        tags$div(tags$h3("Naming a price"),
                 tags$p("When students named their own price for avoiding a conversation, 60 of 92 entered $0. The sample gives no clear evidence that students with larger budgets consistently offered more.")),
        tags$div(tags$h3("Making a choice"),
                 tags$p("For the $20 shirt, 57 of 92 said yes. For the $5 late arrival, 21 of 92 said yes, including 17 of 34 international and 4 of 58 domestic students. Each question changed both the price and the situation.")),
        tags$div(tags$h3("Describing a feeling"),
                 tags$p("Students who gave a high rating for one feeling tended to rate the others higher too. Across all three items, internal consistency was 0.793 on Cronbach’s alpha measure. These hypothetical answers cannot tell us whether those feelings or student status caused a payment choice.")))),
    tags$section(id="method",class="notes",
      tags$div(class="wrap method-grid",tags$div(eyebrow("Study notes"),tags$h2("Who answered?")),
        tags$div(tags$p(sprintf("The dataset contains %s complete responses, including %s domestic and %s international students.",nrow(survey),sum(survey$student_status=="Domestic"),sum(survey$student_status=="International"))),
         tags$details(tags$summary("How to interpret these results"),tags$p("The proposal targeted the two Fall 2026 STA304 lecture sections at UTM and planned recruitment through Piazza by student status. Random selection within each group could not be verified. The results describe these respondents’ hypothetical choices. The two payment scenarios change both the price and the situation, so their difference cannot identify a price effect.")),
         tags$details(tags$summary("Methods and sources"),
          if(is_eda_source) tagList(tags$p("The group’s final exploratory analysis used Spearman rank correlation for the two amounts, two-proportion z-tests for the scenarios, and Cronbach’s alpha for the three ratings. Its significance decisions used a Holm adjustment across 14 comparisons. For late arrival, 4 of 58 domestic respondents said yes. Under the hypothesis of equal payment rates, the smallest of the four expected counts was 7.76. A separate check using two-sided Fisher exact tests for the two scenarios and the same 14-test Holm adjustment also found a group difference for late arrival."),
            tags$p("Source: The Significant Five’s cleaned survey, codebook, original proposal, and final exploratory data analysis (25 October 2026). All amounts are in Canadian dollars."))
          else tags$p("The original study’s fixed estimates and inferential results are not shown for this dataset. Review the methods and sources before interpreting a different dataset. All amounts are in Canadian dollars.")))))),
  tags$footer(class="wrap",tags$b("Margin Notes"),tags$span("The Significant Five · Fall 2026"),tags$a(href="#cover","Back to top ↑")))

server <- function(input, output, session) {
  subset_data <- reactive({
    key <- req(input$group)
    if(key=="All") survey else survey[survey$student_status==key,,drop=FALSE]
  })
  pal <- reactive(palettes[[req(input$colour)]])
  selection <- reactive(scenes[[req(input$scene)]])
  item_label <- reactive(names(rating_items)[match(req(input$item),rating_items)])
  output$sample_tag <- renderText(sprintf("%s  ·  n = %d",group_text(req(input$group)),nrow(subset_data())))
  output$scatter <- renderPlot({face_scatter(subset_data(),if(identical(input$axis,"25")) 25 else max(150,survey$max_wtp_cad),pal())},res=110,
      alt="Scatter plot of monthly discretionary budget versus maximum willingness to pay. The legend compares circles for 1, 5 and 15 responses at the same coordinates.")
  output$zoom_note <- renderText({
    d<-subset_data();if(identical(input$axis,"25")){
      k<-sum(d$max_wtp_cad>25);sprintf("Focused view: %d of %d responses above $25 lie outside the frame. The summary includes all %d.",k,nrow(d),nrow(d))
    } else paste(nrow(d),"responses shown · full vertical range.")
  })
  output$budget_read <- renderUI({d<-subset_data();n<-nrow(d);zeros<-sum(d$max_wtp_cad==0)
    result(paste0("A median offer of ",cash(median(d$max_wtp_cad)),"."),
           sprintf("%d of %d respondents in this view offered $0. Across this selected group, the median monthly discretionary budget was %s.",zeros,n,cash(median(d$budget_cad))))})
  output$scene_figure <- renderUI({d<-subset_data();s<-selection();n<-nrow(d);total<-sum(d[[s$col]]);colors<-pal();
    bars<-lapply(c("Domestic","International"),function(g){
      a<-d[d$student_status==g,,drop=FALSE];if(!nrow(a))return(NULL);k<-sum(a[[s$col]])
      tags$div(class="bargroup",tags$div(class="barlabels",tags$b(paste(g,"students")),tags$span(sprintf("%s · %d/%d",pct(k,nrow(a)),k,nrow(a)))),
        tags$div(class="track",'aria-hidden'="true",tags$i(style=sprintf("width:%.5f%%;background:%s",100*k/nrow(a),colors[["primary"]]))))
    })
    tagList(tags$h3(s$title),tags$p(class="caption",s$prompt),
      tags$div(class="payment-grid",style=paste0("--dot:",colors[["primary"]]),
        tags$div(class="matrix",'aria-hidden'="true",lapply(seq_len(n),function(i)tags$i(class=if(i<=total)"yes" else "no"))),
        tags$div(class="matrix-key",
          tags$span(tags$i(class="yes",'aria-hidden'="true"),sprintf("Would pay · %d",total)),
          tags$span(tags$i(class="no",'aria-hidden'="true"),sprintf("Would decline · %d",n-total)))),
      tags$p(class="plot-key","Each square represents one respondent in this view."),
      tags$div(class="barchart",bars,tags$div(class="axis-labels",tags$span("0%"),tags$span("50%"),tags$span("100%"))))
  })
  output$scene_read <- renderUI({d<-subset_data();s<-selection();n<-nrow(d);k<-sum(d[[s$col]])
    tagList(result(paste0(pct(k,n)," would pay."),sprintf("%d of %d respondents in this view said they would %s.",k,n,s$brief)),
            if(is_eda_source) report_note(if(input$scene=="shirt")
              "76.5% of international and 53.4% of domestic respondents would buy the shirt. The group difference was inconclusive after multiple-testing adjustment (p = 0.141)."
              else "50.0% of international and 6.9% of domestic respondents would pay to skip explaining their late arrival. The gap was 43.1 percentage points (adjusted p < 0.001)."))
  })
  output$rating_title <- renderText(item_label())
  output$rating_endpoints <- renderUI({
    scale <- rating_scales[[req(input$item)]]
    tags$div(class="scale-key",
      tags$span(tags$b("1"),scale[["low"]]),
      tags$span(tags$b("10"),scale[["high"]]))
  })
  output$rating_prompt <- renderText(switch(req(input$item),
    judgment_worry="How strongly do you agree that you worry about being judged by other students when you make a mistake?",
    general_embarrassment="How embarrassed do you feel in minor awkward situations on campus?",
    communication_discomfort="How uncomfortable do you feel discussing an awkward situation directly with a peer or instructor?"))
  output$ratings <- renderPlot(face_ratings(subset_data(),req(input$item),pal()),res=110,alt="Frequency of each response from 1 (low) to 10 (high) for the selected survey item.")
  output$rating_read <- renderUI({x<-subset_data()[[req(input$item)]]
    result(sprintf("%.2f out of 10.",mean(x)),sprintf("Average score for %s among the %d students in this view. The median response was %s out of 10.",tolower(item_label()),length(x),median(x)))})
  output$live_status <- renderText(paste(group_text(req(input$group)),nrow(subset_data()),"respondents.",switch(req(input$view),
     budget="Monthly budget figure.",scenes=paste(selection()$title,"figure."),ratings=paste(item_label(),"figure."))))
}
shinyApp(ui,server)
