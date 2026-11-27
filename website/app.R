library(shiny)
summary_path <- file.path("data", "summary.rds")
if (!file.exists(summary_path)) stop("Missing data/summary.rds")
summary_data <- readRDS(summary_path)
stopifnot(identical(summary_data$schema_version, 1L),
          all(c("All", "Domestic", "International") %in% names(summary_data$groups)))
is_eda_source <- isTRUE(summary_data$is_eda_source)
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
face_ratings <- function(counts, palette) {
  stopifnot(is.numeric(counts), length(counts)==10L, all(is.finite(counts)), all(counts>=0),
            all(c("ink", "muted", "line", "primary", "secondary", "paper") %in% names(palette)))
  n <- sum(counts)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(bg = palette[["paper"]], fg = palette[["ink"]],
                col.axis = palette[["muted"]], col.lab = palette[["ink"]],
                family = "sans", mar = c(4.5, 4.4, 2.1, 1.2),
                xaxs = "i", yaxs = "i", tcl = -0.22, las = 1)
  y_top <- max(2, max(counts)) * 1.22
  graphics::plot.new()
  graphics::plot.window(xlim = c(0.35, 10.65), ylim = c(0, y_top), xaxs = "i", yaxs = "i")
  y_ticks <- pretty(c(0, max(1, counts)), n = 4)
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_top & y_ticks == floor(y_ticks)]
  if (!length(y_ticks)) y_ticks <- 0:1
  graphics::abline(h = y_ticks, col = palette[["line"]], lwd = 0.8)
  graphics::rect(seq_len(10) - 0.32, 0, seq_len(10) + 0.32, counts,
                 col = palette[["primary"]], border = NA)
  graphics::axis(1, at = 1:10, labels = FALSE,
                 col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::mtext(as.character(1:10), side = 1, at = 1:10, line = 0.65,
                  col = palette[["muted"]], cex = 0.8)
  graphics::axis(2, at = y_ticks, col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::title(xlab = "Response (1-10)", ylab = "Number of respondents", line = 3.0)
  graphics::mtext(sprintf("Valid responses: %s", format(n, big.mark = ",")),
                  side = 3, line = 0.35, adj = 0, col = palette[["muted"]], cex = 0.88)
  if (!n) graphics::text(5.5, y_top / 2, "No valid responses in this view",
                         col = palette[["muted"]], cex = 0.94)
  invisible(list(valid = n, missing_or_invalid = 0L, counts = counts))
}


face_amounts <- function(counts, labels, n, unit, palette) {
  stopifnot(is.numeric(counts), length(counts) == length(labels), length(counts) %in% c(2L, 3L),
            all(is.finite(counts)), all(counts >= 0), all(counts == floor(counts)),
            length(n) == 1L, is.finite(n), n >= 0, sum(counts) == n,
            unit %in% c("percent", "count"),
            all(c("ink", "muted", "line", "primary", "paper") %in% names(palette)))
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  compact <- grDevices::dev.size("in")[1] < 4.3
  graphics::par(bg = palette[["paper"]], fg = palette[["ink"]],
                col.axis = palette[["muted"]], col.lab = palette[["ink"]],
                family = "sans", mar = c(4.2, 1.0, 1.2, 1.0),
                xaxs = "i", yaxs = "i", tcl = -0.22, las = 1)
  max_x <- if (unit == "percent") 100 else max(n, 1)
  positions <- rev(seq_along(counts)) * 1.15
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, max_x), ylim = c(0, length(counts) * 1.15 + 0.5), xaxs = "i", yaxs = "i")
  ticks <- if (unit == "percent") seq(0, 100, by = 25) else pretty(c(0, max_x), n = 4)
  ticks <- ticks[ticks >= 0 & ticks <= max_x & (unit == "percent" | ticks == floor(ticks))]
  if (!length(ticks)) ticks <- 0
  graphics::abline(v = ticks, col = palette[["line"]], lwd = 0.7)
  if (n > 0) {
    values <- if (unit == "percent") 100 * counts / n else counts
    for (i in seq_along(counts)) {
      graphics::rect(0, positions[i] - 0.58, values[i], positions[i] - 0.26, col = palette[["primary"]], border = NA)
      graphics::text(0, positions[i], labels[i], adj = c(0, 0.5), col = palette[["ink"]], cex = if (compact) 0.72 else 0.84)
      graphics::text(max_x, positions[i], sprintf("%d / %d (%.1f%%)", counts[i], n, 100 * counts[i] / n),
                     adj = c(1, 0.5), col = palette[["muted"]], cex = if (compact) 0.72 else 0.84)
    }
  } else {
    graphics::text(max_x / 2, mean(range(positions)), "No responses in this group", col = palette[["muted"]], cex = 0.85)
  }
  graphics::axis(1, at = ticks, labels = if (unit == "percent") paste0(ticks, "%") else ticks,
                 col = palette[["line"]], col.ticks = palette[["line"]], cex.axis = if (compact) 0.76 else 0.9)
  graphics::title(xlab = if (unit == "percent") "Share of respondents" else "Number of respondents", line = 2.5, cex.lab = if (compact) 0.82 else 0.95)
  invisible(list(counts = counts, n = n, unit = unit))
}

ui <- fluidPage(lang = "en", title="Margin Notes — The price of saving face",
  tags$head(tags$link(rel="stylesheet",href="style.css"),tags$script(src="cover.js",defer=NA),
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
        conditionalPanel("input.view === 'budget'",selectInput("amount_measure","Show distribution",c("Monthly budget"="budget","Maximum offer"="wtp"),selectize=FALSE,width="100%")),
        conditionalPanel("input.view === 'budget'",selectInput("axis","Show as",c("Percent of group"="percent","Number of respondents"="count"),selectize=FALSE,width="100%")),
        conditionalPanel("input.view === 'scenes'",selectInput("scene","Survey scenario",c("$20 · Stained shirt"="shirt","$5 · Late arrival"="late"),selectize=FALSE,width="100%")),
        conditionalPanel("input.view === 'ratings'",selectInput("item","Survey item",rating_items,selectize=FALSE,width="100%")),
        selectInput("colour","Colour palette",c("Ink & vermilion"="print","Blue & ochre"="coast"),selectize=FALSE,width="100%")),
      conditionalPanel("input.view === 'budget'",
        tags$div(class="figure-grid",tags$div(class="figure-area",eyebrow("Figure 01 / Amounts"),
          tags$h3("How do budgets and offers vary?"),
          tags$p(class="caption","Choose monthly budget or the maximum extra amount a student would pay to avoid directly discussing a minor embarrassing situation on campus. The bars show separate distributions, not matched answers."),
          plotOutput("budget_plot",height="340px"),
          tags$p(class="plot-key","Each label gives the number in the category, the group size, and the percentage of that group.")),
          tags$aside(class="margin-reading",eyebrow("Reading the figure"),uiOutput("budget_read"),
            if(is_eda_source) report_note("The full-study correlation uses paired answers from all 92 respondents. There was no clear evidence of a monotonic association between monthly budget and maximum willingness to pay (Spearman rho = 0.138, Holm-adjusted p = 0.376). These bars show separate distributions and do not display the paired relationship.")))),
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
        tags$div(tags$p(sprintf("The dataset contains %s complete responses, including %s domestic and %s international students.",summary_data$groups$All$n,summary_data$groups$Domestic$n,summary_data$groups$International$n)),
         tags$details(tags$summary("How to interpret these results"),tags$p("The proposal targeted the two Fall 2026 STA304 lecture sections at UTM and planned recruitment through Piazza by student status. Random selection within each group could not be verified. The results describe these respondents’ hypothetical choices. The two payment scenarios change both the price and the situation, so their difference cannot identify a price effect.")),
         tags$details(tags$summary("Methods and sources"),
          if(is_eda_source) tagList(tags$p("The group’s final exploratory analysis used Spearman rank correlation for the two amounts, two-proportion z-tests for the scenarios, and Cronbach’s alpha for the three ratings. Its significance decisions used a Holm adjustment across 14 comparisons. For late arrival, 4 of 58 domestic respondents said yes. Under the hypothesis of equal payment rates, the smallest of the four expected counts was 7.76. A separate check using two-sided Fisher exact tests for the two scenarios and the same 14-test Holm adjustment also found a group difference for late arrival."),
            tags$p("Source: The Significant Five’s cleaned survey, codebook, original proposal, and final exploratory data analysis (25 October 2026). All amounts are in Canadian dollars."))
          else tags$p("The original study’s fixed estimates and inferential results are not shown for this dataset. Review the methods and sources before interpreting a different dataset. All amounts are in Canadian dollars.")))))),
  tags$footer(class="wrap",tags$b("Margin Notes"),tags$span("The Significant Five · Fall 2026"),tags$a(href="#cover","Back to top ↑")))
server <- function(input, output, session) {
  subset_data <- reactive({
    key <- req(input$group)
    req(key %in% c("All", "Domestic", "International"))
    summary_data$groups[[key]]
  })
  pal <- reactive(palettes[[req(input$colour)]])
  selection <- reactive(scenes[[req(input$scene)]])
  item_label <- reactive(names(rating_items)[match(req(input$item),rating_items)])
  output$sample_tag <- renderText(sprintf("%s  ·  n = %d",group_text(req(input$group)),subset_data()$n))
  amount_counts <- reactive({
    d <- subset_data()
    measure <- req(input$amount_measure)
    req(measure %in% c("budget", "wtp"))
    n <- d$n
    validate(need(is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0 && n == floor(n), "The group total is invalid."))
    if (measure == "budget") {
      values <- d$budget_counts
      labels <- c("Under $500", "$500 to under $1,000", "$1,000 or more")
    } else {
      validate(need(is.numeric(d$zero_count) && length(d$zero_count) == 1L && is.finite(d$zero_count) && d$zero_count >= 0 && d$zero_count <= n && d$zero_count == floor(d$zero_count), "The maximum-offer summary is invalid."))
      values <- c(d$zero_count, n - d$zero_count)
      labels <- c("Maximum offer: $0", "Maximum offer: above $0")
    }
    validate(need(is.numeric(values) && length(values) == length(labels) && all(is.finite(values)) && all(values >= 0) && all(values == floor(values)) && sum(values) == n, "The amount summary does not match the group total."))
    list(values = values, labels = labels, n = n, measure = measure)
  })
  output$budget_plot <- renderPlot({
    d <- amount_counts()
    unit <- req(input$axis)
    req(unit %in% c("percent", "count"))
    face_amounts(d$values, d$labels, d$n, unit, pal())
  }, res=110, alt="Distribution of the selected amount in the selected student group. Each category shows a count, group total, and percentage.")
  output$budget_read <- renderUI({
    d <- amount_counts()
    if (d$n == 0L) return(result("No responses in this group.", "Select All respondents or another student group to see results."))
    if (d$measure == "budget") {
      result(paste0(d$values[1], " of ", d$n, " reported under $500."),
             sprintf("In this group, %d reported a monthly budget of $500 to under $1,000 and %d reported $1,000 or more.", d$values[2], d$values[3]))
    } else {
      result(paste0(d$values[1], " of ", d$n, " reported a maximum offer of $0."),
             sprintf("In this group, %d of %d reported a maximum offer above $0.", d$values[2], d$n))
    }
  })
  output$scene_figure <- renderUI({d<-subset_data();s<-selection();n<-d$n;total<-d$scene_yes[[s$col]];colors<-pal();
    bars<-lapply(c("Domestic","International"),function(g){
      if (input$group != "All" && input$group != g) return(NULL)
      a<-summary_data$groups[[g]];k<-a$scene_yes[[s$col]]
      tags$div(class="bargroup",tags$div(class="barlabels",tags$b(paste(g,"students")),tags$span(sprintf("%s · %d/%d",pct(k,a$n),k,a$n))),
        tags$div(class="track",'aria-hidden'="true",tags$i(style=sprintf("width:%.5f%%;background:%s",100*k/a$n,colors[["primary"]]))))
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
  output$scene_read <- renderUI({d<-subset_data();s<-selection();n<-d$n;k<-d$scene_yes[[s$col]]
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
  output$ratings <- renderPlot(face_ratings(subset_data()$ratings[[req(input$item)]]$counts,pal()),res=110,alt="Frequency of each response from 1 (low) to 10 (high) for the selected survey item.")
  output$rating_read <- renderUI({d<-subset_data();x<-d$ratings[[req(input$item)]]
    result(sprintf("%.2f out of 10.",x$mean),sprintf("Average score for %s among the %d students in this view. The median response was %s out of 10.",tolower(item_label()),d$n,x$median))})
  output$live_status <- renderText(paste(group_text(req(input$group)),subset_data()$n,"respondents.",switch(req(input$view),
     budget="Monthly budget figure.",scenes=paste(selection()$title,"figure."),ratings=paste(item_label(),"figure."))))
}
shinyApp(ui,server)
