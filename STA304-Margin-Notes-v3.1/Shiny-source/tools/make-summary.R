# Generate ONLY aggregate summaries from the course CSV.
# Usage: Rscript make-summary.R /path/to/group-cleaned.csv data/summary.rds
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop('Usage: Rscript make-summary.R input.csv output.rds')
csv <- args[1]; destination <- args[2]
fields <- c('student_status','budget_cad','max_wtp_cad','pay5_lateness','buy20_shirt',
            'judgment_worry','general_embarrassment','communication_discomfort')
header <- names(read.csv(csv, nrows = 0, check.names = FALSE))
stopifnot(all(fields %in% header))
types <- ifelse(header == 'student_status','character', ifelse(header %in% fields,'numeric','NULL'))
dat <- read.csv(csv, colClasses = types, check.names = FALSE)
stopifnot(nrow(dat)>0L, !anyNA(dat), all(dat$student_status %in% c('Domestic','International')),
          all(as.matrix(dat[c('pay5_lateness','buy20_shirt')]) %in% 0:1),
          all(as.matrix(dat[c('budget_cad','max_wtp_cad')]) >= 0),
          all(as.matrix(dat[c('judgment_worry','general_embarrassment','communication_discomfort')]) %in% 1:10))
items <- c('judgment_worry','general_embarrassment','communication_discomfort')
scenes <- c('pay5_lateness','buy20_shirt')
make_group <- function(d) {
  stopifnot(nrow(d)>0L)
  list(n=nrow(d), budget_median=median(d$budget_cad), wtp_median=median(d$max_wtp_cad),
       zero_count=sum(d$max_wtp_cad==0), above_25=sum(d$max_wtp_cad>25),
       scene_yes=setNames(lapply(scenes, function(v) sum(d[[v]])),scenes),
       ratings=setNames(lapply(items, function(v) list(counts=tabulate(d[[v]], nbins=10L),
                            mean=mean(d[[v]]), median=median(d[[v]]))),items))
}
output <- list(schema_version=1L,
               is_eda_source=identical(unname(tools::md5sum(csv)), '883f5b57ede66d2c368ec0b645b2f7ed'),
               groups=list(All=make_group(dat), Domestic=make_group(dat[dat$student_status=='Domestic',,drop=FALSE]),
                           International=make_group(dat[dat$student_status=='International',,drop=FALSE])))
saveRDS(output, destination, version=2)
cat('Saved aggregate summaries for', output$groups$All$n, 'respondents.\n')
