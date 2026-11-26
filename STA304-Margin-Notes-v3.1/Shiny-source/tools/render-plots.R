# Usage from the package root: Rscript --vanilla tools/render-plots.R "path/to/cleaned.csv"
args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=1L) stop("Supply the path to the original cleaned survey CSV.")
source(file.path("app","charts.R"), encoding="UTF-8")
header <- names(read.csv(args[1], nrows=0))
items <- c("judgment_worry","general_embarrassment","communication_discomfort")
fields <- c("student_status","budget_cad","max_wtp_cad",items)
stopifnot(all(fields %in% header))
types <- ifelse(header=="student_status","character",ifelse(header %in% fields,"numeric","NULL"))
d <- read.csv(args[1],colClasses=types)
out <- file.path("app","www","plots")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
palettes <- list(print=c(ink="#282921",muted="#5f6053",line="#d2cdba",primary="#aa3726",secondary="#45665d",paper="#fbf8f0"),
                 coast=c(ink="#232e38",muted="#536372",line="#cbd0cb",primary="#275d91",secondary="#996222",paper="#fbf8f0"))
draw <- function(name,height,callback){
  grDevices::png(file.path(out,name),width=1344,height=height,res=170)
  on.exit(grDevices::dev.off());callback()
}
for (group in c("all","domestic","international")) {
  rows <- if(group=="all")d else d[tolower(d$student_status)==group,,drop=FALSE]
  for(col in names(palettes)) {
    for(axis in c("full","25")) {
      name <- paste0(paste(group,"budget",axis,col,sep="-"),".png")
      draw(name,650,function()face_scatter(rows,if(axis=="full")max(150,d$max_wtp_cad) else 25,palettes[[col]]))
    }
    for(item in items){
      name <- paste0(paste(group,item,col,sep="-"),".png")
      draw(name,595,function()face_ratings(rows,item,palettes[[col]]))
    }
  }
}
