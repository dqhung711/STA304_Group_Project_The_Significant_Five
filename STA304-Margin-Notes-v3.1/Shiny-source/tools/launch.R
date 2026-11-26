options(encoding="UTF-8")
args <- commandArgs(trailingOnly=TRUE)
if (!requireNamespace("shiny", quietly=TRUE)) quit(status=1L)
if (identical(args,"check")) quit(status=0L)
if (!identical(args,"run")) stop("Use the launcher for your operating system from the extracted folder.")
shiny::runApp("app", host="127.0.0.1", port=NULL, launch.browser=function(url) {
  cat("Local address:", url, "\n")
  tryCatch(utils::browseURL(url), error=function(e) message("Open that address in your browser."))
})
