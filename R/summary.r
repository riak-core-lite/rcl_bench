#!/usr/bin/env Rscript


# Parse the --file= argument out of command line args and
# determine where base directory is so that we can source
# our common sub-routines
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
dir0 <- dirname(arg0)
source(file.path(dir0, "common.r"))

theme_set(theme_grey(base_size = 17))

# Setup parameters for the script
params = matrix(c(
  'help',    'h', 0, "logical",
  'width',   'x', 2, "integer",
  'height',  'y', 2, "integer",
  'outfile', 'o', 2, "character",
  'indir',   'i', 2, "character",
  'tstart',  '1',  2, "integer",
  'tend',    '2',  2, "integer",
  'ylabel1stgraph', 'Y',  2, "character"
  ), ncol=4, byrow=TRUE)

# Parse the parameters
opt = getopt(params)

if (!is.null(opt$help))
  {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
  }

# Initialize defaults for opt
if (is.null(opt$width))   { opt$width   = 1280 }
if (is.null(opt$height))  { opt$height  = 960 }
if (is.null(opt$indir))   { opt$indir  = "current"}
if (is.null(opt$outfile)) { opt$outfile = file.path(opt$indir, "summary.png") }
if (is.null(opt$ylabel1stgraph)) { opt$ylabel1stgraph = "Op/sec" }

# Load the benchmark data, passing the time-index range we're interested in
b = load_benchmark(opt$indir, opt$tstart, opt$tend)

png(file = opt$outfile, width = opt$width, height = opt$height)

# First plot req/sec from summary
plot1 <- qplot(elapsed, successful / window, data = b$summary,
                geom = c("smooth", "point"),
                xlab = "Elapsed Secs", ylab = opt$ylabel1stgraph,
                main = "Throughput") +

                geom_smooth(aes(y = successful / window, colour = "ok"), size=0.5) +
                geom_point(aes(y = successful / window, colour = "ok"), size=2.0) +

                geom_smooth(aes(y = failed / window, colour = "error"), size=0.5) +
                geom_point(aes(y = failed / window, colour = "error"), size=2.0) +

                scale_colour_manual("Response", values = c("#FF665F", "#188125"))


# Setup common elements 
grid.newpage()

pushViewport(viewport(layout = grid.layout(1, 1)))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)

print(plot1, vp = vplayout(1,1))

dev.off()

txtplot(x = b$summary$elapsed, y = b$summary$successful / b$summary$window, xlab = "Successful requests per second")
txtplot(b$summary$elapsed, b$summary$failed / b$summary$window, xlab = "Errors per second")

str(b)
