source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)

# path.base <- "../"
path.base <- "/home/schoed/scratch/fcne_analysis/"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")

file.data <- paste0(path.data.proc, region, ".data.proc.rds")
path.arrow <- paste0(path.lp, region, ".lp/")

region <- tolower(args[1])
n.threads <- as.integer(args[2])

cpu_count()
getDTthreads()

ds <- open_dataset(paste0(path.arrow, "marginal=ten_loc"), format = "arrow")
data.proc <- readRDS(paste0(path.base, "data/processed/", region, ".data.proc.rds"))

groups <- ids_by_group(dat, id.col = "id", c("it_type", "pa_type"), add.label = TRUE)


system.time({
sum_pred <- summarize_predictions(ds, ids = list(a = 1:1e5, b = 2e5:3e5), n.threads = 4,
                                  draw.ids = as.character(1:100), draw.chunk = 10)
})

id.list <- groups$ids
names(id.list) <- groups$group.label

system.time({
sum_pred <- summarize_predictions(ds, ids = id.list, n.threads = 4,
                                  draw.ids = as.character(1:100), draw.chunk = 100)
})


system.time({
sum_pred <- summarize_predictions(ds, ids = id.list, n.threads = 4,
                                  draw.ids = as.character(1:1000), draw.chunk = 100)
})

summary(sum_pred)
