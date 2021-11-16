source("utilities.R")

args <- commandArgs(trailingOnly = TRUE)

region <- tolower(args[1])
n.threads <- as.integer(args[2])

# path.base <- "../"
path.base <- "/home/schoed/scratch/fcne_analysis/"
path.lp <- paste0(path.base, "models/gam/lp/")
path.data.proc <- paste0(path.base, "data/processed/")

file.data <- paste0(path.data.proc, region, ".data.proc.rds")
path.arrow <- paste0(path.lp, region, ".lp/")

set_cpu_count(n.threads)
setDTthreads(n.threads)

cpu_count()
getDTthreads()

ds <- open_dataset(paste0(path.arrow, "marginal=ten_loc"), format = "arrow")
data.proc <- readRDS(paste0(path.base, "data/processed/", region, ".data.proc.rds"))

groups <- ids_by_group(data.proc, id.col = "id", c("it_type", "pa_type"), add.label = TRUE)

id.list <- groups$ids
names(id.list) <- groups$group.label

system.time({
sum_pred <- summarize_predictions(ds, ids = id.list, n.threads = 4,
                                  draw.ids = as.character(1:100), draw.chunk = 100)
})
summary(sum_pred)
summary(sum_pred - extract_variable(sum_pred, "it_type.none:pa_type.none"))

# system.time({
# sum_pred <- summarize_predictions(ds, ids = id.list, n.threads = 4,
#                                   draw.ids = as.character(1:1000), draw.chunk = 100)
# })
# summary(sum_pred)
# summary(sum_pred - extract_variable(sum_pred, "it_type.none:pa_type.none"))

ds <- open_dataset(paste0(path.arrow, "marginal=full"), format = "arrow")
system.time({
sum_pred <- summarize_predictions(ds, ids = id.list, n.threads = 4,
                                  draw.ids = as.character(1:100), draw.chunk = 100)
})
summary(sum_pred)
summary(sum_pred - extract_variable(sum_pred, "it_type.none:pa_type.none"))
