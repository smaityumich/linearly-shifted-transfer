library(RJSONIO)

summary_path = "~/projects/posterior-drift/UKBB/summaries/"

jsons = list.files(summary_path, full.names = TRUE)

summaries = list()
count = 1
for(file.name in jsons)
{
  summaries[[count]] = fromJSON(file.name)
  count = count + 1
}

summaries.df = matrix(0, nrow = length(summaries),
                      ncol = length(summaries[[1]]))

for(i in 1:length(summaries))
{
  summaries.df[i, ] = unlist(summaries[[i]])
}

colnames(summaries.df) = names(summaries[[1]])