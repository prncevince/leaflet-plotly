source('load_cache_labels.R')

tic()
load_cache_labels(
  path = '../x/x_cache/html',
  labels = c('d_x1', 'd_x2')
)
toc()

