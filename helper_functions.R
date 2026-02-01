# ------------ Helper Functions Used By Assignment, You May Ignore ------------
sample_normal <- function(n, mean=0, sd=1) {
  set.seed(1337)
  samples <- rnorm(n, mean=mean, sd=sd)
  return(samples)
}

sample_normal_w_missing <- function(n, mean=0, sd=1, missing_frac=0.1) {
  set.seed(1337)
  samples <- rnorm(n, mean=mean, sd=sd)
  missing <- rbinom(length(samples), 1, missing_frac)==1
  samples[missing] <- NA
  return(samples)
}

simulate_gene_expression <- function(num_samples, num_genes) {
  set.seed(1337)
  gene_exp <- matrix(
    rnbinom(num_samples*num_genes, rlnorm(num_genes,meanlog = 3), prob=runif(num_genes)),
    nrow=num_genes
  )
  return(gene_exp)
}

simulate_gene_expression_w_missing <- function(num_samples, num_genes, missing_frac=0.1) {
  gene_exp <- simulate_gene_expression(num_samples, num_genes)
  missing <- matrix(
    rbinom(num_samples*num_genes, 1, missing_frac)==1,
    nrow=num_genes
  )
  gene_exp[missing] <- NA
  return(gene_exp)
}