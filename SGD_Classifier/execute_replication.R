#author: ngoet
#first version: 28.9.2018
#this version: 26.10.2018

# dependencies (please uncomment the lines below to install the required R dependencies)
# dependencies <- c('gridExtra', 'Hmisc', 'texreg', 'kernlab', 'car', 'stringr', 'dplyr', 'KernSmooth', 'tidyr', 'pscl', 'RcppArmadillo', 'plyr', 'ggplot2', 'data.table', 'stats', 'pacman', 'methods', 'rlist', 'Rcpp', 'reshape', 'base', 'boot', 'psych', 'grid', 'reshape2', 'tm', 'slam', 'inline', 'mgcv', 'rjags', 'MASS', 'ca', 'parallel', 'quanteda')

# lapply(dependencies,function(x) install.packages(x))

# devtools::install_github("slowkow/ggrepel")
# devtools::install_github("conjugateprior/austin")

#Python dependencies
# python_dependencies <- c('pandas', 'scikit-learn', 'numpy')
# lapply(python_dependencies, function(x) system(paste('pip3 install ',x,sep='')))

# move files
system('mv debate_data online_appendix/skpca/debate_data')
system('mv ukhcdeb_nppr.csv raw_data/ukhcdeb_nppr.csv')
system('mv ukhcdeb_ppr.csv raw_data/ukhcdeb_ppr.csv')

#set working directory
set.seed(42)
dataPath <- gsub('execute_replication.R','',rstudioapi::getSourceEditorContext()$path)

setwd(dataPath)

# set up log file
logfile_name <- gsub('-| |:','_',paste('ukhcpol_logfile_',Sys.time(),'.log',sep=''))
log_file <- file(logfile_name)
cat(paste('Starting replication for main analysis (start time: ',Sys.time(),')',sep=''),file=log_file)
close(log_file)

benchmark <- data.frame(script_name = character(),
	execution_time = numeric())

#execution function
execute_script <- function(benchmark,message,script_name,type="R",input_params=NULL,numCores=NULL){
  clean_workspace()
  message(message)
  log_file <- file(logfile_name, open = 'a')
  cat(paste('\n\nExecuting ',script_name,' script (start time: ',Sys.time(),')',sep=''), file = log_file,append = TRUE)
  close(log_file)

  if(type=="R"){
    execution_time <- round(system.time({source(script_name)})[3]/60,digits=2)
  }else{
    execution_time <- round(system.time({system(paste('python3',script_name,input_params,sep=' '))})[3]/60,digits=2)	
  }

  log_file <- file(logfile_name, open = 'a')
  cat(paste('\n',script_name, ' script successfully run. Execution time: ',execution_time," minutes",sep=''), file = log_file,append = TRUE)
  close(log_file)
  
  print(paste('\n',script_name, ' script successfully run. Execution time: ',execution_time," minutes",sep=''))
  
  benchmark <- rbind(benchmark,data.frame(script_name = script_name,
	execution_time = execution_time))
  
  return(benchmark)
}

#clear function 
clean_workspace <- function(){
  rm(list = setdiff(ls(), c("logfile_name","dataPath","benchmark")))
  Sys.sleep(10)
  gc()
}
# start replication
log_file <- file(logfile_name, open = 'a')
cat(paste('Starting replication of main analysis (start time: ',Sys.time(),')',sep=''),file=log_file)
close(log_file)

# poisson scaling algorithms
# scaling approach 3 - per debate (part 1)
message <- 'Generating estimates for Poisson scaling approach 3 (per debate scaling part 1)'
script_name <- 'poisson_model_implementations/3.wordshoal/wordshoal_with_ca.R'

execute_script(benchmark,message,script_name)
clean_workspace()

# scaling algorithm 2
message <- 'Generating estimates for Poisson scaling approach 2 (dimension scaling)'
script_name <- 'poisson_model_implementations/2.dimension_scaling/4.dimension_scaling.R'

numCores <- 4
threshold <- 0.75 # change to 0.99 for 2nd implementation
execute_script(benchmark,message,script_name,numCores=numCores)
clean_workspace()

# scaling algorithm 1
message <- 'Generating estimates for Poisson scaling approach 1 (full scaling)'
script_name <- 'poisson_model_implementations/1.full_scaling/full_scaling.R'
numCores <- 1
execute_script(benchmark,message,script_name,numCores=numCores)
clean_workspace()

# machine classifier implementations
# generate matrices ppr
message <- 'Generating sparse matrices for speech data where procedural phrases are removed'
script_name <- 'machine_learning_implementations/generate_matrices_ppr.py'

execute_script(benchmark,message,script_name,type="Python",input_params=list(1))
clean_workspace()

#SGD classifier
message <- 'Executing SGD classifier'
script_name <- 'machine_learning_implementations/SGD_CLASSIFIER.py'

execute_script(benchmark,message,script_name,type="Python",input_params='1b 1 1')
clean_workspace()

# generating figures for main analysis
message <- 'Generating figures for main analysis'
script_name <- 'figures/generate_figures.R'

execute_script(benchmark,message,script_name)
clean_workspace()

#log 
computation_time <- sum(benchmark$execution_time)

print(paste('Replication of main analysis complete (total execution time: ',computation_time,' minutes)',sep=''))

log_file <- file(logfile_name, open = 'a')
cat(paste('Replication of main analysis complete (total execution time: ',computation_time,')',sep=''),file=log_file)
close(log_file)

#ON-LINE SUPPLEMENTARY MATERIALS
###############################
print(paste('Starting replication for on-line supplementary materials (start time: ',Sys.time(),')',sep=''))

log_file <- file(logfile_name, open = 'a')
cat(paste('Starting replication for on-line supplementary materials (start time: ',Sys.time(),')',sep=''),file=log_file)
close(log_file)

# machine classifier for US senate
message <- 'Executing SGD classifier for US Senate'
script_name <- 'online_appendix/ReplicationLauderdaleHerzog2016/machine_classification/SGD_CLASSIFIER_IE_US.py'

execute_script(benchmark,message,script_name,type="Python",input_params='1b 0')
clean_workspace()

# machine classifier for Irish Dáil
message <- 'Executing SGD classifier for Irish Dáil'
script_name <- 'online_appendix/ReplicationLauderdaleHerzog2016/machine_classification/SGD_CLASSIFIER_IE_US.py'

execute_script(benchmark,message,script_name,type="Python",input_params='1b 1')
clean_workspace()

# wordshoal US Senate
message <- 'Estimating wordshoal for US Senate'
script_name <- 'online_appendix/ReplicationLauderdaleHerzog2016/Analysis/WordShoalAnalysisUSSenate.R'
execute_script(benchmark,message,script_name)
clean_workspace()

# CFscores comparison
message <- 'Estimating CF scores comparison (generates Figure F3)'
script_name <- 'online_appendix/ReplicationLauderdaleHerzog2016/Analysis/compareCFscores.R'

execute_script(benchmark,message,script_name)
clean_workspace()

# figures (part 1: Figs. F1-F2, Table F2)
message <- 'Generating figures F1-F2; Table F2 (Wordshoal Comparison)'
script_name <- 'online_appendix/figures/generate_figuresF1_F2_tableF2.R'

execute_script(benchmark,message,script_name)
clean_workspace()

# generate table D1
message <- 'Generating table D1'
script_name <- 'online_appendix/tables/tableD1/compare_implementations_NB_vs_SGD.R'

execute_script(benchmark,message,script_name)
clean_workspace()

# generate table E1
message <- 'Generating table E1'
script_name <- 'online_appendix/tables/tableE1/compare_sgd_implementations_ps_vs_nppr.R'

execute_script(benchmark,message,script_name)
clean_workspace()

# string kernel PCA
message <- 'Generating string kernel PCA estimates'
script_name <- 'online_appendix/skpca/skpca_ps.R'
numCores <- 1
execute_script(benchmark,message,script_name)
clean_workspace()

#generate figures B1-B3, E1
message <- 'Generating figures B1-B3, E1'
script_name <- 'online_appendix/figures/generate_figures_appendix.R'

execute_script(benchmark,message,script_name)
clean_workspace()

#NB classifier
message <- 'Executing NB classifier'
script_name <- 'machine_learning_implementations/NB_CLASSIFIER.py'

execute_script(benchmark,message,script_name,type="Python",input_params='1b 1 1')
clean_workspace()

# scaling approach 3 - per debate (part 2)
message <- 'Generating estimates for Poisson scaling approach 3 (per debate scaling part 2)'
script_name <- 'poisson_model_implementations/3.wordshoal/per_debate_scaling.R'
numCores <- 4
execute_script(benchmark,message,script_name,numCores=numCores)
clean_workspace()
