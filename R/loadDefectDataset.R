#' A Load Defect Dataset function
#'
#' This function allows you to load defect dataset in software engineering research
#'
#' @param system_name system_name name
#' @param corpus corpus
#' @importFrom foreign read.arff
#' @importFrom utils read.csv
#' @export
#' @return an object of data
#' 
#' @examples
#' Data = loadDefectDataset('groovy-1_5_7','jira')
#' dep <- Data$dep
#' indep <- Data$indep
#' data <- Data$data
#' 
loadDefectDataset <- function(system_name,corpus=""){

    corpus <- ifelse(corpus == "", listDataset[listDataset$system == system_name,]$corpus,corpus)

    read.mccabe <- function(system_name){
        filename <- system.file("extdata/terapromise/mccabe",paste0(system_name,".arff"), package = "Rnalytica")
        data <- read.arff(filename)
        dep <- "Defective"
        switch(system_name, 
               JM1={    dep <- "label" },
               ar1={    dep <- "defects" },
               ar3={    dep <- "defects" },
               ar4={    dep <- "defects" },
               ar5={    dep <- "defects" },
               ar6={    dep <- "defects" },
               kc2={    dep <- "problems" },
                   { dep <- "Defective"}
                )  
        data[,dep] <- ifelse(data[,dep] %in% c("Y","true","yes") , T, F)
        indep <- colnames(data)[!colnames(data) %in% dep]
        return(list(data = data[,c(indep,dep)], dep = dep, indep = indep))
    }
    
    read.ck <- function(system_name){
        filename <- system.file("extdata/terapromise/ck",paste0(system_name,".csv"), package = "Rnalytica")
        data <- read.csv(filename)
        dep <- "bug" 
        data[,dep] <- ifelse(data[,dep] > 0, T, F)
        indep <- c("wmc","dit","noc","cbo","rfc","lcom","ca","ce","npm","lcom3","loc","dam","moa","mfa","cam","ic","cbm","amc","max_cc","avg_cc")
        return(list(data = data[,c(indep,dep)], dep = dep, indep = indep))
    }
    
    read.eclipse <- function(system_name){
        filename <- system.file("extdata/zimmermann/",paste0(system_name,".csv"), package = "Rnalytica")
        data <- read.csv(filename, sep=";")
        dep <- "post"
        indep <- c("pre","ACD","FOUT_avg","FOUT_max","FOUT_sum","MLOC_avg","MLOC_max","MLOC_sum","NBD_avg","NBD_max","NBD_sum","NOF_avg","NOF_max","NOF_sum","NOI","NOM_avg","NOM_max","NOM_sum","NOT","NSF_avg","NSF_max","NSF_sum","NSM_avg","NSM_max","NSM_sum","PAR_avg","PAR_max","PAR_sum","TLOC","CC_avg","CC_max","CC_sum")
        data[,indep] <- lapply(data[,indep], function(x) as.numeric(as.character(x)))
        data[,dep] <- ifelse(data[,dep] > 0, T, F)
        return(list(data = data[,c(indep,dep)], dep = dep, indep = indep))
    }
    
    read.kim <- function(system_name){
        filename <- system.file("extdata/kim",paste0(system_name,".arff"), package = "Rnalytica")
        data <- read.arff(filename)
        dep <- "isDefective"
        indep <- colnames(data)[!colnames(data) %in% c(dep,"defect_num")]
        data[,dep] <- ifelse(data[,dep] %in% c("buggy","TRUE") , T, F)
        return(list(data = data[,c(indep,dep)], dep = dep, indep = indep))
    }
    
    
    read.ambros <- function(system_name){
        filename <- system.file("extdata/ambros/",paste0(system_name,".csv"), package = "Rnalytica")
        data <- read.csv(filename, sep=";")
        dep <- "bugs"
        indep <- colnames(data)[c(2:16)]
        data[,dep] <- ifelse(data[,dep] > 0, T, F)
        return(list(data = data[,c(indep,dep)], dep = dep, indep = indep))
    }
    
    read.jira <- function(system_name){
      filename <- system.file("extdata/jira/",paste0(system_name,".csv"), package = "Rnalytica")
      data <- read.csv(filename)
      data$HeuBug <- as.factor(data$HeuBug)
      data$RealBug <- as.factor(data$RealBug)
      dep <- "RealBug"
      indep <- colnames(data)[c(2:66)]
      return(list(data = data[,c(indep,dep,"HeuBug")], dep = dep, indep = indep))
    }
    
    pool <- NULL
    switch(corpus, 
           mccabe={    pool <- read.mccabe(system_name) },
           ck={     pool <- read.ck(system_name) },
           eclipse={    pool <- read.eclipse(system_name) },
           kim={    pool <- read.kim(system_name) },
           ambros={    pool <- read.ambros(system_name) },
           jira={    pool <- read.jira(system_name) }
    )  
    pool
}


