
# can be quickly applied to data.table

# bin data vectors into 4 levels
data_bin_4 <- function(x) {
  p = quantile(x, na.rm = TRUE)
  p <- unique(p)  # Remove duplicates first
  
  # Need at least 2 unique breaks for cut() to work
  if (length(p) < 2) {
    # All values are identical - return all 1s
    return(rep(1, length(x)))
  }
  
  as.numeric(cut(x, breaks=p, include.lowest = T))
}

# bin data vectors into 3 levels
data_bin_3 <- function(x) {
  p = quantile(x, probs = c(0, 0.33333, 0.66666, 1), na.rm = TRUE)
  p <- unique(p)  # Remove duplicates first
  
  # Need at least 2 unique breaks for cut() to work
  if (length(p) < 2) {
    # All values are identical - return all 1s
    return(rep(1, length(x)))
  }
  
  as.numeric(cut(x, breaks=p, include.lowest = T))
}

# bin data vectors into 2 levels
data_bin_2 <- function(x) {
  p = quantile(x, probs = c(0, 0.5, 1), na.rm = TRUE)
  p <- unique(p)  # Remove duplicates first
  
  # Need at least 2 unique breaks for cut() to work
  if (length(p) < 2) {
    # All values are identical - return all 1s
    return(rep(1, length(x)))
  }
  
  as.numeric(cut(x, breaks=p, include.lowest = T))
}

#' deng_obj  data engineering object (deng)
#' 
#' An object that transforms data into more robust forms.
#' This includes pairs, signature-pairs, quartiles, and ranks
#'
#' @examples
#' # New object
#' ann <- Recm$new("Ann")
#' 
#' @export
Data_eng <- R6Class("Data_eng",
                public = list(
                  
                  #' @field data_mode what type of engineering to do
                  data_mode = NULL,
                  
                  #' @field signatures if there are signatures to be used
                  signatures = NULL,
                  
                  #' @field pair_list list of column names to create pairs
                  pair_list = NULL,
                  
                  #' @description Create a new `Recm` object.
                  #' @param name The object is named.
                  #' @return A new `recm` object.
                  initialize = function(data_mode  = NULL,
                                        signatures = NULL,
                                        pair_list  = NULL) {
                    self$data_mode <- data_mode
                    self$signatures <- signatures
                    self$pair_list <- pair_list
                    
                    if (!all(data_mode %in% c('pairs','allpairs','sigpairs','namedpairs','quartiles','tertiles','binary','ranks','original'))) {
                      print("ERROR:  please choose a valid collection of data modes: ")
                      print('pairs,namedpairs,allpairs,sigpairs,quartiles,tertiles,binary,ranks,original')
                      stop(paste0('data_mode, ', self$data_mode  ,' wrong value'))
                    }
                    
                  },
                  
                  
                  # data engineering
                  #' @description Data engineering, replaces the object's data.table.
                  data_eng = function(data=NULL) {
                    
                    rankdat <- NULL
                    pairdat <- NULL
                    newdat <- data.table()
                    allgenes <- c(unlist(self$pair_list), unlist(self$signatures))
                    datcols <- colnames(data)
                    
                    if ('original' %in% self$data_mode) {
                      newdat <- cbind(newdat, data)
                    }
                    
                    if (length(allgenes) == 0) {
                      stop('No provided feature names in pairs or sigpairs.')
                    }
                    
                    if ('tertiles' %in% self$data_mode) {
                      # Filter to numeric columns only before apply
                      numeric_cols <- sapply(data, is.numeric)
                      numeric_col_names <- names(data)[numeric_cols]
                      numeric_data <- data[, ..numeric_col_names]
                      
                      tertdat <- as.data.table(t(apply(numeric_data, 1, data_bin_3)))
                      
                      # Use the numeric column names instead of datcols
                      colnames(tertdat) <- numeric_col_names
                      tertdat <- tertdat[, ..allgenes]
                      colnames(tertdat) <- sapply(colnames(tertdat), function(a) paste0(a,'_tertiles',collapse = ''))
                      newdat <- cbind(newdat, tertdat)
                    }
                    
                    if ('quartiles' %in% self$data_mode) {
                      # Filter to numeric columns only before apply
                      numeric_cols <- sapply(data, is.numeric)
                      numeric_col_names <- names(data)[numeric_cols]
                      numeric_data <- data[, ..numeric_col_names]
                      
                      quartdat <- as.data.table(t(apply(numeric_data, 1, data_bin_4)))
                      
                      # Use the numeric column names instead of datcols
                      colnames(quartdat) <- numeric_col_names
                      quartdat <- quartdat[, ..allgenes]
                      colnames(quartdat) <- sapply(colnames(quartdat), function(a) paste0(a,'_quartiles',collapse = ''))
                      newdat <- cbind(newdat, quartdat)
                    }
                    
                    if ('binarize' %in% self$data_mode) {
                      # Filter to numeric columns only before apply
                      numeric_cols <- sapply(data, is.numeric)
                      numeric_col_names <- names(data)[numeric_cols]
                      numeric_data <- data[, ..numeric_col_names]
                      
                      bindat <- as.data.table(t(apply(numeric_data, 1, data_bin_2)))
                      
                      # Use the numeric column names instead of datcols
                      colnames(bindat) <- numeric_col_names
                      bindat <- bindat[, ..allgenes]
                      colnames(bindat) <- sapply(colnames(bindat), function(a) paste0(a,'_binary',collapse = ''))
                      newdat <- cbind(newdat, bindat)
                    }

                    if ('ranks' %in% self$data_mode) {
                      rankdat <- as.data.table(t(apply(data,1,rank)))
                      colnames(rankdat) <- datcols
                      rankdat <- rankdat[, ..allgenes]
                      colnames(rankdat) <- sapply(colnames(rankdata), function(a) paste0(a,'_ranked',collapse = ''))
                      newdat <- cbind(newdat, rankdat)
                    }
                    
                    if ('namedpairs' %in% self$data_mode) {
                      # if mode includes 'pairs' then we need to make var-pairs
                      if (is.null(self$pair_list)) {
                        stop('Error: pairlist not found.')
                      }
                      pair_list_format <- gsub(' ', '_', self$pair_list)
                      if (!all(pair_list_format %in% colnames(data))) {
                        print("ERROR: pair_list must be column names in data.")
                        print(pair_list_format)
                        stop(paste0('pair_list contains invalid value'))
                      }
                      if (length(self$pair_list) %% 2 != 0) {
                        print("Pair list must have an even length.")
                        stop('pair_list has invalid format or length.')
                      }
                      newcol_names <- c()
                      newcol_dat <- list()
                      cols <- pair_list_format 
                      for (ci in seq.int(from=1,to=length(self$pair_list),by=2)) {
                          cj <- ci+1
                          res0 <- as.numeric(data[,.SD,.SDcols=cols[ci]] > data[,.SD,.SDcols=cols[cj]])
                          this_new_col <- paste0(cols[ci],'_X_', cols[cj])
                          newcol_names <- c(newcol_names, this_new_col)
                          newcol_dat[[this_new_col]] <- res0
                      }
                      pairdat <- data.table(data.frame(newcol_dat))
                      newdat <- cbind(newdat, pairdat)
                    }

                    if ('allpairs' %in% self$data_mode) {
                      # if mode includes 'pairs' then we need to make var-pairs
                      if (!is.null(self$pair_list)) {
                        pair_list_format <- gsub(' ', '_', self$pair_list)
                        if (!all(pair_list_format %in% colnames(data))) {
                          print("ERROR: pair_list must be column names in data.")
                          print(pair_list_format)
                          stop(paste0('pair_list contains invalid value'))
                        }
                        subdat <- data[, pair_list_format, with=FALSE]  ## subset to only what's in the pair_list
                      } else {
                        subdat <- data
                      }
                      newcol_names <- c()
                      newcol_dat <- list()
                      cols <- colnames(subdat)
                      for (ci in 1:(length(cols)-1)) {
                        for (cj in (ci+1):length(cols)) {
                          res0 <- as.numeric(subdat[,.SD,.SDcols=ci] > subdat[,.SD,.SDcols=cj])
                          this_new_col <- paste0(cols[ci],'_X_', cols[cj])
                          newcol_names <- c(newcol_names, this_new_col)
                          newcol_dat[[this_new_col]] <- res0
                        }
                      }
                      allpairdat <- data.table(data.frame(newcol_dat))
                      newdat <- cbind(newdat, allpairdat)
                    }
                    
                    if ('sigpairs' %in% self$data_mode) {
                      if (is.null(self$signatures)) {
                        stop("Signatures is null in data eng obj. Please include signatures.")
                      }
                      # list of the new columns added to the data
                      newcol_names <- c()
                      # the new data frame we're building
                      newcol_dat <- list()
                      # names of the signatures
                      sig_names <- names(self$signatures)
                      # for each pair of sig_names
                      for (ci in 1:(length(sig_names)-1)) {
                        for (cj in (ci+1):length(sig_names)) {
                          # fix the names and get the features
                          sn1 <- sig_names[ci]
                          sn2 <- sig_names[cj]
                          s1 <- gsub(' ', '_', self$signatures[[sn1]])
                          s2 <- gsub(' ', '_', self$signatures[[sn2]])
                          sig_pair_temp <- list()
                          
                          # Find which signature genes are actually in the data
                          idx <- which(colnames(data) %in% s1)
                          jdx <- which(colnames(data) %in% s2)
                          
                          # Create sig_group based on ACTUAL genes present, not full signature
                          sig_group = c( rep.int('a', length(idx)), rep.int('b', length(jdx)))
                          
                          ### New sig fun ###
                          compute_roc <- function(sig_group, idx, jdx, a) {
                            val_group <- c( as.numeric(a[idx]), as.numeric(a[jdx]) )
                            roc_obj <- ROCit::rocit(val_group, sig_group)
                            return(roc_obj$AUC)
                          }
                          
                          sig_pair_temp <- apply(data, 1, FUN = function(a) compute_roc(sig_group, idx, jdx, a))
                          ###    ###    ###
                          
                          this_new_col <- paste0(sig_names[ci],'_X_', sig_names[cj])
                          newcol_names <- c(newcol_names, this_new_col)
                          newcol_dat[[this_new_col]] <- as.numeric(sig_pair_temp)
                          print(paste0("sigpair new column: ", this_new_col ))
                        }
                      }
                      sigpairs_dat <- data.table(data.frame(newcol_dat))
                      newdat <- cbind(newdat, sigpairs_dat)
                    }
              
                    return(newdat)
                  } # end data_eng
                ) # end public
)
                  

