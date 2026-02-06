
# R6 object


#' Recm  Robust ensemble classifier machine (Recm)
#' 
#' An object that holds data and an ensemble of classifiers
#' The ensemble is composed of XGBoost classifiers trained on binarized labels.
#'
#' 
#' @export
Robencla <- R6Class("Robencla",
                  public = list(
                    
                    #' @field name the object's name
                    name = NULL,
                    
                    #' @field data_mode string vector, dictates the types of data engineeering, acceptable values include combination of: original  quartiles  pairs  ranks  sigpairs
                    data_mode = NULL,

                    #' @field op_mode string describing the operation mode 'data', 'train', 'test'
                    op_mode = NULL,
                    
                    #' @field signatures lists of variables, must be in data, that will be used together, and compared to other signatures
                    signatures = NULL,
                    
                    #' @field pair_list a list of column names in the data, that will become paired data, named list or vector
                    pair_list = NULL,
                    
                    #' @field file_name the data file
                    file_name = NULL,
                    
                    #' @field label_name column name containing the label in the training data
                    label_name = NULL,
                    
                    #'  @field sample_id the data column used to identify samples
                    sample_id = NULL,
                    
                    #'  @field sample_ids to save the sample_ids
                    sample_ids = NULL,
                    
                    #'  @field train_sample_ids the sample IDs used in training data
                    train_sample_ids = NULL,

                    #'  @field test_sample_ids the sample IDs used in test data
                    test_sample_ids = NULL,
                    
                    #'  @field train_index the index into training data to subsample, mostly for CV
                    train_index = NULL,
                    
                    #'  @field test_index the index into test data to subsample, mostly for CV
                    test_index = NULL,
                    
                    #' @field cv_rounds the number of cross-validation rounds, values greater than 1 overrides data_split.
                    cv_rounds=1,
                    
                    #' @field sample_prop numeric value indicating proportion of samples for each ensemble member
                    sample_prop = NULL,
                    
                    #' @field feature_prop numeric value indicating proportion of features to each ensemble member
                    feature_prop = NULL,

                    #' @field data_split numeric value indicating proportion of features to each ensemble member
                    data_split = NULL,
                                        
                    #' @field train_data the data.table used to train the ensemble
                    train_data = NULL,
                    
                    #' @field train_label the vector used as the training label
                    train_label = NULL,
                    
                    #'@field test_data the data.table used as test data
                    test_data = NULL,
                    
                    #' @field test_label the vector used as test data labels
                    test_label = NULL,

                    #' @field data_colnames the column names used to train the models 
                    data_colnames = NULL,
                    
                    #' @field unique_labels the unique set of labels
                    unique_labels = NULL,
                    
                    #' @field ensbl the ensemble of predictors
                    ensbl = list(),
                    
                    #' @field call_table the table of predictions, collecting from the ensbl list of predictors
                    call_table = NULL,
                    
                    #' @field pred_table the table of predictions, collecting from the ensbl list of predictors
                    pred_table = NULL,
                    
                    #' @field cv_results the table of results over all samples.
                    cv_results = NULL,
                    
                    #' @field cv_importance the list of importance from each fold.
                    cv_importance = NULL,
                    
                    #' @field combine_function function for combining across ensemble
                    combine_function = NULL,
                    
                    #' @field verbose turn off warnings
                    verbose = NULL,
                    
                    #' @description Create a new `Recm` object.
                    #' @param name The object is named.
                    #' @return A new `recm` object.
                    initialize = function(name = NA) {
                      self$name <- name
                      #self$greet()
                    },
                    
                    #' @description
                    #' Returns the robencla version.
                    #' @return A character string representing the package version.
                    version = function() {
                      return("0.5.5")
                    },
                    
                    
                    #' @description
                    #' Creates a printable representation of the object.
                    #' @return A character string representing the object.
                    greet = function() {
                      cat(paste0("Hello, this model is named ", self$name, ".\n"))
                    },
                    
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    read_train_data = function(file_name, sep, header) {
                      self$file_name <- file_name
                      self$train_data <- data.table::fread(file=file_name, sep=sep, header=T)
                      colnames(self$train_data) <- gsub(" ", "_", colnames(self$train_data))
                      return(invisible(self))
                    },
                    
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    #' @param header boolean, whether the table has a header line
                    read_test_data = function(file_name, sep, header) {
                      self$file_name <- file_name
                      self$test_data <- data.table::fread(file=file_name, sep=sep, header=T)
                      colnames(self$test_data) <- gsub(" ", "_", colnames(self$test_data))
                      return(invisible(self))
                    },

                    
                    
                    #' @description Splits the data into training and test data.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_split_fun = function(data_split) {
                      
                      if (is.null(data_split)) {
                        stop("Missing sample_prop in params list!")
                      }
                      
                        # to split the data into training and test components
                        n <- nrow(self$train_data)
                        idx <- sample.int(n = n, size=data_split*n)
                        jdx <- setdiff( (1:n), idx)
                        self$train_index <- idx
                        self$test_index <- jdx
                        
                      return()
                    },
                    

                    
                    #' @description Does some setup processing on the training data file, drop columns and identify the label column.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    train_data_setup = function(data_frame=NULL,
                                                file_name=NULL, 
                                                sep=NULL, 
                                                data_mode=NULL, 
                                                signatures=NULL, 
                                                pair_list=NULL,
                                                label_name=NULL, 
                                                sample_id=NULL, 
                                                drop_list=NULL,
                                                verbose=NULL){
                      #READING DATA
                      self$file_name <- file_name
                      self$data_mode <- data_mode
                      self$signatures <- signatures
                      self$pair_list <- pair_list
                      self$sample_id <- sample_id
                      self$label_name <- label_name
                      self$verbose <- verbose
                      
                      print('starting train data setup')

                      # assume the file format
                      if (!is.null(file_name)) {
                        if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                          sep = ','
                        }
                        else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                          sep = '\t'
                        } else if (!is.null(file_name) & is.null(sep)) {
                          stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                        }
                      }
                      
                      # read in the data or convert to a data.table
                      if (is.null(data_frame) & !is.null(file_name)) {
                        thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      } else if (!is.null(data_frame) & is.null(file_name)) {
                        colnames(data_frame) <- gsub("\\.","_",colnames(data_frame))
                        thisdata <- as.data.table(data_frame)
                      } else {
                        stop('Specify only ONE of data_frame or file_name.')
                      }
                      
                      # reorder the rows randomly
                      self$train_data <- thisdata[sample(nrow(thisdata)),]
                      
                      # fix any spaces in the column names
                      colnames(self$train_data) <- gsub(" ", "_", colnames(self$train_data))
                      
                      # remove label from data
                      if (is.null(label_name)) {
                        stop("Make sure label_name is not null!")
                      } else {
                        if (!label_name %in% colnames(self$train_data)) {
                          stop('Make sure the label name matches one of the columns!')
                        } else {
                          label_name <- gsub(' ', '_', label_name)
                          self$train_label <- sapply(self$train_data[[label_name]], as.character)
                          #set(self$train_data, j = label_name, value = NULL)
                          #self$train_data[, ..label_name := NULL]
                        }
                      }
                      
                      # remove sample ID column from data
                      if ((!is.null(sample_id)) && sample_id %in% colnames(self$train_data)) {
                        self$sample_id <- gsub(' ', '_', sample_id)
                        self$train_sample_ids <- sapply(self$train_data[[self$sample_id]], as.character)
                        #set(self$train_data, j = self$sample_id, value = NULL)  # then del it
                        #self$train_data[, ..sample_id := NULL]
                      } else {
                        self$train_sample_ids <- 1:nrow(self$train_data)
                      }
                      
                      # remove any other data variables
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in%  colnames(self$train_data)))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        #set(self$train_data, j = drop_list, value = NULL)
                        #self$train_data[, ..drop_list := NULL]
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }

                      # if we have some columns that have zero variance, fix that
                      data_var <- suppressWarnings(  ## columns with text become NAs
                                    self$train_data[, lapply(.SD, var, na.rm=TRUE)] )
                      data_var_idx <- which(data_var == 0.0)
                      if (length(data_var_idx) > 0) {
                        if (is.null(verbose) || verbose > 0) {
                          print("TRAINING DATA CONTAINS ZERO VARIANCE COLUMNS")
                          print("...filling with random noise...")
                        }
                        for (dvi in data_var_idx) {
                          self$train_data[,dvi] <- runif(n=nrow(self$train_data))
                        }
                      }
                      
                      # save the final data columns used
                      self$data_colnames <- colnames(self$train_data)
                      
                      # and record the unique categories in labels 
                      self$unique_labels <- unique(self$train_label)

                      # DATA ENGINEERING
                      # self$data_eng('train')
                      self$op_mode <- 'train'

                      print('finish train data setup')

                      return(invisible(self))
                    },
                    
                    
                    #' @description Does some setup processing on the test data file, drop columns and identify the label column.
                    #' The data_mode and signatures will have already been set in training.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    test_data_setup = function(data_frame=NULL,
                                               file_name=NULL, 
                                               sep=NULL, 
                                               label_name=NULL, 
                                               sample_id=NULL, 
                                               drop_list=NULL,
                                               verbose=NULL){
                      
                      allgenes <- gsub(" ", "_", c(unlist(self$pair_list), unlist(self$signatures)))
                      
                      if (!is.null(file_name)) {
                        if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                          sep = ','
                        }
                        else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                          sep = '\t'
                        } else if (!is.null(file_name) & is.null(sep)) {
                          stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                        }
                      }
                      
                      # read in the data or convert to a data.table
                      if (is.null(data_frame) & !is.null(file_name)) {
                        thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      } else if (!is.null(data_frame) & is.null(file_name)) {
                        colnames(data_frame) <- gsub("\\.","_",colnames(data_frame))
                        thisdata <- as.data.table(data_frame)
                      } else {
                        stop('Specify only ONE of data_frame or file_name.')
                      }
                      
                      self$test_data <- thisdata[sample(nrow(thisdata)),]
                      # replace spaces with underscores
                      colnames(self$test_data) <- gsub(" ", "_", colnames(self$test_data))

                      ### have to remove class
                      if (is.null(label_name)) {
                        self$test_label <- NULL  # don't have to have labels to make calls.
                      } else {
                        if (!label_name %in% colnames(self$test_data)) {
                          stop('Make sure the label name matches one of the columns!')
                        } else {
                          label_name <- gsub(' ', '_', label_name)
                          self$test_label <- sapply(self$test_data[[label_name]], as.character)
                          #set(self$test_data, j = label_name, value = NULL)
                          #self$test_data[, ..label_name := NULL]
                        }
                      }
                      
                      # remove column containing sample IDs
                      if ((!is.null(sample_id))) { # already know that the col names match
                        test_sample_id <- gsub(' ', '_', sample_id)
                        self$test_sample_ids <- sapply(self$test_data[[test_sample_id]], as.character)
                        #set(self$test_data, j = test_sample_id, value = NULL)  # then del it
                        #self$test_data[, ..sample_id:=NULL]
                      } else {
                        self$test_sample_ids <- 1:nrow(self$test_data)
                      }
                      
                      # remove any other variables.
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% colnames(self$test_data)))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        #set(self$test_data, j = drop_list, value = NULL)
                        #self$test_data[, ..drop_list := NULL]
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }
                      
                      if ( (self$label_name %in% colnames(self$test_data)) ) {
                        #set(self$test_data, j = self$label_name, value = NULL)
                        #self$test_data[, ..label_name := NULL]
                      }
                      
                      # check that the data column names are the same as used in training
                      if (! all(allgenes %in% colnames(self$test_data)) ) {
                        stop('Test data column names must match what was used in training.')
                      } else {
                        # just subset to the proper columns now
                        #self$test_data
                        self$test_data <- self$test_data[, allgenes, with = FALSE]
                      }

                      # if we have some columns that have zero variance, fix that
                      data_var <- self$test_data[, lapply(.SD, var, na.rm=TRUE)]
                      data_var_idx <- which(data_var == 0.0)
                      if (length(data_var_idx) > 0) {
                        if (is.null(verbose) || verbose > 0) {
                          print("TEST DATA CONTAINS ZERO VARIANCE COLUMNS")
                          print("...filling with random noise...")
                        }
                        for (dvi in data_var_idx) {
                          self$test_data[,dvi] <- runif(n=nrow(self$test_data))
                        }
                      }
                      
                      # DATA ENGINEERING
                      # self$data_eng('test')
                      self$op_mode <- 'test'

                      return(invisible(self))
                    },

                    
                    # takes the label, and returns a vector of 0s and 1s
                    binarize_label = function(label, x) {
                      if (is.numeric(x)) {
                        x <- as.character(x)
                      }
                      new_label <- ifelse(label == x, yes=1, no=0)
                      return(new_label)
                    },
                    
                    
                    #' @description 
                    #' Builds list of ensembles of XGBoost object, each classifying one binary label.
                    #' @param params list, The main set of parameters.
                    #'
                    #' @details A list of classifiers, each trained on a random sample of the training data.
                    #'
                    #' @return A ensemble object is added to the list of objects in recm$enbl.
                    #'
                    #'
                    build_label_ensemble = function(params) {
                      
                      print('starting ensemble build')

                      # for each category
                      for (li in self$unique_labels) {
                        
                        print(paste0('subtype ',li))

                        # first create the binarized label
                        bin_label <- self$binarize_label(label=self$train_label, x=li)
                                                         
                        # then create the classifier object
                        ## !! pair list is either a named list or a vector
                        if (class(self$pair_list) == "list") {
                          this_pair_list <- self$pair_list[[li]]
                        } else {
                          this_pair_list <- self$pair_list
                        }

                        self$ensbl[[li]] <- Ensemble$new(name=li, 
                                                         obj_mode='ensemble',
                                                         size=params$size, 
                                                         data_mode=self$data_mode,
                                                         train_data=self$train_data,
                                                         pair_list=this_pair_list,
                                                         signatures=self$signatures,
                                                         label=bin_label, 
                                                         params=params)

                        self$ensbl[[li]]$data_eng(self$op_mode)
                        
                      }

                      print('finished ensemble build')
                      
                      return(invisible(self))
                    },
                    
                    
                    
                    build_pred_table = function() {
                      final_train_data <- list()
                      for (li in self$unique_labels) {
                        final_train_data[[li]] <- self$ensbl[[li]]$pred_combined
                      }
                      
                      self$pred_table <- do.call(cbind.data.frame, final_train_data)
                      
                      return(invisible(self))
                    },
                    
                    
                    remap_multiclass_labels = function(label) {
                      mapper <- list() # first we construct a mapper function
                      idx <- 0
                      for (li in self$unique_labels) { 
                        mapper[[li]] <- idx
                        idx <- idx+1
                      }
                      new_labels <- sapply(label, function(a) mapper[[a]])
                      return(new_labels)
                    },
                    
                    
                    unmap_multiclass_labels = function(labels) {
                      new_labels <- c()
                      for (li in labels) { 
                        new_labels <- c(new_labels, self$unique_labels[(li+1)])
                      }
                      return(new_labels)
                    },
                    
                    
                    build_final_ensemble = function(params) {
                      # here the ensemble will be trained on the prior predictions
                      # for each category, get the predictions
                      # turn that into a dataframe / matrix
                      print('building final ensemble')
                      self$build_pred_table()

                      remapped_label <- self$remap_multiclass_labels(self$train_label)

                      # train a XGBoost that takes multiple labels.
                      self$ensbl[["final"]] <- Ensemble$new(name="final",
                                                         obj_mode="final",
                                                         size=params$size, 
                                                         data_mode=self$data_mode,
                                                         train_data=self$pred_table,
                                                         pair_list=c(),
                                                         signatures=c(),
                                                         label=remapped_label, 
                                                         params=params
                                                    )

                      print('finished building final ensemble')
                      return(invisible(self))
                    },
                    
                    
                    train_models = function() {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$train_models()
                      }
                      return(invisible(self))
                    },
                    
                    
                    train_final = function() {
                      self$ensbl[['final']]$train_models()
                      return(invisible(self))
                    },
                    
                    
                    ensemble_setup = function(combine_function) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$member_predict(
                          as.matrix(self$ensbl[[li]]$train_data))
                      }
                      return(invisible(self))
                    },
                    
                    
                    ensemble_predict = function(data) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$test_data <- data # can't be matrix until after data eng
                        self$ensbl[[li]]$data_eng('test')
                        self$ensbl[[li]]$member_predict(self$ensbl[[li]]$test_data)
                      }
                      return(invisible(self))
                    },
                    
                    
                    # predict final uses predictions from predict_ensemble
                    final_predict = function(data) {
                      self$ensemble_predict(data)
                      self$build_pred_table()

                      # then we should have a new pred_table from the data
                      pred_matrix <- as.matrix(self$pred_table)
                      
                      self$ensbl[['final']]$member_predict(as.matrix(pred_matrix))
                      return(invisible(self))
                    },
                    

                    print_error = function(label, root, threshold) {
                      if (all(class(label) == 'numeric') == FALSE) {
                        label <- as.numeric(label)
                      }
                      new_label <- self$binarize_label(label, root)
                      self$ensbl[[root]]$print_error(new_label, threshold)
                      return(invisible(self))
                    },
                    
                    
                    #' @description 
                    #' Returns a table of classification metrics, one row per label.
                    #' @param these_labels vector, the classification labels.
                    #' @param these_calls vector, the predicted calls.
                    #' @param use_cv_results boolean, use the cross validation results or internal test set
                    #'
                    #' @details Returns various metrics associated with machine learning performance.
                    #'
                    #' @return A table of classification metrics for each label and overall.
                    #'
                    #'
                    classification_metrics = function(labels=NULL, calls=NULL, use_cv_results=FALSE ) {
                    
                      if (use_cv_results) {
                        these_calls  <- self$cv_results$BestCalls
                        these_labels <- self$cv_results$Label
                      } 
                      else if ( (!is.null(calls)) && (!is.null(labels)) ) {
                        these_labels <- labels
                        these_calls  <- calls
                      } else {
                        if (is.null(self$test_label)) {
                          return("No test labels.")
                        } else {
                          # first make sure our labels are mapped to integers correctly
                          these_labels <- self$test_label
                          # then get the calls
                          mapped_calls <- self$ensbl[['final']]$pred_combined
                          these_calls  <- self$unmap_multiclass_labels(mapped_calls)
                        }
                      }
                      
                      metrix <- Metrics$new(these_labels, these_calls)
                      
                      return(metrix$compute_metrics())
                    },
                    

                    #' @description 
                    #' Returns a table of feature importance, one list element per label.
                    #'
                    #' @details Returns a table of feature importance, one list element per label.
                    #'
                    #' @return A table of information gain for each feature pair per class.
                    #'
                    #'
                    importance = function() {
                      # for each ensembl 
                      resList <- list()
                      for (i in 1:length(self$ensbl)) {
                        impList <- list()
                        ei <- self$ensbl[[i]]
                        # for each booster in the ensemble member
                        for (j in 1:ei$size) {
                          impList <- rbind(impList, xgb.importance(model=ei$bstl[[j]]))
                        }
                        # then we have a set of tables, and they might or 
                        # might not have the same feature names. Assume they
                        # do not. Also don't have the same sizes.
                        resList[[ei$name]] <- impList %>% 
                          group_by(Feature) %>% 
                          summarise(MedianGain=median(Gain),
                                    MedianCover=median(Cover),
                                    MedianFreq=median(Frequency)) %>%
                          arrange(desc(MedGain))
                      }
                      # now one item per ensemble
                      return(resList)
                    }, 
                    
                    
                    #' @description 
                    #' Returns a table of results on the prediction, one row per example.
                    #'
                    #' @details Returns various metrics associated with machine learning performance.
                    #'
                    #' @return A table of classification metrics for each label and overall.
                    #'
                    #'
                    results = function() {
                      
                      # get the calls, labeled numerically as 0 to n labels
                      mapped_calls <- self$ensbl[['final']]$pred_combined
                      # map calls to the feature names, the mapped calls are indices to unique labels
                      calls <- self$unmap_multiclass_labels(mapped_calls)
                      # build a data frame with calls and sample ids
                      df <- data.frame(SampleIDs = self$test_sample_ids, BestCalls=calls)
                      # norm each row to add to 1.0
                      norm_pred_table <- (self$pred_table) / rowSums(self$pred_table)
                      # add the labels into the final calls table
                      df <- cbind(df, norm_pred_table)
                      # if there's labels available, include them!
                      if (!is.null(self$test_label)) {
                        df <- cbind(df, data.frame(Label=self$test_label))
                      }
                      return(df)
                    },
                    
                    
                    # Run CV or specify a split
                    autocv = function(  data_frame=NULL,
                                        data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        cv_rounds=1,
                                        data_mode=NULL,
                                        signatures=NULL,
                                        pair_list=NULL,
                                        params=NULL,
                                        verbose=NULL
                                      ) {
                      
                      params[['objective']] <- "binary:logistic"
                      params[['eval_metric']] <-'logloss'
                      final_params <- params
                      final_params[['objective']] <- 'multi:softmax'
                      final_params[['eval_metric']] <- 'mlogloss'
                      
                      self$combine_function=params$combine_function
                      
                      if (is.null(data_frame)) {
                        data_frame <- data.table::fread(file=data_file, sep=sep, header=T)
                        data_frame <- as.data.frame(data_frame)
                      }
                      
                      self$op_mode <- 'cv'
                      
                      for (cvi in 1:cv_rounds){
                        
                        print(paste0("*** CV Round ", cvi, " ***"))
                        
                        # SPLIT DATA round 1
                        self$data_split_fun(params$sample_prop) 
                        
                        data_train_table <- data_frame[self$train_index,]
                        data_test_table  <- data_frame[self$test_index, ]
                        
                        self$train(data_frame=data_train_table,
                                   data_file=NULL,
                                   sep,
                                   label_name,
                                   sample_id,
                                   drop_list,
                                   data_mode,
                                   signatures,
                                   pair_list,
                                   params,
                                   verbose)
                        
                        self$test(data_frame=data_test_table,
                                  data_file=NULL,
                                  sep,
                                  label_name,
                                  sample_id,
                                  drop_list,
                                  verbose)
                        
                        
                        # capture the feature importance from each fold
                        self$cv_importance[[cvi]] <- self$importance()
                        
                        # append the final results 
                        if (cvi > 1) {
                          self$cv_results = rbind(self$cv_results, 
                                                  self$results())
                        } else {
                          self$cv_results = self$results()
                        }
                        
                      } # done with CV
                    
                    },# end autopred
                    
                    
                    # Train a classifier
                    train = function(data_frame=NULL,
                                     data_file=NULL,
                                     sep=NULL,
                                     label_name=NULL,
                                     sample_id=NULL,
                                     drop_list=NULL,
                                     data_mode=NULL,
                                     signatures=NULL,
                                     pair_list=NULL,
                                     params=NULL,
                                     verbose=NULL
                    ) {
                      
                      params[['objective']] <- "binary:logistic"
                      params[['eval_metric']] <-'logloss'
                      final_params <- params
                      final_params[['objective']] <- 'multi:softmax'
                      final_params[['eval_metric']] <- 'mlogloss'
                      
                      self$combine_function=params$combine_function
                    
                      # perform the data set up
                      self$train_data_setup(data_frame=data_frame,
                                            file_name=data_file,
                                            sep=sep,
                                            data_mode=data_mode,
                                            signatures=signatures,
                                            pair_list=pair_list,
                                            label_name=label_name, 
                                            sample_id=sample_id,
                                            drop_list=drop_list,
                                            verbose=verbose)
                      
                      # building the first layer of predictors, each a binary prediction
                      # on one factor in the target labels.
                      # training and making predictions on the training data
                      self$build_label_ensemble(params=params)$
                        train_models()$
                        ensemble_setup()  ## not ensemble_predict?
                      
                      # then we build the output layer, trained on the predictions of the first layer
                      self$build_final_ensemble(final_params)$
                        train_final()
                      
                      return(invisible(self))
                    }, # end autotrain
                    
                    
                    # make predictions on a new data set
                    # after running autotrain()
                    predict = function(data_frame=NULL,
                                        data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        verbose=NULL) {
                    
                      self$test_data_setup(data_frame=data_frame,
                                      file_name=data_file, 
                                      sep=sep, 
                                      label_name=label_name, 
                                      sample_id=sample_id, 
                                      drop_list=drop_list,
                                      verbose = verbose)
                      
                      self$final_predict(self$test_data)
                  
                      return(invisible(self)) 
                    },
                    
                    
                    # Add to the Robencla class:
                    trim = function() {
                      # Clear top-level data
                      self$test_data <- NULL
                      self$train_data <- NULL
                      self$test_label <- NULL
                      self$train_label <- NULL
                      self$test_sample_ids <- NULL
                      self$train_sample_ids <- NULL
                      self$pred_table <- NULL
                      self$call_table <- NULL
                      
                      # Clear data from each ensemble member
                      for (ens_name in names(self$ensbl)) {
                        ens <- self$ensbl[[ens_name]]
                        if (!is.null(ens)) {
                          ens$train_data <- NULL
                          ens$test_data <- NULL
                          ens$label <- NULL
                          ens$pred_table <- NULL
                          ens$preds <- NULL
                          ens$pred_combined <- NULL
                        }
                      }
                      
                      invisible(self)
                    }
                  ) # end public
      )


