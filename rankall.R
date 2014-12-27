rankall <- function(outcome, num = "best") {
    ## Read outcome data，将"Not Available"转化为NA
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character",
                     na.strings="Not Available")
    
    ## Check that outcome are valid
    #构造outcome和对应列序号的数据框
    valid_outcome_df <- data.frame(name=c("heart attack", "heart failure", "pneumonia" ),
                                   index=c(11,17,23),
                                   stringsAsFactors=FALSE)
    if ( is.na(outcome) | !outcome %in% valid_outcome_df[,1] ) {
        stop("invalid outcome")
    }
    
    #check input num validation
    if ( !num %in% c("best","worst" ) & !is.numeric( num ) ) {
        stop("invalid num")
    }
    
    #获取输入outcome所对应的列序号
    outcome_col_idx <- valid_outcome_df[valid_outcome_df$name==outcome,2]
    
    #获取输入outcome所对应的{hosptial(2),state(7),outcome}数据框，
    #并过滤outcome列中值为NA的数据
    valid_data <- data[!is.na(data[,outcome_col_idx]), c(2,7,outcome_col_idx)]
    names(valid_data) <- c("hospital", "state", "rate")
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    #按照州对数据进行分割
    split_data <- split(valid_data, valid_data[,2])
    
    #对split_data中的每一个子列表，进行排序（先outcome，再hospital）
    order_idx <- lapply(split_data, function(x) order(as.numeric(x[,3]), x[,1]))
    
    order_data <- list()
    for ( i in 1:length(split_data) ) {
        order_data <- c(order_data, list(split_data[[i]][order_idx[[i]],]))
    }
    
    output_data <- sapply(order_data, function(x, num) {
        df <- as.data.frame(x)
        row_num <- nrow(df)
        
        if ( num == "best" ) {
            c(df[1,1], df[1,2])
        }
        else if ( num == "worst" ) {
            c(df[row_num,1],df[row_num,2])
        }
        else {
            if ( row_num < num )
                c(NA, df[1,2])
            else
                c(df[num,1],df[num,2])
        }
    }, num )
    
    result_data <- as.data.frame(t(output_data))
    names(result_data) <- c("hospital","state")
    row.names(result_data) <- result_data$state
    
    result_data
}
