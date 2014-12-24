rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data，将"Not Available"转化为NA
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character",
                     na.strings="Not Available")
    
    ## Check that state and outcome are valid
    valid_state <- unique(data[,"State"])
    if ( is.na(state) | !state %in% valid_state ) {
        stop("invalid state")
    }
    
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
    
    #获取输入state所对应的{hosptial(2),outcome}数据框，
    #并过滤outcome列中值为NA的数据
    valid_data <- data[data$State==state & !is.na(data[,outcome_col_idx]),
                       c(2,outcome_col_idx)]
    
    #对数据进行排序
    order_idx <- order(as.numeric(valid_data[,2]), valid_data[,1])
    order_data <- valid_data[order_idx,]
    rank_data <- cbind(order_data, c(1:nrow(order_data)))
    names(rank_data) <- c("Hosptial.Name","Rate","Rank")
    
    ## Return hospital name in that state with the given rank 30-day death rate
    if ( num == "best" )
        rank_data[1,1]
    else if ( num == "worst")
        rank_data[nrow(rank_data),1]
    else if ( num <= nrow(rank_data) )
        rank_data[num,1]
    else
        NA
}