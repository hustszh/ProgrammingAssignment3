best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    #获取输入outcome所对应的列序号
    outcome_col_idx <- valid_outcome_df[valid_outcome_df$name==outcome,2]
    #获取输入state所对应的{hosptial(2),state(7),outcome}数据框，
    #并过滤outcome列中值为NA的数据
    valid_data <- data[data$State==state & !is.na(data[,outcome_col_idx]),
                       c(2,7,outcome_col_idx)]
    #获取outcome列值最小时所在的行号
    min_row_idx <- which.min(as.numeric(valid_data[,3]))
    #获取对应的医院名称
    hosptial <- valid_data[min_row_idx, 1]
    #排序
    sort(hosptial)
}