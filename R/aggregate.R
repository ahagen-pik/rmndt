#' Internal function to apply the weights and perform some checks.
#'
#' @param data a data.table.
#' @param mapping a mapping between the aggregated categories and their parts. *All* aggregated categories in `data` have to be part of the mapping.
#' @param weights table with weights for disaggregation, the name of the column with the aggregated categories has to be `manycol`. If columns (other than the column with the aggregated category) of the `weights` coincide with columns of the data, the respective columns are considered when joining.
#' @param fewcol name of the column containing aggregated categories. Default is "region".
#' @param manycol name of the column containing dis-aggregated categories. Default is "iso".
#' @param valuecol name of the column with the actual value to disaggregate, default is `value`.
#' @param datacols index columns that label categories which have to be treated seperately when dis-aggregating with a weight.
#' @param weightcol column with the weights for the dis-aggregation, default is `weight`.
#' @import data.table

apply_weights <- function(data, mapping, weights, fewcol, manycol, valuecol, datacols, weightcol){
        
        diff <- setdiff(unique(mapping[[manycol]]), unique(weights[[manycol]]))
        if(length(diff)){
            warning("The weights are incomplete. ",
                    "Some dis-aggregated categories are found in the mapping, but not in the weights: ",
                    paste(diff, collapse=", "))
        }

        ## we are only interested in the matching cols and the weight col
        inboth <- intersect(colnames(data), colnames(weights))

        ## are there other dimensions to consider when applying the weights?
        othercols <- setdiff(inboth, manycol)

        ## leftjoin data: look up the row of the matching weight and add the weight column by reference,
        ## this avoids a copy of the weights and of the data
        idx <- weights[data, on=c(manycol, othercols), which=TRUE]
        if(length(idx) != nrow(data)){
            ## duplicated weights: keep the original join, which multiplies the data rows
            weights <- weights[, c(inboth, weightcol), with=F]
            data <- weights[data, on=c(manycol, othercols)]
        } else {
            set(data, j=weightcol, value=weights[[weightcol]][idx])
            ## same column order as the original join weights[data]
            setcolorder(data, c(inboth, weightcol))
        }

        ## if there are NAs in the weights, the weights were incomplete along the additional dimension
        if(any(is.na(data[[weightcol]]))){
            warning("NAs are found when joining the weights. ",
                    "The weights are incomplete along the following dimension(s):",
                    paste(othercols, collapse=", "))
        }

        ## apply weights
        data[, (valuecol) := get(valuecol)*get(weightcol)/sum(get(weightcol)), by=c(fewcol, othercols, datacols)]
        data[, (weightcol) := NULL]
    
}


#' Disaggregate data in a data.table object using a mapping.
#' If no weights are given, the value for the aggregated categories is used on the disaggregated ones.
#' If a weight is given, the values from the aggregated categories are distributed according to the weights.
#'
#' @param data a data.table.
#' @param mapping a mapping between the aggregated categories and their parts. *All* aggregated categories in `data` have to be part of the mapping.
#' @param fewcol name of the column containing aggregated categories. Default is "region".
#' @param manycol name of the column containing dis-aggregated categories. Default is "iso".
#' @param valuecol name of the column with the actual value to disaggregate, default is `value`.
#' @param datacols index columns that label categories which have to be treated seperately when dis-aggregating with a weight.
#' @param weights table with weights for disaggregation, the name of the column with the aggregated categories has to be `manycol`. If columns (other than the column with the aggregated category) of the `weights` coincide with columns of the data, the respective columns are considered when joining.
#' @param weightcol column with the weights for the dis-aggregation, default is `weight`.
#' @import data.table
#' @export

disaggregate_dt <- function(data, mapping,
                     fewcol="region",
                     manycol="iso",
                     valuecol="value",
                     datacols="data",
                     weights=NULL,
                     weightcol="weight"){
    ## Note that isocol in the data has to match the column name in the mapping
    mapping <- mapping[, c(manycol, fewcol), with=F]

    ## require the mapping to be a superset of the regions in data
    diff <- setdiff(unique(data[[fewcol]]), mapping[[fewcol]])
    if(length(diff)){
        stop("Mapping is incomplete. Missing aggregated categories: ", paste(diff, collapse=", "))
    }
    ## disaggregation function
    data <- mapping[data, on=c(fewcol), allow.cartesian=T]

    if(!is.null(weights)){
        data <- apply_weights(data, mapping, weights, fewcol, manycol, valuecol, datacols, weightcol)
    }
    data[, (fewcol) := NULL]
    return(data)
}

#' Aggregate values in a data.table object using a mapping.
#' If no weight is given, the value for the aggregated categories is the sum of the parts.
#' Otherwise, the weight is used to calculate a weighted average accross the parts.
#'
#' @param data, a magpie object.
#' @param mapping, a mapping between the aggregated categories in the data and ISO3 countrycodes. *All* regions in `data` have to be part of the mapping.
#' @param fewcol, name of the column containing aggregated categories. Default is "region".
#' @param manycol, name of the column containing dis-aggregated categories. Default is "iso".
#' @param yearcol, name of the column containing time step info. Default is "year".
#' @param valuecol name of the column with the value to aggregate, default is `value`.
#' @param datacols index columns that label categories which have to be treated seperately when aggregating with a weight.
#' @param weights table with weights for a (weighted average) aggregation, the name of the column with the aggregated categories has to be `manycol`. If columns (other than the column with the aggregated category) of the `weights` coincide with columns of the data, the respective columns are considered when joining.
#' @param weightcol column with the weights for aggregation, default is `weight`.
#' @import data.table
#' @export

aggregate_dt <- function(data, mapping,
                         fewcol="region",
                         yearcol="year",
                         manycol="iso",
                         datacols="data",
                         valuecol="value",
                         weights=NULL,
                         weightcol="weight"){

    ## aggregation function, sums by default
    ## alternatively, do a weighted average
    mapping <- mapping[, c(manycol, fewcol), with=F]

    ## left join: only regions in the mapping are mapped
    data <- mapping[data, on=c(manycol)]

    ## require the mapping to be a superset of the countries in data
    diff <- setdiff(unique(data[[manycol]]), mapping[[manycol]])
    if(length(diff)){
        warning("Mapping is incomplete. Data for the following ISO countries is omitted: ", paste(diff, collapse=", "))
        data <- data[!is.na(get(fewcol))]
    }

    if(!is.null(weights)){
        data <- apply_weights(data, mapping, weights, fewcol, manycol, valuecol, datacols, weightcol)
    }
    ## sum
    data[, (valuecol) := sum(get(valuecol), na.rm=T), by=c(yearcol, fewcol, datacols)]

    # drop manycol
    data[, (manycol) := NULL]
    # drop duplicate rows
    data <- unique(data)
    return(data)
}

