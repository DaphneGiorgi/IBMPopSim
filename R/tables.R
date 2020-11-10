#' Creates a death table.
#'
#' @description Creates a death table from a population data frame.
#' For each \code{i=1..N-1} and \code{j=1..M}, the number of individuals with age at last birthday in \code{[ages[i],ages[i+1])} and died in \code{[times[j],times[j+1])} is computed.
#'
#' @param population Population data frame containing at least \code{'birth'} and \code{'death'} columns.
#' @param ages A vector of size \code{N} composed of age groups.
#' The function computes the number of death  in each age group \code{[ages[i],ages[i+1])}, for \code{i=1..N-1}.
#' @param period A vector of size \code{M} composed of time intervals.
#'
#' @details The function computes the number of death in each time interval \code{[times[j],times[j+1])}, \code{j=1..M}.
#'
#' @return A death table matrix.
#'
#' @examples
#' dth_table <- death_table(EW_pop_out, 0:101, 0:31)
#'
#' @export
death_table<-function(population, ages,  period){
  if ( length(ages)<=1) {
    stop("Argument 'ages' must be of length at least 2")
  }
  if ( length(period)<=1) {
    stop("Argument 'times' must be of length at least 2")
  }
    N <- length(ages)
    M <- length(period)
    if (!'out' %in% colnames(population)){
    ## Filter only dead peole
    df_dead <- filter(population, !is.na(.data$death) &
                        (.data$death >= period[1]) &
                        (.data$death <= period[M]) &
                        (.data$death - .data$birth >= ages[1]) &
                        (.data$death - .data$birth < ages[N]))
    }
    else {
      df_dead <- filter(population, !is.na(.data$death) &
                              (.data$out ==FALSE) &
                              (.data$death >= period[1]) &
                              (.data$death <= period[M]) &
                              (.data$death - .data$birth >= ages[1]) &
                              (.data$death - .data$birth < ages[N]))
}

    # Add age group at death time and death time in year group
    df_dead <- df_dead %>%
      transmute(age = ages[findInterval(.data$death - .data$birth,
                                        ages[1:(N-1)])],
                d_time = period[findInterval(.data$death, period[1:(M-1)])])
    # Complete with all possible couples (age,d_time) before cast
    df_temp <- data.frame(df_dead %>% group_by(.data$age, .data$d_time) %>%
                            summarise(value=n())) %>%
      complete(age=ages[1:(N-1)], d_time=period[1:(M-1)], fill=list(value=0)) # Complete with age groupes in which there are no individuals

    # Count number of death for each couple (age,d_time)
    dth_table <- cast(df_temp, age ~ d_time)
    res <- data.matrix(dth_table[,-1])
    rownames(res)<- ages[1:(N-1)]
    colnames(res)<- period[1:(M-1)]
    return(res)
}

#' @keywords internal
exposure <- function(c_i, d_i, a, t, a_step=1,t_step=1,e_i=0){
  a_i = pmax(t, c_i+a,e_i,na.rm = TRUE)
  b_i = pmin(t+t_step, c_i+a+a_step, d_i,na.rm = TRUE)
  return(pmax(0, b_i-a_i,na.rm = TRUE))
}

#' Creates an exposure table.
#'
#' @description  Returns the Central Exposure-to-Risk for given ages groups and time period.
#' The central Exposure-to-risk is computed as the sum of the time spent by individuals in a given age group over a given period, where age is the age at last birthday.
#'
#' @inheritParams death_table
#'
#' @details The function computes the central exposure-to-risk in each time interval \code{[t[j],t[j+1])}, \code{j=1..M}, and age groups.
#'
#' @return An exposure matrix
#'
#' @examples
#' ex_table <- exposure_table(EW_pop_out,0:101,0:2)
#'
#' @export
exposure_table <- function(population,ages,period){
  if ( length(ages)<=1) {
    stop("argument 'ages' must be of length at least 2")
  }
  if ( length(period)<=1) {
    stop("argument 'times' must be of length at least 2")
  }
  N <- length(ages)
  M <- length(period)
  Ex_table <- matrix(rep(0,(N-1)*(M-1)), nrow = N-1)
  for (i in 1:(M-1)){ # for each time compute sum of individual risk exposures
    df_alive <- population_alive(population,period[i],ages[1],ages[N])
    t_step <- period[i+1]-period[i]
    if('entry' %in% colnames(population)){
      Ex_table[,i] <- sapply(1:(N-1), function(j)(
        sum(population %>%
              transmute(n = exposure(.data$birth, .data$death, ages[j], period[i],ages[j+1]-ages[j], t_step,.data$entry))
        )))
      
    }
    else {
      Ex_table[,i] <- sapply(1:(N-1), function(j)(
        sum(population %>%
              transmute(n = exposure(.data$birth, .data$death, ages[j], period[i],ages[j+1]-ages[j], t_step))
        )))
    }
  }
  rownames(Ex_table)<- ages[1:(N-1)]
  colnames(Ex_table)<- period[1:(M-1)]
  return(Ex_table)
}



#' A function returning a merged dataframe from a list of population dataframes with id.
#'
#' @param pop_df_list A list of population dataframe where the first three columns of each dataframe are id, birth and death.
#' @param char_ignored A vector of characteristics names which are only kept from the first element of pop_df_list.
#'
#' @return A dataframe composed of all individuals with their characteristics at each simulation time.
#'
#'
#' @export
merge_pop_withid <- function(pop_df_list, char_ignored = NULL){
  merge_pop <- pop_df_list %>% reduce(full_join, by = c("id","birth","death",char_ignored),all=TRUE)
  char_names <- names(pop_df_list[[1]])[!names(pop_df_list[[1]]) %in% c("id","birth","death",char_ignored)]
  n <- length(char_names)
  names(merge_pop) <- c("id","birth","death",char_ignored, paste(rep(char_names,length(pop_df_list)),rep(1:length(pop_df_list),each=n),sep="_"))
  return(merge_pop)
}


#' Returns a population of individuals alive.
#'
#' @param population A population data frame containing at least a column \code{'birth'} and \code{'death'}.
#' @param  t  A numeric indicating the time.
#' @param  a1  0 by default. All individuals of age over \code{a1} at \code{t} are selected.
#' @param  a2 Inf by default. All individuals of age below \code{a2} at \code{t} are selected.
#'
#'@return  The function returns a population data frame containing all individuals alive at time \code{t} and of age in \code{[a1,a2)}.
#'
#'@export
population_alive <- function(population, t, a1=0, a2=Inf) {
  
  if ('entry' %in% colnames(population)){
    df_alive <- filter(population, (is.na(.data$death) | (.data$death > t)) &
                          (t - .data$birth>=a1) &
                          (t - .data$birth< a2) &
                          (is.na(.data$entry) | (.data$entry <=t))
                         ) 
                       
                          
  }
  else{
    df_alive  <- filter(population, (is.na(.data$death) |
                                       (.data$death > t)) &
                          (t - .data$birth>=a1) &
                          (t - .data$birth< a2))
    
  }
  return(df_alive)

}
