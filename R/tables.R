#' A function returning  a Death table
#'
#' @param population Population dataframe where first colunms are birth and death.
#' @param ages A vector of size N composed of age groups.
#' The function computes the number of death  in each
#' age group [ages[i],ages[i+1][, for i in 1..N-1.
#' @param times A vector of size M composed of time intervals.
#' The function computes the number of death in each time interval [t[j],t[j+1][, j=1..M.
#'
#'
#' @return A death table matrix
#' 
#' @examples 
#' dth_table <- death_table(EW_pop_out,0:101,0:31)
#'
#' @export
death_table<-function(population, ages,  times){
    N <- length(ages)
    M <- length(times)
    ## Filter only dead peole
    df_dead <- filter(population,!is.na(death)& (death >= times[1]) & (death <= times[M])
                      & (death-birth >=ages[1]) & (death - birth < ages[N]))
    # Add age group at death time and death time in year group
    df_dead <- df_dead %>% transmute(age = ages[findInterval(death - birth, ages[1:(N-1)])], d_time = times[findInterval(death,times[1:(M-1)])])
    # Complete with all possible couples (age,d_time) before cast
    df_temp <- data.frame(df_dead %>% group_by(age,d_time) %>% summarise(value=n())) %>%
        complete(age=ages[1:(N-1)],d_time=times[1:(M-1)],fill=list(value=0)) # Complete with age groupes in which there are no individuals

    # Count number of death for each couple (age,d_time)
    dth_table <- cast(df_temp, age~d_time)
    return(data.matrix(dth_table[,-1]))

}

exposure <- function(c_i, d_i, a, t, a_step=1,t_step=1){
    a_i = pmax(t, c_i+a)
    b_i = pmin(t+t_step, c_i+a+a_step, d_i,na.rm = TRUE)
    return(pmax(0, b_i-a_i,na.rm = TRUE))
}

#' A function returning an Exposure table
#'
#' @param population Population dataframe where the first two columns are birth and death.
#' @param ages A vector of size N composed of age groups.
#' The function computes the central Exposure-to-risk in each
#' age group [ages[i],ages[i+1][, for i in 1..N-1, and time intervals.
#' @param times A vector of size M composed of time intervals.
#' The function computes the central exposure-to-risk in each time interval [t[j],t[j+1][, j=1..M, and age groups.
#'
#' @return An exposure matrix
#' 
#' @examples 
#' ex_table <- exposure_table(EW_pop_out,0:101,0:2)
#'
#' @export
exposure_table <- function(population,ages,times){
    N <- length(ages)
    M <- length(times)
    Ex_table <- matrix(rep(0,(N-1)*(M-1)), nrow = N-1)
    for (i in 1:(M-1)){ # for each time compute sum of individual risk exposures
        df_alive <- filter(population,(is.na(death) | (death > times[i])) & (ages[1] <=times[i]-birth) & (ages[N] >times[i]-birth))
        t_step <- times[i+1]-times[i]
        Ex_table[,i] <- sapply(1:(N-1), function(j)(
            sum(population %>%  transmute(n = exposure(birth,death,ages[j],times[i],ages[j+1]-ages[j],t_step))
            )))
    }
    return(Ex_table)
}
