#' Synthetic Longitudinal MOS-SS Social Support Survey Data
#'
#' A synthetic dataset containing responses to the eight MOS-SS emotional
#' domain items measured at two time points (baseline and one-year follow-up),
#' together with baseline demographic and clinical covariates. The data are
#' provided for testing and demonstration of the longitudinal GRMTree
#' functions (\code{\link{longitudinal_grmtree}}, \code{\link{rs_characterize}})
#' within the grmtree package.
#'
#' The data were simulated from a two-factor longitudinal graded response model.
#' Response shift was intentionally built into the older subgroup (age > 61):
#' the \code{MOS_Info} item exhibits reprioritization (a discrimination change
#' over time) and the \code{MOS_Fears} item exhibits recalibration (a threshold
#' change over time). Younger patients show no response shift. As a result,
#' \code{\link{longitudinal_grmtree}} produces an age split, and
#' \code{\link{rs_characterize}} detects response shift within the older
#' subgroup. Items are coded 1--5, representing None of the time, A little of
#' the time, Some of the time, Most of the time, and All of the time,
#' respectively.
#'
#' @format A tibble with 1,500 rows and 23 variables:
#' \describe{
#'   \item{MOS_Listen}{Baseline: someone you can count on to listen when you need to talk (1--5)}
#'   \item{MOS_Info}{Baseline: someone to give you information to help you understand a situation (1--5)}
#'   \item{MOS_Advice_Crisis}{Baseline: someone to give good advice in a crisis (1--5)}
#'   \item{MOS_Confide}{Baseline: someone to confide in or talk to about yourself or your problems (1--5)}
#'   \item{MOS_Advice_Want}{Baseline: someone whose advice you really want (1--5)}
#'   \item{MOS_Fears}{Baseline: someone to share your private worries or fears (1--5)}
#'   \item{MOS_Personal}{Baseline: someone to turn to for suggestions about a personal problem (1--5)}
#'   \item{MOS_Understand}{Baseline: someone who understands your problems (1--5)}
#'   \item{MOS_Listen_year1}{One-year follow-up: listen item (1--5)}
#'   \item{MOS_Info_year1}{One-year follow-up: information item (1--5)}
#'   \item{MOS_Advice_Crisis_year1}{One-year follow-up: crisis advice item (1--5)}
#'   \item{MOS_Confide_year1}{One-year follow-up: confide item (1--5)}
#'   \item{MOS_Advice_Want_year1}{One-year follow-up: advice wanted item (1--5)}
#'   \item{MOS_Fears_year1}{One-year follow-up: fears item (1--5)}
#'   \item{MOS_Personal_year1}{One-year follow-up: personal suggestions item (1--5)}
#'   \item{MOS_Understand_year1}{One-year follow-up: understand item (1--5)}
#'   \item{sex}{Sex (Male, Female)}
#'   \item{age}{Age in years (numeric)}
#'   \item{residency}{Residence location (Urban, Rural)}
#'   \item{job}{Employment status (Employed, Unemployed)}
#'   \item{education}{Education level (Primary/High school, College/University)}
#'   \item{ever_smoker}{Ever-smoker status (Yes, No)}
#'   \item{comorbidity_count}{Number of chronic conditions (0--5)}
#'   \item{bmi}{Body mass index (numeric)}
#' }
#'
#' @examples
#' library(grmtree)
#' library(dplyr)
#'
#'   # Load the synthetic longitudinal data and take a glimpse
#'   data("grmtree_long_data", package = "grmtree")
#'   glimpse(grmtree_long_data)
#'
#' @source Synthetic data generated from a two-factor longitudinal graded
#'   response model for package testing and demonstration. Contains no real
#'   patient data.
"grmtree_long_data"
