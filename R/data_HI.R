#' Height inventory dataset
#'
#' @description (\code{HI}) dataset consists of responses of 4885 respondents
#'   (1479 males, 3406 females) to Height Inventory (HI). It contains 26 ordinal
#'   items, vector of self-reported heights (in centimeters), and vector of
#'   gender membership.
#' @usage data(HI)
#'
#' @author Recka, K. & Cigler, H. \cr Masaryk University
#'
#' @references Recka, K., Cigler, H. (2020). Height Inventory: A psychological
#' attempt at measuring body height. Unpublished manuscript.
#'
#' @keywords datasets
#'
#' @format \code{HI} is a \code{data.frame} consisting of 4885 observations on
#'   the 28 variables. First 26 variables are responses on scale
#'   \code{"1"} strongly disagree, \code{"2"} disagree, \code{"3"} agree,
#'   \code{"4"} strongly agree. Original item number and English wording is
#'   provided below.
#'   \describe{
#'   \item{ShortTrousers}{1. A lot of trousers are too short for me. }
#'   \item{TallerThanM}{2.	I am taller than men of my age.}
#'   \item{TallerThanF}{3.	I am taller than women of my age.}
#'   \item{HeightForBasketball}{4.	I have an appropriate height for playing basketball or volleyball.}
#'   \item{AskMeToReach}{5.	Sometimes other people ask me to reach something for them.}
#'   \item{CommentsTall}{6.	I am used to hearing comments about how tall I am.}
#'   \item{ConcertObstructs}{7.	At concerts, my stature usually obstructs other people’s views.}
#'   \item{ShortBed}{8.	Ordinary beds are too short for me.}
#'   \item{TopShelfEasy}{9.	I can easily take wares from top shelves at a store.}
#'   \item{CrowdViewComf}{10.	In a crowd of people, I still have a comfortable view.}
#'   \item{ShortBlanket}{11.	Blankets and bedspreads rarely cover me completely.}
#'   \item{BendToHug}{12.	When I want to hug someone, I usually need to bend over.}
#'   \item{CarefullHead}{13.	Often, I must be careful to avoid bumping my head against a doorjamb or a low ceiling.}
#'   \item{NotSmallerThanM}{14.	I am smaller than men of my age.}
#'   \item{NotStoolNeeded}{15.	I often need a stool to reach something other people would reach without it. }
#'   \item{NotPlayDwarf}{16.	I could play a dwarf.}
#'   \item{NotSmallerThanW}{17.	I am smaller than women of my age.}
#'   \item{NotNoticeSmall}{18.	One of the first things people notice about me is how small I am.}
#'   \item{NotOnTipToes}{19.	I often need to stand on the tip of my toes to get a better view.}
#'   \item{NotClothChildSize}{20.	When I buy clothes, children’s sizes often fit me well.}
#'   \item{NotBusLegsEnoughSpace}{21.	I have enough room for my legs when traveling by bus.}
#'   \item{NotFasterWalk}{22.	I often need to walk faster than I’m used to in order to keep pace with taller people.}
#'   \item{NotAgeUnderestim}{23.	Because of my smaller stature, people underestimate my age.}
#'   \item{NotWishLowerChair}{24.	It would be more comfortable for me if chairs were made lower.}
#'   \item{NotUpwardLook}{25.	When talking to other adults, I have to look upwards if I want to meet their eyes.}
#'   \item{NotMirrorTooHigh}{26.	Some mirrors are placed so high up that I have to crane my neck to use them.}
#'   \item{gender}{Gender membership, \code{"M"} males, \code{"F"} females. }
#'   \item{HeightCM}{Self-reported height in centimeters. }
#' }
"HI"
