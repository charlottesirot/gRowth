\name{TCD.cleanR}
\alias{TCD.cleanR}
\alias{Quant.creatR}
\description{
TCD.cleanR helps the user to check his/her TCD file in order to apply the \alias{Quant.creatR} and to create the "quant file". TCD.cleanR will check for the following requirments: 1/ no analysis that has no name (identifi), 2/ Peak.Nr..Flash.TCD column accepts only the following values: 1, 2, NA or empty cell, 3/ for standards, rArea Flash TCD column needs exactly three lines containing exactly two values and an empty cell (NA accepted) (the analysis of the sucrose is an exception (exactly two lines containing exactly one values and an empty cell), and for samples, 4/ all samples should have a position in the box and rArea Flash TCD with exactly three lines containing exactly two values and an empty cell (NA accepted).
}
\usage{
TCD.cleanR()
}
\note{
WARNING: The aim of TCD.cleanR is NOT to correct automatically the TCD file, this delicated task requiring human skills. However, this function is dedicated to point out errors or abnormalities of the raw TCD file. When the first round of correction has been finished and before uploading any file in quant.creatR, please check a last time with TCD.cleanR that there is NO error in the required CONCLUSIONS (this task may require several rounds).
}

