\name{CNEA.cleanR}
\alias{CNEA.cleanR}
\alias{isoSignature.creatR}

\description{
Help the user to check his/her CNEA file in order to be able to apply the \alias{isoSignature.creatR} function to create the sigbature file. CNEA cleaner will check for requirments: 1/ no line are empty, and 2/ every line has at least one signature for d13C and d15N. These mistakes are required to be corrected. Secondary conditions (3/ Analysis should have exactly one dilution value and 4/ Every analysis must begin with 3 or 4 d15N/d14N and last with 2 values of d13C/d12C)

}
\usage{
CNEA.cleanR()
}

\note{
WARNING: The aim of CN_EA.CleanR is NOT to correct automatically the CN_EA file, this delicated task requiring human skills. However, this function is dedicated to point out errors or abnormalities of the raw CN_EA file. When the first round of correction has been finished and before uploading any file in isoSignature.creatR, please check a last time with CN_EA.CleanR that there is NO error in the required CONCLUSIONS (this task may require several rounds of CN_EA.CleanR).
}

\seealso{
\alias{isoSignature.creatR}
}
\examples{
CNEA.cleanR()
}

