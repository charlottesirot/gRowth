\name{isoSignature.creatR}
\alias{isoSignature.creatR}
\description{
\code{isoSignature.creatR aims to create the file containing the final isotope signature. To this aimn, the user has the choice to set up its session (e.g. selection of the standards, samples, background value). This function also allows to have a proper normalization 1/ according to the standards or 2/ according to the GelA. Finally, it will automatically create the isotope signature file.}
}
\usage{
isoSignature.creatR()
}

\note{
WARNING: Before uploading any file in isoSignature.creatR, please check with the CNEA_cleanR that the raw CN_EA file does NOT contain any error in required CONCLUSIONS (this task may require several run of utilisation of the function TCD_cleanR). If the CN_EA file is upload without this last verification, isoSignature.creatR CANNOT garantee the validity of the following data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\alias{CNEA.cleanR}
}
