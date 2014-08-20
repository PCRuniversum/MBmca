\name{DMP}
\alias{DMP}
\docType{data}
\title{ Bimodal melting curve experiment on the surface of microbeads. }

\description{ A melting curve experiment with six microbead populations and 
short oligonucleotide probes (direct hybridization). Detection probes for 
human VIM (vimentin), MLC-2v (myosin regulatory light chain 2, 
ventricular/cardiac muscle isoform), SERCA2 (Atp2a2 - ATPase, 
Calcium-transporting ATPase sarcoplasmic reticulum type, slow twitch 
skeletal muscle isoform), HRPT1 (hyperparathyroidism 1) and the artificial 
sequences Poly(dA)20 (20 nt of dA) and aCS (artificial Control Sequence). }

\usage{data(DMP)}
\format{
  A data frame with the melting curves of six different capture and 
detection probe pairs on six microbeads populations for VIM, MLC-2v, 
SERCA2, Poly(dA)20, aCS and HPRT1. First column contains the temperature 
(in degree Celsius, 1 degree Celsius per step) followed by melting curves 
of VIM, MLC-2v, SERCA2, Poly(dA)20, aCS and HPRT1 with bimodal melting 
pattern. The dyes and quencher used were Atto 647N and BHQ2.
  \describe{
    \item{\code{T}}{a numeric vector, Temperature in degree Celsius.}
    
    \item{\code{VIM.no.Q}}{a numeric vector, VIM without quencher and 
without Poly(dT)20 region.}
    
    \item{\code{MLC2v.w.Q.w.dT20}}{a numeric vector, MLC-2v with 
quencher-labeled detection probe and fluorescent Poly(dA)20 detection 
probe.}
		
    \item{\code{SERCA2.no.Q.w.dT20}}{a numeric vector, SERCA2 without 
quencher-labeled detection probe and Poly(dA)20 detection probe.}
				      
    \item{\code{PolydA20.w.Q}}{a numeric vector, Poly(dT)20 with 
fluorescent Poly(dA)20 detection probe (quencher labeled).}
			
    \item{\code{aCS.w.Q.w.dT20}}{a numeric vector, artificial Control 
Sequence without quencher-labeled detection probe and fluorescent 
Poly(dA)20 detection probe.}
				 
    \item{\code{HPRT1.no.Q.w.dT20}}{a numeric vector, HPRT1 without 
quencher-labeled detection probe and fluorescent Poly(dA)20 detection 
probe.}
  }
}
\source{ Data were measured with the VideoScan platform:

A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:35--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

Surface Melting Curve Analysis with R. S. Roediger, A. Boehm and I. 
Schimke. \emph{The R Journal}. 5(2):37--52, 2013. 
\url{http://journal.r-project.org/}
}

\seealso{ \code{\link{MFIerror}}, \code{\link{mcaSmoother}}, 
\code{\link{diffQ}}, \code{\link{diffQ}}, \code{\link{MultiMelt}}, 
\code{\link{DualHyb}}}

\examples{
data(DMP)
}

\keyword{datasets}
