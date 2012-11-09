if (require(rptR))
{
  rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
      datatype = "Gaussian", method = "ANOVA")
  ## R  = 0.748
  rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
      datatype = "Gaussian", method = "corr")
  ## R  = 0.754
  rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
      datatype = "Gaussian", method = "REML")
  ## R  = 0.748
} else {
  stop("Please install the package: rptR\n
       install.packages('rptR', repos='http://R-Forge.R-project.org')")
}

if (require(ICC))
{
  ICCbare(specimen, femur.length, WalkingStickFemurs)
  ## [1] 0.7475028
} else {
  stop("Please install the package: ICC")
}
