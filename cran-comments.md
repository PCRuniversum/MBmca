## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

# Fixes in this submission

- The package was removed some time ago because of dependencies to the chipPCR package. The later is back on CRAN.
- This submission includes fixes to the DESCRIPTION
    - omit the redundant "with R"
    - The Description field no longer starts with the package name, 'This package' or similar.
    - LaTeX markup such as "--" removed
    - Rather than full citation, link via web reference added

# check_for_cran()
─  Building package
─  Uploading package
─  Preparing build, see status at
   https://builder.r-hub.io/status/MBmca_1.0.1.tar.gz-051681bbc80d4eb7b755e9129ca56121
   https://builder.r-hub.io/status/MBmca_1.0.1.tar.gz-041bc4521e4f4f45b65cbd5e04ce1744
   https://builder.r-hub.io/status/MBmca_1.0.1.tar.gz-3683860d2f0b4bf6a421fb0a9ba24cfe
─  Build started
─  Creating new user
─  Downloading and unpacking package file
─  Querying package dependencies
─  Installing package dependencies
─  Running R CMD check
   setting _R_CHECK_FORCE_SUGGESTS_ to false
   setting R_COMPILE_AND_INSTALL_PACKAGES to never
   setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
   setting R_REMOTES_STANDALONE to true
   setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
   setting _R_CHECK_FORCE_SUGGESTS_ to true
   setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
─  using log directory 'C:/Users/USERKDuIEsaASU/MBmca.Rcheck'
─  using R Under development (unstable) (2021-02-15 r80013)
─  using platform: x86_64-w64-mingw32 (64-bit)
─  using session charset: ISO8859-1 (908ms)
─  using option '--as-cran'
✔  checking for file 'MBmca/DESCRIPTION'
─  checking extension type ... Package
─  this is package 'MBmca' version '1.0.1' (807ms)
N  checking CRAN incoming feasibility
   Maintainer: 'Stefan Roediger <stefan_roediger@gmx.de>'
   New submission
   
   
   Package was archived on CRAN
   Possibly mis-spelled words in DESCRIPTION:
   
     microbead (20:62)
     qPCR (22:23)
   
   CRAN repository db overrides:
     X-CRAN-Comment: Archived on 2020-12-16 as requires archived package
       'chipPCR'.
   
   The Description field contains
     https://journal.r-project.org/archive/2015-1/RJ-2015-1.pdf].
     https://journal.r-project.org/archive/2013-2/roediger-bohm-schimke.pdf;
   Please enclose URLs in angle brackets (<...>).
✔  checking package namespace information (826ms)
✔  checking package dependencies
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files (806ms)
✔  checking for hidden files and directories
✔  checking for portable file names
✔  checking serialization versions
✔  checking whether package 'MBmca' can be installed (1.8s)
✔  checking installed package size
✔  checking package directory
✔  checking for future file timestamps
✔  checking 'build' directory (1s)
✔  checking DESCRIPTION meta-information
✔  checking top-level files
✔  checking for left-over files
✔  checking index information (804ms)
✔  checking package subdirectories
✔  checking R files for non-ASCII characters
✔  checking R files for syntax errors
✔  checking whether the package can be loaded (807ms)
✔  checking whether the package can be loaded with stated dependencies
✔  checking whether the package can be unloaded cleanly
✔  checking whether the namespace can be loaded with stated dependencies
✔  checking whether the namespace can be unloaded cleanly (812ms)
✔  checking loading without being on the library search path
✔  checking use of S3 registration
✔  checking dependencies in R code
✔  checking S3 generic/method consistency (868ms)
✔  checking replacement functions
✔  checking foreign function calls
✔  checking R code for possible problems
✔  checking Rd files (807ms)
✔  checking Rd metadata
✔  checking Rd line widths
✔  checking Rd cross-references
✔  checking for missing documentation entries (1.6s)
✔  checking for code/documentation mismatches
✔  checking Rd \usage sections
✔  checking Rd contents
✔  checking for unstated dependencies in examples (814ms)
✔  checking contents of 'data' directory
✔  checking data for non-ASCII characters
✔  checking data for ASCII and uncompressed saves
N  checking sizes of PDF files under 'inst/doc' (806ms)
   Unable to find GhostScript executable to run checks on size reduction
✔  checking installed files from 'inst/doc'
✔  checking files in 'vignettes'
✔  checking examples (4s)
✔  checking for unstated dependencies in 'tests'
─  checking tests (808ms)
✔  Running 'spelling.R'
✔  checking for unstated dependencies in vignettes
✔  checking package vignettes in 'inst/doc' (788ms)
✔  checking re-building of vignette outputs (39.1s)
✔  checking PDF version of manual (5.3s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
─  Done with R CMD check
─  Cleaning up files and user
