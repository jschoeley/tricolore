This is a re-submission of the package `tricolore` (https://cran.r-project.org/web/packages/tricolore/index.html) removed from CRAN on 2023-05-07 along with its dependency `ggtern`. As `ggtern` is available again, `tricolore` can return to CRAN as well. NOTEs have been fixed.

## Test environments

* Linux Mint 21.2, R 4.4.0
* MacOS 14.4.1, R 4.4.0
* Ubuntu 22.04.4, R devel
* Ubuntu 22.04.4, R 4.4.0
* Ubuntu 22.04.4, R 4.3.3
* Windows Server 2022 10.0.20348, R 4.4.0

## R CMD check results

The following changes proposed by Konstanze Lauseker have been
implemented:

- "Please do not start the description with the title, "This package",
package name, or similar."
  - FIXED: Updated and expanded description.
- "Please proofread your DESCRIPTION. It currently reads: continous
  I believe it should be: continuous"
  - FIXED: Fixed typo.
- "If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file"
  - FIXED: Added references to DESCRIPTION in stated format.
- Please omit one colon. (triple colon)
  - FIXED: Following Uwe Ligges recommendation I've added a note to each
  documentation example where I use the triple colon stating:
  "NOTE: only intended for internal use and not part of the API"
