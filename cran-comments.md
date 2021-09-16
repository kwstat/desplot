# desplot 1.8

## test environments

* local R 4.0.3 Windows 10
* WinBuilder devel
* Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Rhub Ubuntu Linux 16.04 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results

One NOTE:

```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2983638
    From: inst/doc/desplot_examples.html
    Status: 403
    Message: Forbidden
```

This URL works fine in a web browser. Please advise if changes are needed.

## Downstream dependencies

Reverse suggests:	agridat

Checked OK.


# --- desplot 1.7 ---

## test environments

* local R 4.0.2 Windows 10
* Rhub Ubuntu Linux 16.04, R-release
* Rhub Windows Server 2008, R-devel
* Rhub Fedora Linux, R-devel

## R CMD check results

Status: OK

## Downstream dependencies

Reverse suggests:	agridat

Checked OK.


# --- desplot 1.6 ---

## test environments

* local R 3.6.1 Windows 7
* win-builder R 3.6.1
* win-builder R 3.5.3
* R-hub Debian Linux, R-devel, GCC

## R CMD check results

One NOTE:

```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2983638
    From: inst/doc/desplot_examples.html
    Status: 403
    Message: Forbidden
```

This URL works fine in a web browser. Please advise if changes are needed.

## Downstream dependencies

Reverse suggests:	agridat

Checked OK.



# --- desplot 1.5 ---

## test environments

* local R 3.5.3 Windows 7
* win-builder R 3.6.0 alpha
* win-builder R 3.5.3

## R CMD check results

One NOTE:

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2983638
    From: inst/doc/desplot_examples.html
    Status: 403
    Message: Forbidden

This URL works fine in a web browser. Please advise if changes are needed.

## Downstream dependencies

Reverse suggests:	agridat

Checked OK.



# --- desplot 1.3 ---

## test environments

* local R 3.4.2 Windows 7
* win-builder r-release 3.4.2
* win-builder r-devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.



# --- desplot 1.2 ---

## test environments

* local R 3.4.0 Windows 7
* win-builder R 3.4.1
* win-builder r-devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.



# --- desplot 1.1 ---

## test environments

* local R 3.3.2 on Windows 7
* win-builder R 3.3.2
* R-hub Debian Linux, R-devel, GCC

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.



# --- desplot 1.0 ---

This is a new package submission.  The functions in this package are being
split off from the 'agridat' package so that the latter can be a data-only
package.

## test environments

* local R 3.2.3 on Windows 7
* win-builder release
* win-builder devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.


