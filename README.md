# StrathE2E2
R package containing the Strath End-to-end ecosystem model.

## General Requirements

StrathE2E2 runs on the following platforms:

* Mac OS X
* Linux (64-bit)
* Windows (64-bit, Windows 10 recommended)

with the following minimum runtime:

* R 3.6 or later
* R packages deSolve, NetIndices

## Installation

There is a source package for Linux and binary packages for Windows 10 and Mac OS X in our [CRAN](https://cran.r-project.org/) compatible [R package repository](https://marineresourcemodelling.gitlab.io/sran/index.html).

To install StrathE2E2, use install.packages() function and specify our repository:
```
install.packages("StrathE2E2", repos="https://marineresourcemodelling.gitlab.io/sran")
```

which will install either the source or binary package (depends on your OS).

## Quick Start

To run the model:

```
library(StrathE2E2)
model <- e2e_read("North_Sea","1970-1999")
results <- e2e_run(model, nyears=5)
e2e_plot_ts(model,results)
```

Within R, type *help.start()* and navigate to the package documentation for various user and technical guides,

## Building from source

The source package can be built on all platforms, and is the only way to install on Linux.

Download latest version from [Strath E2E2 GitLab repository](https://gitlab.com/MarineResourceModelling/StrathE2E/StrathE2E2) (use the repository download button).

In addition to the standard requirements, some additional R packages are required to build the package documentation (vignettes):

  * knitr
  * rmarkdown

For each platform there are some additional software requirements - see the individual platform build sections below.

### Linux build

Full requirements for Linux build:

| Requirement	| URL/Comment					|
|---------------|-----------------------------------------------|
| R		| https://cran.r-project.org/bin/windows/	|
| R packages	| deSolve, NetIndices, knitr, rmarkdown		|
| pandoc	| https://pandoc.org/				|
| Texlive	| https://www.tug.org/texlive/			|

Usually Linux distributions will have everything either already installed, or available for install via the system package manager.

At a terminal prompt, try:
```
> R CMD INSTALL StrathE2E2-master.tar.gz
```

The package will be installed in your local R library folder (often ~/R).

### Windows build

Full requirements for Windows build:

| Requirement	| URL/Comment					|
|---------------|-----------------------------------------------|
| R		| https://cran.r-project.org/bin/windows/	|
| R packages	| deSolve, NetIndices, knitr, rmarkdown		|
| Rtools	| https://cran.r-project.org/bin/windows/Rtools	|
| pandoc	| https://pandoc.org/				|
| MikTeX	| https://miktex.org/				|
| pdfpages      | https://ctan.org/pkg/pdfpages                 |

Install *Rtools* as administrator and make sure the box to *Add rtools to the system PATH* is checked.

Add the *R* bin folder to the system PATH as well. Go to *Control Panel*->*System and Security*->*System* then select *Advanced system settings*. Click on the *Environment Variables* button edit the *PATH* variable, adding the path to the *R* bin folder, something like:
```
> C:\Program Files\R\R-3.6.1\bin
```

If you upgrade *R* at a later date you must remember to update this *PATH* setting as well.

In a *cmdtool* navigate to working folder where the unpacked source code resides in a sub-folder.

Start R and run:
```
> library(devtools)
> document("StrathE2E2")
```

Then on command line:
```
> R CMD build StrathE2E2
> R CMD INSTALL --build StrathE2E2_3.2.0.tar.gz
```

This will build the binary package *StrathE2E2_3.2.0.zip*


### MacOS X build

Full requirements for MacOS build:

| Requirement	| URL/Comment					|
|---------------|-----------------------------------------------|
| R		| https://cran.r-project.org/bin/macosx		|
| R packages:	| deSolve, NetIndices, knitr, rmarkdown		|
| X11 Quartz	| https://www.xquartz.org/			|
| pandoc	| https://pandoc.org/				|
| MacTeX	| https://www.tug.org/mactex			|

make sure to restart any *terminal* applications after installing these packages.

Within a *Terminal*, unpack the downloaded source package and install/build:
```
tar zxvf StrathE2E2_3.2.0.tar.gz
R CMD INSTALL --build StrathE2E2
```

If the command line developer tools are missing you should accept the offer of installing them.

A successful build will install the package in R, and there will also be a binary package file *StrathE2E2_3.2.0.tgz* created.

