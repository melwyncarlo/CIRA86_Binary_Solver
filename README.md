# CIRA86 Binary Solver

### COSPAR International Reference Atmosphere (CIRA) 1986 Model

<br>This is a **Fortran Free-Form** version of the original fixed-form COSPAR/CIRA (1986) data driver.
<br>This program reads the VAX/VMS-styled binary database files (&#42;.dat).

It is meant to interpolate based on the required input, and output the results accordingly.
<br>However, the binary files, themselves, are a bit challenging to interpret.

The original driver file **cirat.for**, unfortunately, produces incorrect results.
<br>Also, it's confusing to read, given its age-old fixed-form style reserved for the punch card system.

The file, **cira86.f90**, is of free-form style, easy to read. The executable generated is interactive.

The **data** file contains the readable contents of one of the binary data files, named **ch11.dat**.
<br>Reading through that data file would help me comprehend the original data, and how I need to interpret it.
<br>Haven't completely succeeded yet. Some data seems arbirtrary; need to make some sense of it.

The CIRA-86 data files (both binary and ASCII formats) and driver can be downloaded from the following 
<br>FTP website: [CCMC, GSFC, NASA - CIRA-86 Atmospheric Model](https://ccmc.gsfc.nasa.gov/pub/modelweb/atmospheric/cira/)

Documents describing the CIRA-86 atmospheric model, the procedure involved in data accumulation, 
<br>and illustrating graphs and tables can be found in the link below:
<br>
<br>[[1]  Monthly Mean Global Climatology of Temperature, Wind, Geopotential Height, and Pressure for 0-120 km,
<br>by E. Fleming, S. Chandra, et. al., in February 1988](https://artefacts.ceda.ac.uk/badc_datadocs/cira/fleming.pdf)
<br>
<br>[[2]  Middle Atmosphere Program. Handbook for MAP. Volume 16: Atmospheric Structure and Its Variation in the
<br>Region 20 to 120 Km. Draft of a New Reference Middle Atmosphere, K. Labitzke, J. Barnett, 
<br>and B. Edwards, in July 1985](https://ntrs.nasa.gov/citations/19860003346)
