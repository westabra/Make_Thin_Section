# Third-Party Notices

This file records third-party material incorporated into or adapted by
Make_Thin_Section.

## GCDkit

Project: Geochemical Data Toolkit (GCDkit)  
Project website: https://www.gcdkit.org/  
Upstream license: GNU General Public License, version 2 or later
(`GPL-2.0-or-later`; the GCDkit package declares `GPL (>= 2)`)

The numerical field-coordinate tables used to draw and classify the plutonic
and volcanic QAPF diagrams in Make_Thin_Section were adapted from the GCDkit
QAPF implementations (`QAPFPlut.r` and `QAPFVolc.r`). The surrounding Shiny
user interface, data preparation, point-in-polygon classification, plotting
workflow, and export integration are implemented as part of Make_Thin_Section.

Relevant citation:

Janousek, V.; Farrow, C. M.; Erban, V. (2006). Interpretation of whole-rock
geochemical data in igneous geochemistry: introducing Geochemical Data Toolkit
(GCDkit). *Journal of Petrology*, 47(6), 1255-1259.
https://doi.org/10.1093/petrology/egl013

Make_Thin_Section is distributed under the GNU General Public License, version
3 or later (`GPL-3.0-or-later`), which is compatible with the upstream
`GPL-2.0-or-later` terms.

## R packages

R packages used at runtime are installed separately by the supplied dependency
installer scripts. They are not copied into this repository and retain their
respective authorship and licenses. Consult each installed package's
`DESCRIPTION` file for its license terms.
