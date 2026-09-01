# Introduction

This is a small library to generate vector graphics in common lisp, inspired by the excellent TikZ library in LaTeX.
It is primarily an little project to learn CL, so there will certainly be deficiencies. 
Like TikZ it is oriented to the production of technical diagrams, or other diagrams where there is benefit to having precise, mathematical control over the components. 
The hope is to also add some primitive support for generating interactive svg elements as well. 


The library is 3D aware, in the sense that it operates natively in 3D space, of which 2D drawing is a special case. 
It supports 3D transformations and multiple viewports, as well as both isometric and orthographic projections. 
Back-face culling and centroid based depth ordering is also supported. 
Note though that there is no support for "true" 3D rendering like OpenGL; there is no clipping or per-fragment visibility. 

There is support for TeX math equations, implemented by dispatching to an installed latex distribution and the using dvisvgm to convert the equation to an SVG path, which may be used by the system.
Naturally, this requires a working LaTeX installation and dvisvgm.


Right now the library has much of the "core" functionality implemented. 
There are a number of visual primitives implemented, such as circles, rectangles, faces, labels, and paths.
These paths can be composed and references to construct complex diagrams.
There are a number of higher level drawing functions, including ones for producing parametrized paths and surfaces in 3D.


However, at the moment the library is not fully flushed out, and also importantly it is not particularly ergonomic.
I hope to finish up the core functionality, such as grid and tree layouts, and also add a convince DSL and other helper functions to produce common structures.





