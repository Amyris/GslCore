# Genotype Specification Language (GSL) Core library

Amyris domain specification language for rapidly specifying genetic designs

Documentation in the repo is sparse currently, but you can find

* the scientific paper describing the language here http://pubs.acs.org/doi/abs/10.1021/acssynbio.5b00194
* GSL documentation as part of the Autodesk genetic constructor tool here https://geneticconstructor.readme.io/docs/genotype-specification-language
* the press release on the GSL / Autodesk collaboration here http://investors.amyris.com/releasedetail.cfm?ReleaseID=992005

This library provides all core modules of the compiler, template code for constructing an application, and a set of plug-ins providing basic core functionality.

## Build instructions

* make sure you have a current dotnet sdk installed (6 or higher)
* if your version doesn't match `global.json` update the global.json 
* `./build.sh build`
