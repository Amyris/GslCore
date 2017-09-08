#### 0.4.24 - September 8 2017
* Check for dnasrc pragma when promoting long slices to regular parts.

#### 0.4.23 - September 3 2017
* Linker specification file is comma-delimited.

#### 0.4.22 - June 25 2017
*  Support for seamless primer generation for non linkered designs

#### 0.4.21 - June 16 2017
* Allow single-part assemblies to act as promoters in level 2 titrations.

#### 0.4.20 - June 9 2017
* Use a controlled degree of parallelism in block processing.

#### 0.4.19 - April 21 2017
* Extend thumper primer limit to 80
* VS2017 build fix

#### 0.4.18 - April 10 2017
* Add ToString representations to core message types.
* Update FSharpCore to >= 4.1.0.

#### 0.4.17 - March 1 2017
* LNT: removing unused thumper proxy code out of core library

#### 0.4.16 - February 9 2017
* Fix indexing bug in ORF annotation.

#### 0.4.15 - February 9 2017
* Update to latest AmyrisBio for bug fixes.

#### 0.4.14 - February 8 2017
* using oligoDesignWithCompromise now for many operations

#### 0.4.13 - January 25 2017
* Added slice annotation structure.
* Parts with ORFs will have annotations of these regions on DnaSlice.

#### 0.4.12 - January 19 2017
* Bug in external part point mutation caused bp insertion for * operator

#### 0.4.11 - January 5 2017
* Add source code positions to DnaAssembly-phase error reports.

#### 0.4.10 - December 15 2017
* Allow plugins to configure themselves based on parsed compiler options directly.

#### 0.4.9 - December 13 2016
* Eliminate shared module.

#### 0.4.8 - December 13 2016
* Package codon opt arguments in a domain record to improve explicitness.

#### 0.4.7 - December 8 2016
* Pragmas are provided to L2 expansion and allele swap plugins.

#### 0.4.6 - December 2 2016
* Dna creation happens for each assembly independently and collects all errors.
* Refactored main compiler function pipeline to be more granular and pull more items to top level.

#### 0.4.5 - December 2 2016
* Refactor core to use the Amyris.Dna domain type.

#### 0.4.4 - November 28 2016
* Depuplicate command line args in usage message and include aliases.

#### 0.4.3 - November 28 2016
* Extend plugin format description and improve plugin help listing.

#### 0.4.2 - November 28 2016
* Fix semantic versioning to use reflection.
* Add --plugins command line argument.

#### 0.4.1 - November 28 2016
* Eliminate part reuse datatype in favor of retaining information about source PPP.

#### 0.4.0 - November 22 2016
* Initial release of GslCore library.
