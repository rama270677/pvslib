NASALib
=

The NASALib is a collaborative effort being carried out for over [three decades](http://shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library) in the context of the [research on theorem proving](http://shemesh.larc.nasa.gov/fm/fm-pvs.html) sponsored by [NASA Langley](http://www.nasa.gov/centers/langley/home) and maintained by the [NASA
Langley Formal Methods Team](http://shemesh.larc.nasa.gov/fm).

Mostly centered in the Prototype Verification System ([PVS](http://pvs.csl.sri.com)) by SRI International, the The NASALib main contributions are: 
1) [PVS libraries](#Libraries): a collection of formal developments and 
2) [Scripts](#Scripts): a set of shell scripts designed to automate and ease the PVS user experience.

The current version of the NASALib is 7.1.0 (06/20/19) and it is compatible with [PVS 7.1+](http://pvs.csl.sri.com/download.shtml).
The following instructions assume that PVS 7.1 is installed in the directory
`<pvsdir>`, i.e., in the instructions below replace `<pvsdir>` by the
absolute path where PVS is installed.

# Libraries

Currently, the NASALib is composed by the following 53 libraries.

| Library  | Description | 
| --- | --- | 
| [ACCoRD](./ACCoRD/README.md) | Framework for the analysis of air traffic conflict detection and resolution algorithms | 
| [affine_arith](./affine_arith/README.md) | Formalization of affine arithmetic and strategy for evaluating polynomial functions with variables on interval domains. |
| [algebra](./algebra/README.md) | Groups, monoids, rings, etc. |
| [analysis](./analysis/README.md) | Real analysis, limits, continuity, derivatives, integrals. |
| [ASP](./ASP/README.md) | Denotational semantics of Answer Set Programming. |
| [aviation](./aviation/README.md) | Support definitions and properties for aviation-related formalizations. |
| [Bernstein](./Bernstein/README.md) | Formalization of multivariate Bernstein polynomials. |
| [CCG](./CCG/README.md) | Formalization of diverse termination criteria. |
| [complex](./complex/README.md) | Complex numbers. |
| [complex_alt](./complex_alt/README.md) | Alternative formalization of complex numbers. |
| [complex_integration](./complex_integration/README.md) | Complex integration. |
| [co_structures](./co_structures/README.md) | Sequences of countable length defined as co-algebraic datatypes. |
| [digraphs](./digraphs/README.md) | Directed graphs: circuits, maximal subtrees, paths, DAGs. |
| [exact_real_arith](./exact_real_arith/README.md) | Exact real arithmetic including trig functions. |
| [examples](./examples/README.md) | Examples of application of the functionality provided by the NASALib. |
| [extended_nnreal](./extended_nnreal/README.md) | Extended non-negative reals. |
| [fast_approx](./fast_approx/README.md) | Approximations of standard numerical functions. |
| [fault_tolerance](./fault_tolerance/README.md) | Fault tolerance protocols. |
| [float](./float/README.md) | Floating point numbers and arithmetic. |
| [graphs](./graphs/README.md) | Graph theory. |
| [groups](./groups/README.md) | Group theory. |
| [interval_arith](./interval_arith/README.md) | Interval arithmetic and numerical approximations. Includes automated strategies numerical for computing numerical approximations and interval for checking satisfiability and validity of simply quantified real-valued formulas. This development includes a formalization of Allen interval temporal logic. |
| [ints](./ints/README.md) | Integer division, gcd, mod, prime factorization, min, max. |
| [lebesgue](./lebesgue/README.md) | Lebesgue integral with connection to Riemann Integral. |
| [linear_algebra](./linear_algebra/README.md) | Linear algebra. |
| [lnexp](./lnexp/README.md) |  Logarithm, exponential and hyperbolic functions. & Foundational definitions of logarithm, exponential and hyperbolic functions. |
| [matrices](./matrices/README.md) |  |
| [measure_integration](./measure_integration/README.md) | Sigma algebras, measures, Fubini-Tonelli Lemmas. |
| [MetiTarski](./MetiTarski/README.md) | |
| [metric_space](./metric_space/README.md) | Domains with a distance metric, continuity and uniform continuity. |
| [numbers](./numbers/README.md) | Elementary number theory. |
| [orders](./orders/README.md) | Abstract orders, lattices, fix points. |
| [power](./power/README.md) | Generalized Power function (without ln/exp). |
| [probability](./probability/README.md) | Probability theory. |
| [PVS0](./PVS0/README.md) | Formalization of fundamental computability concepts. |
| [PVSioChecker](./PVSioChecker/README.md) | Animation of PVS specifications. |
| [reals](./reals/README.md) | Summations, sup, inf, sqrt over the reals, absolute value, etc. |
| [Riemann](./Riemann/README.md) |  Riemann Integral |
| [scott](./scott/README.md) | Scott topology. |
| [series](./series/README.md) | Power series, comparison test, ratio test, Taylor's theorem. |
| [sets_aux](./sets_aux/README.md) | Power sets, orders, cardinality over infinite sets. Includes functional and relational facts based on Axiom of Choice and refinement relations based on equivalence relations. |
| [sigma_set](./sigma_set/README.md) | Summations over countably infinite sets. |
| [sorting](./sorting/README.md) | Sorting algorithms. |
| [structures](./structures/README.md) | Bounded arrays, finite sequences, bags, and several other structures. |
| [Sturm](./Sturm/README.md) |  Formalization of Sturm's theorem for univariate polynomials. Includes strategies `sturm` and `mono-poly` for automatically proving univariate polynomial relations over a real interval. |
| [Tarski](./Tarski/README.md) | Formalization of Tarski's theorem for univariate polynomials. Includes strategy tarski for automatically proving systems of univariate polynomial relations on the real line. |
| [topology](./topology/README.md) | Continuity, homeomorphisms, connected and compact spaces, Borel sets/functions. |
| [trig](./trig/README.md) | Trigonometry: definitions, identities, approximations. |
| [TRS](./TRS/README.md) | Term rewrite systems and Robinson unification algorithm. |
| [TU_games](./TU_games/README.md) | Cooperative TU-games. |
| [vect_analysis](./vect_analysis/README.md) | Limits, continuity, and derivatives of vector functions. |
| [vectors](./vectors/README.md) | 2-D, 3-D, 4-D, and n-dimensional vectors. |
| [while](./while/README.md) | Semantics for the programming language While. |

## Dependencies

![dependency graph](./all-theories.svg "Dependency Graph")

# Scripts

The NASALib also provides a collection of scripts that automates several tasks.

* [`proveit`](./Scripts#proveit) (*) - Runs PVS in batch mode 
* [`provethem`](./Scripts#provethem) (*) - Runs `proveit` on several libraries 
* [`pvsio`](./Scripts#pvsio) (*) - Command-line utility to run the PVSio ground evaluator.
* [`prove-all`](./Scripts#prove-all) - Runs `proveit` on each library in the NASALib by wrapping `provethem` in order to provide a specific kind of run. 
* [`cleanbin-all`](./Scripts#cleanbin-all) - Clean `.pvscontext` and binary files from PVS libraries.
* [`find-all`](./Scripts#find-all) - Searches strings matching a given regular expressions in PVS libraries.
* [`dependencygraph`](./Scripts#dependencygraph) - Generates a library dependency graph for libraries in the current directory.
* [`dependency-all`](./Scripts#d#dependency-all) - Generates the dependency graphs for the PVS libraries in the current folder.

(*) Already included in the PVS 7.1 distribution.

Click [here](./Scripts) for details.

# Getting the NASALib

## Last Stable Version

The last stable version of the NASALib is available from this 
[web site](http://shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library). 
It comes in 3 sizes: *basic*, *classic*, and *full*. 
All the distribution files include the same PVS specification and proof files. 
They differ in the binary files, which are only included in the classic and full distributions. 
The full distribution also includes pre-installed versions of [Z3](http://z3.codeplex.com) and [MetiTarski](http://www.cl.cam.ac.uk/~lp15/papers/Arith). 

Download and decompress the version of your preference in the `<pvsdir>` directory.

## Development Version

For PVS advanced users, the development version of the NASA PVS Library is available from [GitHub](https://github.com/nasa/pvslib). 
To clone the development version, type the following command at the directory `<pvsdir>`
(the dollar sign represents the prompt of the operating system).

```shell
$ git clone http://github.com/nasa/pvslib nasalib 
```

The command above will put a copy of the library in the directory
`<pvsdir>/nasalib`.

This version of the NASA PVS Library includes [Hypatheon](http://shemesh.larc.nasa.gov/people/bld/hypatheon.html). 
Hypatheon is a database utility that provides a capability for indexing PVS theories and making them searchable via a GUI client.

### Major Recent Changes

*) **The library `trig_fnd` is now deprecated**. It's still provided for backward compatibility, but it should be replaced by `trig`.  The new library `trig`, which used to be axiomatic, is now foundational. However, in contrast to `trig_fnd`, trigonometric definitions are based on infinite series, rather than integrals. This change considerably reduces the type-checking of theories involving trigonometric functions. The change from `trig_fnd` to `trig` should not have a major impact in your formal developments since names of definitions and lemmas are the same. However, theory importing may be slightly different.

*) The PVS developments `TCASII`, `WellClear`,  and `DAIDALUS` are now [available](https://github.com/nasa/WellClear/tree/master/PVS) as part of the [GitHub WellClear distribution](https://github.com/nasa/WellClear). The PVS development `PRECiSA`  is now [available](https://github.com/nasa/PRECiSA/tree/master/PVS) as part of the [GitHub PRECiSA distribution](https://github.com/nasa/PRECiSA). The PVS development `PolyCARP`  is now [available](https://github.com/nasa/PolyCARP/tree/master/PVS) as part of the [GitHub PolyCARP distribution](https://github.com/nasa/PolyCARP).


# Quick Installation

The following instructions assume that the NASALib is located in the directory `<pvsdir>/nasalib`.

## 1) Add this directory to the environment variable `PVS_LIBRARY_PATH`

If it does not exists, creates such variable and with the path of this directory as only content. It is usually very useful to have your shell systems creating this variable at startup. To this end, and depending upon your shell, you may want to add one of the following lines in your startup script.  For C shell (csh or tcsh), you may add this line in `~/.cshrc`:
```shell
setenv PVS_LIBRARY_PATH "<pvsdir>/nasalib"
```
For Borne shell (bash or sh), add this line in either `~/.bashrc` or `~/.profile`:
```shell
export PVS_LIBRARY_PATH="<pvsdir>/nasalib"
```

## 2) Additional steps to protect previous NASALib configurations (optional)

If you had a previous installation of the NASALib, either remove the file `~/.pvs.lisp` or, if you have a special configuration in that file, remove the following line  
```lisp
(load "<pvsdir>/nasalib/pvs-patches.lisp") 
```
## 3) Install Scripts

Finally, go to the directory `<pvsdir>/nasalib` and run the following shell scripts (the dollar sign represents the prompt of the operating system).

The `install-scripts` command will update and install the NASALib scripts as needed.
~~~shell
$ ./install-scripts
~~~

## 4) Install Hyphateon (optional)

The `fetch-hypatheon-db` utility fetches an updated version of the NASALib database to be used by Hypatheon.
~~~shell
$ ./fetch-hypatheon-db
~~~

## Details
For more information about the installation process and options visit the [installation](http://shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library/installation.html) page.

# Contributors

The NASALib has grown over the years thanks to the contribution of the following people.

* [Aaron Dutle](http://shemesh.larc.nasa.gov/people/amd), NASA, USA
* Alfons Geser, HTWK Leipzig, Germany
* Amer Tahat, Michigan Technological University, USA
* Amy Isvik, Wartburg College, USA
* Ana Cristina Rocha Oliveira, University of Brasilia, Brazil
* André Galdino, Federal University of Goiás, Brazil
* Andreia Avelar Borges, University of Brasilia, Brazil
* Anthony Narkawicz, NASA, USA
* Ariane Alves Almeida, University of Brasilia, Brazil
* [Bruno Dutertre](http://www.csl.sri.com/users/bruno), SRI, USA
* [Ben Di Vito](http://shemesh.larc.nasa.gov/people/bld), NASA, USA
* [César Muñoz](http://shemesh.larc.nasa.gov/people/cam), NASA, USA
* Concepción Vidal, University of La Coruña, Spain
* David Griffioen,CWI, The Netherlands
* [David Lester](http://apt.cs.man.ac.uk/people/dlester), Manchester University, UK
* Dragan Stosic, Ireland
* [Érik Martin-Dorel](http://erik.martin-dorel.org/), U. Montpellier 2 & U. of Perpignan (formerly), France
* Felicidad Aguado, University of La Coruña, Spain
* Flavio L.C. de Moura, University of Brasilia, Brazil
* [Gilles Dowek](https://who.rocq.inria.fr/Gilles.Dowek/index-en.html), INRIA, France
* [George Hagen](http://shemesh.larc.nasa.gov/people/geh), NASA, USA
* Gilberto Perez, University of La Coruña, Spain
* Gregory Anderson, University of Texas at Austin, USA
* Hanne Gottliebsen, NIA, USA
* [Heber Herencia-Zapana](http://www.nianet.org/resources/Research/Research-Staff/Heber-Herencia-Zapana/Heber-Herencia-Zapana.aspx), NIA, USA
* J. Tanner Slagel, NASA, USA
* Jerry James, Utah State University, USA
* [Jeff Maddalon](http://shemesh.larc.nasa.gov/people/jmm), NASA, USA
* Jon Sjogren, Department of Defense, USA
* John Siratt, University of Arkansas at Little Rock, USA
* Katherine Cordwell, CMU, USA
* [Kristin Rozier](http://ti.arc.nasa.gov/profile/kyrozier), NASA, USA
* [Lee Pike](http://corp.galois.com/lee-pike), Galois, USA
* [Marco A. Feliú](https://www.nianet.org/directory/research-staff/marco-feliu/), NIA & NASA, USA
* [Mariano Moscato](https://www.nianet.org/directory/research-staff/mariano-moscato/), NIA & NASA, USA
* [Mauricio Ayala-Rincón](http://www.mat.unb.br/~ayala), University of Brasilia, Brazil
* [Natarajan Shankar](http://www.csl.sri.com/users/shankar), SRI, USA
* Pablo Ascariz, University of La Coruña, Spain
* [Paul Miner](http://shemesh.larc.nasa.gov/people/psm), NASA, USA
* Pedro Cabalar, University of La Coruña, Spain
* Radu Siminiceanu, NIA, USA
* [Ricky Butler](http://shemesh.larc.nasa.gov/people/rwb), NASA, USA
* [Silvie Boldo](https://www.lri.fr/~sboldo), INRIA, France
* [Sam Owre](http://www.csl.sri.com/users/owre), SRI, USA
* Thaynara de Lima, Federal University of Goiás, Brazil
* Thiago Mendoça Ferreira Ramos, University of Brasilia, Brazil
* [Víctor Carreño](http://shemesh.larc.nasa.gov/people/vac), NASA, USA


---

Enjoy it.

[The NASA Langley Formal Methods Team](http://shemesh.larc.nasa.gov/fm)