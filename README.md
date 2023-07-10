# Spectra Synthesizer
This repository contains the code of the GR(1) synthesizer, which is based on the Spectra specification language that can be found in `spectra-lang` repository.

## Packaged Version
To download a packaged version of Eclipse with spectra tools installed, go to http://smlab.cs.tau.ac.il/syntech/spectra/index.html.
We provide also an update site to install occasional updates in the following link: `http://smlab.cs.tau.ac.il/syntech/spectra/tools/update/`.

## Installation
The synthesizer and its tools are developed over the Eclipse framework, therefore a working installation of Eclipse IDE for Java and DSL Developers (preferably version above or equals 2022-09) is a prerequisite.

In order to compile Spectra synthesizer from source code:
- As a prerequisite, follow the instructions in the `spectra-lang` repository.
- `git clone` this repository into your computer.
- Import all existing projects from both repositories into an Eclipse workspace. After the clone you should have two folders called `spectra-lang` and `spectra-synt` in your file system, containing many Java projects. `spectra-lang` projects should be already imported into your workspace. Import the projects of this repository by 'Import -> Projects from Folder or Archive', the choosing `spectra-synt` directory, and selecting all the projects from there *except* the spectra-synt folder itself (it is not a real project and it might cause problems if imported together with the other real projects).
- Run an Eclipse instance by creating a new `Eclipse Application` configuration in the `Run Configurations` window.

## Documentation
Information about Spectra language and tools can be found here: http://smlab.cs.tau.ac.il/syntech/.
A comprehensive hands-on tutorial can be found here: https://smlab.cs.tau.ac.il/syntech/tutorial/.
A detailed user guide can be downloaded from here: http://smlab.cs.tau.ac.il/syntech/spectra/userguide202210.pdf.

## Licensing
Spectra project is licensed under BSD-3-Clause. It uses CUDD library, which is also licensed under BSD-3-Clause, and whose modified code can be found in `spectra-cudd` repository. It also uses Brics automaton library, licensed under BSD-2-Clause and available here: https://www.brics.dk/automaton/.
