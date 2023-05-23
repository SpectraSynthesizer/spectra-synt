# Spectra Synthesizer
This repository contains the code of the GR(1) synthesizer, which is based on the Spectra specification language that can be found in `spectra-lang` repository.

## Packaged Version
To download a packaged version of Eclipse with spectra tools installed, go to http://smlab.cs.tau.ac.il/syntech/spectra/index.html.
We provide also an update site to install occasional updates in the following link: `http://smlab.cs.tau.ac.il/syntech/spectra/tools/update/`.

## Installation
The synthesizer and its tools are developed over the Eclipse framework, therefore a working installation of Eclipse (preferably version above or equals 2022-09) is a prerequisite.

In order to compile Spectra synthesizer from source code:
- `git clone` this repository into your computer, along with `spectra-lang`.
- Import all existing projects from both repositories into an Eclipse workspace. After the clone you should have two folders, `spectra-lang` and `spectra-synt`, containing many Java projects. Import all these projects by 'Import -> Projects from Folder or Archive', then choosing once `spectra-lang` directory, selecting all the projects from there, and later `spectra-synt` directory, also selecting all the projects from there.
- Build Spectra language auto generated code by right clicking on `src/tau.smlab.syntech/GenerateSpectra.mwe2` and then 'Run As -> MWE2 Workflow`.
- Run project by creating a new `Eclipse Application` configuration in `Run Configurations` window.

## Documentation
Information about Spectra language and tools can be found here: http://smlab.cs.tau.ac.il/syntech/.
A comprehensive hands-on tutorial can be found here: https://smlab.cs.tau.ac.il/syntech/tutorial/.
A detailed user guide can be downloaded from here: http://smlab.cs.tau.ac.il/syntech/spectra/userguide202210.pdf.

## Licensing
Spectra project is licensed under BSD-3-Clause. It uses CUDD library, which is also licensed under BSD-3-Clause, and whose modified code can be found in `spectra-cudd` repository. It also uses Brics automaton library, licensed under BSD-2-Clause and available here: https://www.brics.dk/automaton/.
