* Diviz generator
Description file (`XMCDA_files/description-wsDD.xml`) that is used to generate diviz package lays in the `XMCDA_files` directory. **Don't edit `diviz/description-wsDD.xml`, this file is overwritten each time the generator is being launched.**
There is a warning in case `diviz/description-wsDD.xml` and `XMCDA_files/description-wsDD.xml` exist and are different. In this situation `diviz/description-wsDD.xml` is copied to `/tmp/` dir to be able to restore it. 

Test directory (if exists) is being copied each time the generator is launched and is restored after generating diviz package.

`diviz_generator.sh` uses generator from `decision-deck-webservices-infrastructure/python/generate_from_description.py` (main diviz repository) to 
generate a package that contains R code and is compatibile with diviz. It takes a description file from the previous point, $OUT_DIR location and
generates a whole diviz's compatibile file hierarchy. In case $OUT_DIR already existed, generator removes it (this is generator requirement, however `diviz-generator.sh` saves `tests` directory), run `generate_from_description.py` script, `merger.py` and restores `tests` directory (if a saved copy exists).

`merger.py` is used to merge all source R files form R package into one R file that may be pasted into diviz's main file that is located in `src` directory.
`merger.py` gets 3 arguments, in which 1 is optional ` relative_source_files_path merged_file_path [merged_filename]`.
`merger.py` is also used in `diviz_generator.sh` pipeline so it's not necessary to run it separately.
