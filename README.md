# FunctionalOOP
Python-like OOP Programming Language parser and interpreter in SML

# Installing SML
SML/NJ can be installed on mac with `brew cask install smlnj`.
Make sure to add the alias `alias sml='rlwrap /usr/local/smlnj/bin/sml'` to your `~/.bash_profile`.

# Running the interpeter
Enter the folder and run `sml -m sources.cm` to start the program.

See `autorun.sml` to more details on the entrypoint. If you have a codefile,
you can run it by piping it: `cat mycode.txt | sml -m sources.cm`.
