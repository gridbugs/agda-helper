#AGDA Helper

##Installation
Install AGDA and the Agda Haskell library.
Modify the Makefile as per the version of the Agda Haskell library you installed (the PACKAGE line).
Modify agda-helper.hs with your AGDA standard library path:
This line:                          
                          (cmd_load file [".", "/usr/share/agda-stdlib"] )
