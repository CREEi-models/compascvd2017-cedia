# microsim-health-cedia
# COMPAS
Installation :

- git clone git@github.com:CEDIA-models/compascvd017.git
- Go to ./compas/build
- ./compile_linux.sh all
- Run in stata create_compas.do
- You need to add stata-mp to your $PATH


Run :
./compas/runtime/compas scenario

To run COMPAS the user need to obtain the Public-Use Microdata Files (PUMFs) of the Canadian Community Health Survey of 2008-2009 and 2010 from Statistics Canada. These data are freely accessible to researchers who belong to an institution that is a Data Liberation Initiative partner. As stated in the license agreement linked to below, "institutional members of the DLI have access to the entire PUMFs collection, as outlined in the Appendix 1 of the DLI Licence. They can use the PUMFs for statistical and research purposes but they cannot share the data files with non DLI members."
http://www.statcan.gc.ca/eng/dli/caselaw/license

The Data Liberation Initiative is a licensing procedure for institutions that makes certain data owned by Statistics Canada available to affiliated researchers and personnel. Option 4 at the link below should be used by researchers who wish to access Public-Use Microdata Files (PUMFs) outside the Data Liberation Initiative. This option involves contacting Statistics Canada to sign a licence agreement.
https://www.statcan.gc.ca/eng/dli/caselaw/decline
