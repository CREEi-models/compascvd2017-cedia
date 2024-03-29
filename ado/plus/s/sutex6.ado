**************************************
*This is sutex6.ado beta version
*04 Sep 2001
*Questions, comments and bug reports : 
*terracol@univ-paris1.fr
*************************************


cap prog drop sutex6
prog define sutex6
version 6.0
syntax [varlist] [if] [in] [aweight fweight],[DIGits(integer 3)] [LABels] [PAR] [NOBS] [MINmax] [NA(string)] [TITle(string)] [KEY(string)] [LONGtable] [NOCHECK] [PLace(string)]

capture confirm variable `varlist'
if _rc==7 {
		di in red "no variables found"
		exit
		}

if "`place'"=="" {
			local place="htbp"
			}

************************
*Table heads
************************
local nm_vr="Variable"
local nm_me="Mean"
local nm_sd="Std. Dev."
local nm_mn="Min."
local nm_mx="Max."
local headlo="... table \thetable{} continued"
local footlo="Continued on next page..."
*********************************

local v=2
tokenize "`varlist'"
qui su `1'
local q1=r(N)
mac shift
while "`1'" !="" {
			qui su `1'
			local q`v'=r(N)
			if `q`v''!=`q1' {
						local nobs="nobs"
						}
			local v=`v'+1
			mac shift
			}

local z="`na'"
if "`na'"!="" {
			local na2="na(`z')"
			}
if "`varlist'"=="" {
				local varlist =unav _all
				}
if "`title'"=="" {
			local title="Summary statistics"
			}
if "`key'"=="" {
			local key="sumstat"
			}


***********************
* Number of digits
***********************

local nbdec="0."
local i=1
while `i'<=`digits'-1 {
				local nbdec="`nbdec'0"
				local i=`i'+1
				}
if `digits'==0 {
			local nbdec="1"
			}
if `digits'>0 {
			local nbdec="`nbdec'1"
			}


********************
* setting columns
********************

if "`minmax'"!="" {
			local a1=" c c"
			}
if "`minmax'"!="" {
			local a2=" & \textbf{`nm_mn'} &  \textbf{`nm_mx'}"
			}
local a3=2
if "`minmax'"!="" {
			local a3=`a3'+2
			}
if "`nobs'"!="" {
			local a3=`a3'+1
			}
local a6=`a3'+1
if "`nobs'"!="" {
			local a4=" c"
			}
if "`nobs'"!="" {
			local a5=" & \textbf{N}"
			}
if "`par'"!="" {
			local op="("
			}
if "`par'"!="" {
			local fp=")"
			}

if "`file'"=="" {
			di in wh "%------- Begin LaTeX code -------%"_newline
			}
if "`file'"!="" {
			di in wh ""_n
			}

******************
* "regular" table
******************
if "`longtable'"=="" {
di in wh "\begin{table}[`place']\centering \caption{`title'\label{`key'}}"
di in wh "\begin{tabular}{l c c `a1' `a4'}\hline\hline"
di in wh "\multicolumn{1}{c}{\textbf{`nm_vr'}} & \textbf{`nm_me'}" _newline " & \textbf{`op'`nm_sd'`fp'}`a2' `a5'\\\ \hline"
}

*******************
*longtable
******************

if "`longtable'"!="" {

di in wh  "\begin{center}"_newline "\begin{longtable}{l c c `a1' `a4'}"
di in wh "\caption{`title'\label{`key'}}\\\"_newline"\hline\hline\multicolumn{1}{c}{\textbf{`nm_vr'}}"_newline" &\textbf{`nm_me'}"_newline" & \textbf{`op'`nm_sd'`fp'}`a2' `a5' \\\ \hline"
di in wh "\endfirsthead"
di in wh "\multicolumn{`a6'}{l}{\emph{`headlo'}}"_newline"\\\ \hline\hline\multicolumn{1}{c}{\textbf{`nm_vr'}}"_newline" & \textbf{`nm_me'}"_newline" & \textbf{`op'`nm_sd'`fp'}`a2' `a5' \\\ \hline"
di in wh "\endhead"
di in wh "\hline"
di in wh "\multicolumn{`a6'}{r}{\emph{`footlo'}}\\\"
di in wh "\endfoot"
di in wh "\endlastfoot"
}

tokenize "`varlist'"
local l=0
while "`1'" !="" {
			local l=`l'+1
			mac shift
			}

local i=1
while `i'<=`l' {
			if "`par'"!="" {
						local op="("
						}
			if "`par'"!="" {
						local fp=")"
						}
			tokenize "`varlist'"
			local nom="``i''"
			qui su `nom' `if' `in'
			if "`labels'"!="" {
						local lab : variable label ``i''
						if "`lab'"!="" {
								    local nom="\`lab'"
									}
						}
		
			***************************
			*LaTeX special characters
			**************************
			if "`nocheck'"=="" {
							latres ,name(`nom')
							local nom="$nom"
							}
			****************************

			local mean=round(r(mean), `nbdec')
			local sd=round(sqrt(r(Var)), `nbdec')
			if substr("`mean'",1,1)=="." {
								local mean="0`mean'"
								}
			if substr("`mean'",1,2)=="-." {
								local pb=substr("`mean'",3,.)
								local mean="-0.`pb'"
								}
			if substr("`sd'",1,1)=="." {
								local sd="0`sd'"
								}
			parse "`mean'", parse(.)
			local mean="$_1"+"$_2"+substr("$_3",1,`digits')
			parse "`sd'", parse(.)
			local sd="$_1"+"$_2"+substr("$_3",1,`digits')
			local N`i'=r(N)
			if `N`i''==0  {
					   local mean="`na'"
					   local sd="`na'"
					   local op=""
					   local fp=""
					  }
			if `N`i''==1  {
						local sd="`na'"
						local op=""
						local fp=""
						}
			local min=round( r(min), `nbdec')
			if substr("`min'",1,1)=="." {
								local min="0`min'"
								}
			if substr("`min'",1,2)=="-." {
								local pb=substr("`min'",3,.)
								local min="-0.`pb'"
								}
			parse "`min'", parse(.)
			local min="$_1"+"$_2"+substr("$_3",1,`digits')
			local max=round( r(max), `nbdec')
			if substr("`max'",1,1)=="." {
								local max="0`max'"
								}
			if substr("`max'",1,2)=="-." {
								local pb=substr("`max'",3,.)
								local max="-0.`pb'"
								}
			parse "`max'", parse(.)
			local max="$_1"+"$_2"+substr("$_3",1,`digits')
			if `N`i''==0  {
					   local min="`na'"
					   local max="`na'"
						}
			if "`minmax'"!="" {
						local extr="& `min' & `max'"
						}
			if "`nobs'"!="" {
						local taille=" & `N`i''"
						}
			local ligne="\`nom' & `mean' & `op'`sd'`fp' `extr' `taille'"
			di in wh "`ligne'\\\"
			local i=`i'+1
		}



if "`nobs'"!="" {
			di in wh "\hline"
			}


local N=r(N)
if "`nobs'"=="" {
			di in wh "\multicolumn{1}{c}{N} & \multicolumn{`a3'}{c}{`N'}\""\\"  " \hline"
			}

if "`longtable'"==""{
				di in wh "\end{tabular}"_newline "\end{table}"
			}

if "`longtable'"!=""{
				di in wh "\end{longtable}"_newline "\end{center}"
			}






if "`file'"!="" {
			di in wh ""_n
			}
if "`file'"=="" {
			di in wh _newline "%------- End LaTeX code -------%"
			}
if "`file'"!="" {
			file close `fich'
			}

macro drop ligne*
macro drop nom 
if "`file'"!="" {
			di `"file {view "`file'"} saved"'
			}

end




cap prog drop latres
program define latres
version 6.0
syntax ,name(string) [sortie(string) nom]
if "`sortie'"=="" {
			local sortie="nom"
			}

local cr1="_" 
local crc1="\_"
local cr2="\"
local crc2="$\backslash$ "
local cr3="$"
local crc3="\symbol{36}"
local cr4="{"
local crc4="\{"
local cr5="}"
local crc5="\}"
local cr6="%"
local crc6="\%"
local cr7="#"
local crc7="\#"
local cr8="&"
local crc8="\&"
local cr9="~"
local crc9="\~{}"
local cr10="^"
local crc10="\^{}"
local cr11="<"
local crc11="$<$ "
local cr12=">"
local crc12="$>$ "

local nom="`name'"

			local t=length("`nom'")
			local rg=1
			local mot2=""
			while `rg'<=`t' {
						local let`rg'=substr("`nom'",`rg',1)
						local num=1
						while `num'<=12 {
									if "`let`rg''"=="`cr`num''" {
														local let`rg'="`crc`num''"
														}
									local num=`num'+1
									}
						if "`let`rg''"=="" {
										local mot2="`mot2'"+" " 
										}
						else if "`let`rg''"!="" {
											local mot2="`mot2'"+"`let`rg''"
										}		
						local rg=`rg'+1
						}
						
			global `sortie'="`mot2'"
end


