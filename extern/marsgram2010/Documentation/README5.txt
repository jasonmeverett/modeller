                    Sample Input and Output Files


                     PROGRAM TEST REFERENCE CASES

inputstdx.txt  - commented test input filex for reference case, using TES
                 mapping year x = 0, 1, or 2

ListMapYrx.txt - corresponding list output file for reference case, using
                 TES mapping year x = 0, 1, or 2

Comments at the end of the input files provide brief descriptions of
the input parameters. Most of these parameters have default values,
as specified in this input file.  Only parameters that differ from
their default values actually need to be included in the input file.
These input/list files can be used to verify proper installation and
operation of Mars-GRAM 2010.  After a successful run of Mars-GRAM with
input file inputstdx.txt, the LIST.txt file produced can be compared with
the ListMapYrx.txt files provided.  On UNIX machines this is done with
the command

diff LIST.txt ListMapYrx.txt    (using x = 0, 1, or 2)

(in UNIX, a null response indicates no differences between the two files).

In PC-DOS, this comparison is done with command

fc LIST.txt ListMapYrx.txt      (using x = 0, 1, or 2)

Note that UNIX output includes leading zeroes in front of decimal points
(e.g. 0.12 or -0.34), while in DOS format, the leading zeroes are dropped
(e.g.  .12 or  -.34).  Both UNIX version and DOS version input/list files
are provided on the distribution CD.  However, if the user wishes to
compare a UNIX version file with a DOS version file, a text editor can be 
used to insert (or remove) these leading zeroes, as necessary, for purposes 
of doing the file comparison.  Because of machine-dependent round-off 
differences, a few numbers may differ (typically no more than one number in 
the last significant digit displayed) between the LIST.txt file produced on 
the user's machine and the ListMapYrx.txt reference files provided.
