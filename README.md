# file_reader_8086_Assembly
Spring 2023 Machine Organization PROG4
File reader/writer in 8086 assembly. 

This program accepts two arguments via command tail in the following format:
Prog4.exe <filename>.<File type> <unsigned integer> 
with exactly one space between each item.

This program reads a file formatted as such: 
<name> <unsigned integer>
There can be any amount of white space between any of these items. 
The program stores the arguments from the commmand tail and converts 
the integer argument to an integer. 

The program then opens the argument file, creates an ouput file (titled
output.dat) and begins to read from the source file. We temporarily store
each name and string of integers, then convert the string to an integer, 
compare the integer from our argument to the person's integer, and if the 
person's integer is greater or equal to the argument integer, we write that
person's name to the outfile. 

When the program is done, it will display time elapsed to the nearest tenth
of a second and output.dat will exist in the folder this program is stored.


