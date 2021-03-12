# Key files
FM4FUN.fsx
FM4FUNAST.fs
FM4FUNLexer.fsl
FM4FUNParser.fsp
FM4FUNCompiler.fs

# Installing
To install you need to compile the Lexer and Parser.
## Lexer
To compile the lexer use the following command: \
{path-to "fslex.exe"} FM4FUNLexer.fsl --unicode
## Parser
To compile the parser use the following command: \
{path-to "fsyacc.exe"} FM4FUNParser.fsp --module FM4FUNParser

# Running
To run the program, you need to run the FM4FUN.fsx like you normally would.
We used: \
dotnet fsi FM4FUN.fsx

# Using
When you have opened FM4FUN.fsx, you will be promted to enter a GCL-command.
Here you can copy-paste the command of your choice. \
The command can be inline or multi-line, if it is multi-line it CANNOT contain double newlines. \
If the program is parsed then you will get back the formatted GCL-program, 
otherwise an error will be printed with an approximate position of that error. \
You will then have the option to select whether a program graph should be computed. 
   * Det for deterministic
   * NonDet for non-deterministic. 
   * No or everything else for no. 