# 8086-Disassembler
This disassembler was written in x86 Assembly language during the first semester of the Software Engineering program at Vilnius University (VU).
## Usage
This program was written for Turbo Assembler (TASM) and uses TASM specific features, so it should be Assembled with TASM.

In order to assemble it with TASM write:

`tasm disasm`

`tlink disasm`
[enter]

The Assembled executable should be run with two positional arguments: input file name and output file name:

`disasm <input_file_name> <output_file_name>`
[enter]
For example, to disassemble an executable `test.com` and output the disassembly result into `output.asm`, run:

`disasm test.com output.asm`

