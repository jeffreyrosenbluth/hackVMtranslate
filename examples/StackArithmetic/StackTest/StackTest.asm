@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.1
D;JEQ
@SP
A=M-1
M=0
@LABEL.2
0;JMP
(LABEL.1)
@SP
A=M-1
M=-1
(LABEL.2)
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.3
D;JEQ
@SP
A=M-1
M=0
@LABEL.4
0;JMP
(LABEL.3)
@SP
A=M-1
M=-1
(LABEL.4)
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.5
D;JEQ
@SP
A=M-1
M=0
@LABEL.6
0;JMP
(LABEL.5)
@SP
A=M-1
M=-1
(LABEL.6)
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.7
D;JLT
@SP
A=M-1
M=0
@LABEL.8
0;JMP
(LABEL.7)
@SP
A=M-1
M=-1
(LABEL.8)
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.9
D;JLT
@SP
A=M-1
M=0
@LABEL.10
0;JMP
(LABEL.9)
@SP
A=M-1
M=-1
(LABEL.10)
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.11
D;JLT
@SP
A=M-1
M=0
@LABEL.12
0;JMP
(LABEL.11)
@SP
A=M-1
M=-1
(LABEL.12)
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.13
D;JGT
@SP
A=M-1
M=0
@LABEL.14
0;JMP
(LABEL.13)
@SP
A=M-1
M=-1
(LABEL.14)
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.15
D;JGT
@SP
A=M-1
M=0
@LABEL.16
0;JMP
(LABEL.15)
@SP
A=M-1
M=-1
(LABEL.16)
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
D=M-D
@LABEL.17
D;JGT
@SP
A=M-1
M=0
@LABEL.18
0;JMP
(LABEL.17)
@SP
A=M-1
M=-1
(LABEL.18)
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M+D
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M-D
@SP
A=M-1
M=-M
@SP
AM=M-1
D=M
A=A-1
M=M&D
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M|D
@SP
A=M-1
M=!M