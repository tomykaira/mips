device_map = [0] * 128

DATA.readlines.each do |line|
  if line[0] != '#'
    char, device, internal = line.split(" ")
    device_map[device.to_i] = internal.to_i
  end
end
device_map.each do |code|
  puts code.to_s(2).rjust(7, "0")
end
__END__
# ascii codes
`       14      96
Q       21      81
1       22      49
Z       26      90
S       27      83
A       28      65
W       29      87
2       30      50
C       33      67
X       34      88
D       35      68
E       36      69
4       37      52
3       38      51
V       42      86
F       43      70
T       44      84
R       45      82
5       46      53
N       49      78
B       50      66
H       51      72
G       52      71
Y       53      89
6       54      54
M       58      77
J       59      74
U       60      85
7       61      55
8       62      56
,       65      44
K       66      75
I       67      73
O       68      79
0       69      48
9       70      57
.       73      46
/       74      47
L       75      76
;       76      59
P       77      80
0       78      48
'       82      39
[       84      91
=       85      61
]       91      93
\       93      92

# special codes
# after char-code, decimal.
#	decimal	internal-code
BKSP 	102	127
SPACE 	41	32
CAPS 	88	[STATUS]
L SHFT 	18	[STATUS]
L CTRL 	20	[STATUS = CAPS]
L ALT 	17	[STATUS]
ENTER 	90	10
ESC 	118	27

# BKSP 	66	F0,66
# SPACE 	29	F0,29
# TAB 	0D 	F0,0D
# CAPS 	58	F0,58
# L SHFT 	12	FO,12
# L CTRL 	14	FO,14
# L ALT 	11	F0,11
# R SHFT 	59	F0,59
# ENTER 	5A 	F0,5A
# ESC 	76	F0,76
# F1 	05	F0,05
# F2 	06	F0,06
# F3 	04	F0,04
# F4 	0C 	F0,0C
# F5 	03	F0,03
# F6 	0B 	F0,0B
# F7 	83	F0,83
# F8 	0A 	F0,0A
# F9 	01	F0,01
# F10 	09	F0,09
# F11 	78	F0,78
# F12 	07	F0,07
# NUM 	77	F0,77
# KP * 	7C 	F0,7C
# KP - 	7B 	F0,7B
# KP + 	79	F0,79
# KP . 	71	F0,71
# KP 0 	70	F0,70
# KP 1 	69	F0,69
# KP 2 	72	F0,72
# KP 3 	7A 	F0,7A
# KP 4 	6B 	F0,6B
# KP 5 	73	F0,73
# KP 6 	74	F0,74
# KP 7 	6C 	F0,6C
# KP 8 	75	F0,75
# KP 9 	7D 	F0,7D
#
# L GUI 	E0,1F 	E0,F0,1F
# R CTRL 	E0,14 	E0,F0,14
# R GUI 	E0,27 	E0,F0,27
# R ALT 	E0,11 	E0,F0,11
# APPS 	E0,2F 	E0,F0,2F
#
# KP / 	E0,4A 	E0,F0,4A
# KP EN 	E0,5A 	E0,F0,5A
#
# INSERT 	E0,70 	E0,F0,70
# HOME 	E0,6C 	E0,F0,6C
# PG UP 	E0,7D 	E0,F0,7D
# DELETE 	E0,71 	E0,F0,71
# END 	E0,69 	E0,F0,69
# PG DN 	E0,7A 	E0,F0,7A
# U ARROW 	E0,75 	E0,F0,75
# L ARROW 	E0,6B 	E0,F0,6B
# D ARROW 	E0,72 	E0,F0,72
# R ARROW 	E0,74 	E0,F0,74
