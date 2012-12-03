device_map = [0] * 256

DATA.readlines.each do |line|
  if line[0] != '#'
    _, normal, _, shift, device = line.split(" ")
    device_map[device.to_i(16)] = normal.to_i(16)
    device_map[device.to_i(16) + 128] = shift.to_i(16)
  end
end
device_map.each do |code|
  puts code.to_s(2).rjust(7, "0")
end
__END__
# JIS version
# http://www.ne.jp/asahi/shared/o-family/ElecRoom/AVRMCOM/PS2_RS232C/KeyCordList.pdf
# normal | with shift | scan code
1 31 ! 21 16
2 32 " 22 1E
3 33 # 23 26
4 34 $ 24 25
5 35 % 25 2E
6 36 & 26 36
7 37 ' 27 3D
8 38 ( 28 3E
9 39 ) 29 46
0 30 0 30 45
a 61 A 41 1C
b 62 B 42 32
c 63 C 43 21
d 64 D 44 23
e 65 E 45 24
f 66 F 46 2B
g 67 G 47 34
h 68 H 48 33
i 69 I 49 43
j 6A J 4A 3B
k 6B K 4B 42
l 6C L 4C 4B
m 6D M 4D 3A
n 6E N 4E 31
o 6F O 4F 44
p 70 P 50 4D
q 71 Q 51 15
r 72 R 52 2D
s 73 S 53 1B
t 74 T 54 2C
u 75 U 55 3C
v 76 V 56 2A
w 77 W 57 1D
x 78 X 58 22
y 79 Y 59 35
z 7A Z 5A 1A
- 2D = 3D 4E
^ 5E ~ 7E 55
\ 5C | 7C 6A
@ 40 ` 60 54
[ 5B { 7B 5B
; 3B + 2B 4C
: 3A * 2A 52
] 5D } 7D 5D
, 2C < 3C 41
. 2E > 3E 49
/ 2F ? 3F 4A
\ 5C _ 5F 51
#
# special codes
# after char-code, decimal.
#	decimal	internal-code
BKSP  7f BKSP  7f 66
SPACE 20 SPACE 20 29
ENTER 0A ENTER 0A 5A
ESC   1B ESC   1B 76
# CAPS 	88	[STATUS]
# L SHFT 	18	[STATUS]
# L CTRL 	20	[STATUS = CAPS]
# L ALT 	17	[STATUS]
