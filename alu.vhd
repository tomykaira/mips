library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity alu is
  
  port (
    a, b    : in  std_logic_vector(31 downto 0);
    control : in  std_logic_vector(2 downto 0);
    output  : out std_logic_vector(31 downto 0);
    zero    : out std_logic);

end alu;

architecture behave of alu is

  signal bb : std_logic_vector(31 downto 0);
  signal c : std_logic;
  signal o0, o1, o2, o3 : std_logic_vector(31 downto 0);

  signal out_buf : std_logic_vector(31 downto 0);

begin  -- behave

  c <= control(2);

  bb <= not b when control(2) = '1' else b;

  o0 <= a and bb;
  o1 <= a or b;
  o2 <= a + bb + c;
  o3 <= x"0000000" & "000" & o2(31);

  out_buf <= o0 when control(1 downto 0) = "00" else
             o1 when control(1 downto 0) = "01" else
             o2 when control(1 downto 0) = "10" else
             o3 when control(1 downto 0) = "11";

  output <= out_buf;
  zero <= '1' when conv_integer(out_buf) = 0 else '0';

end behave;
