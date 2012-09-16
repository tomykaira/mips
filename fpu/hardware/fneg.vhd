-- TEST
-- a     h  | s   h
-- 00000000 | 80000000
-- 3f800000 | bf800000
-- bf800000 | 3f800000
-- 80000000 | 00000000
-- 3f801234 | bf801234
-- /TEST

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity fneg is
  
  port (
    a : in std_logic_vector(31 downto 0);
    s : out std_logic_vector(31 downto 0)
  );

end fneg;

architecture behave of fneg is
  
begin  -- behave

  s(30 downto 0) <= a(30 downto 0);
  s(31) <= not a(31);
  
end behave;
