LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;

-- comparator
-- Compare two vectors a, b and return LT (a<b) and EQ(a=b)
-- If is_float flag is high, two vectors are treated as 32-bit float values.
-- If input is int, treated as signed.

-- MEMO: another way: return ILT, IEQ, FLT, FEQ for both patterns 

-- TEST
-- a        | b        | is_float | lt | eq
--        0 |        0 |        0 | 0  | 1
--        1 |        0 |        0 | 0  | 0
--       -1 |        0 |        0 | 1  | 0
--       -1 |        1 |        0 | 1  | 0
--       54 |       54 |        0 | 0  | 1
--       54 |       39 |        0 | 0  | 0
--       39 |       54 |        0 | 1  | 0
--       39 |      -54 |        0 | 0  | 0
--      -54 |       39 |        0 | 1  | 0
--      -54 |      -54 |        0 | 0  | 1
--      -54 |      -39 |        0 | 1  | 0
--      -39 |      -54 |        0 | 0  | 0
-- /TEST

entity comparator is
  port (
    a, b : in std_logic_vector(31 downto 0);
    is_float : in STD_LOGIC;
    lt, eq : out STD_LOGIC
);
end comparator;

architecture behave of comparator is
  
begin

  eq <= '1' when a = b else '0';

  process (a, b, is_float)
  begin
    -- a is negative, b is not
    if a(31) = '1' and b(31) = '0' then
      lt <= '1';
    -- a is positive, b is negative
    elsif a(31) = '0' and b(31) = '1' then
      lt <= '0';
    -- neg-neg and pos-pos are both comparable
    elsif a < b then
      lt <= '1';
    else
      lt <= '0';
    end if;
  end process;

end behave;
