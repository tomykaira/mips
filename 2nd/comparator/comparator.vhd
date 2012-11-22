LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;

-- comparator
-- Compare two vectors rs, rt and return LT (rs<rt) and EQ(rs=rt)
-- If is_float flag is high, two vectors are treated as 32-bit float values.
-- If input is int, treated as signed.

-- MEMO: another way: return ILT, IEQ, FLT, FEQ for both patterns

-- TEST
-- def f { |x| x.include?(".") ? [x.to_f].pack('f').unpack('I').first : x.to_i }
-- rs     f | rt     f | is_float | lt | eq
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
--      0.0 |      0.0 |        1 | 0  | 1
--      1.0 |      0.0 |        1 | 0  | 0
--     -1.0 |      0.0 |        1 | 1  | 0
--     -1.0 |      1.0 |        1 | 1  | 0
--     54.0 |     54.0 |        1 | 0  | 1
--     54.0 |     39.0 |        1 | 0  | 0
--     39.0 |     54.0 |        1 | 1  | 0
--     39.0 |    -54.0 |        1 | 0  | 0
--    -54.0 |     39.0 |        1 | 1  | 0
--    -54.0 |    -54.0 |        1 | 0  | 1
--    -54.0 |    -39.0 |        1 | 1  | 0
--    -39.0 |    -54.0 |        1 | 0  | 0
--      1.1 |      1.0 |        1 | 0  | 0
--      1.0 |      1.1 |        1 | 1  | 0
-- /TEST

entity comparator is
  port (
    rs, rt : in std_logic_vector(31 downto 0);
    is_float : in STD_LOGIC;
    lt, eq : out STD_LOGIC
);
end comparator;

architecture behave of comparator is

begin

  eq <= '1' when rs = rt else '0';

  process (rs, rt, is_float)
  begin
    -- rs is negative, rt is not
    if rs(31) = '1' and rt(31) = '0' then
      lt <= '1';
    -- rs is positive, rt is negative
    elsif rs(31) = '0' and rt(31) = '1' then
      lt <= '0';
    -- float should compared by exponent and mantissa
    elsif rs(31) = '1' and rt(31) = '1' and is_float = '1' then
      if rs(30 downto 0) > rt(30 downto 0) then
        lt <= '1';
      else
        lt <= '0';
      end if;
    -- neg-neg and pos-pos are both comparable
    else
      if rs < rt then
        lt <= '1';
      else
        lt <= '0';
      end if;
    end if;
  end process;

end behave;
