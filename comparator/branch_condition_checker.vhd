-- branch condition checker
-- decode an operation and output branch or not

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;

-- float/int x eq/lt/le x true/false
-- comparator is fully tested as an unit
-- TEST
-- depend ./comparator.vhd
-- def f { |x| x.include?(".") ? [x.to_f].pack('f').unpack('I').first : x.to_i }
-- op   b | a   f | b   f | go_branch
-- 100000 | 5     | 5     | 1    # beq
-- 100000 | 1     |       | 0
-- 100001 | 2     |       | 1    # blt
-- 100001 | 8     |       | 0
-- 100010 | 5     |       | 1    # ble
-- 100010 | 8     |       | 0
-- 100100 | 5.0   | 5.0   | 1    # fbeq
-- 100100 | 2.0   |       | 0
-- 100101 | 2.0   |       | 1    # fblt
-- 100101 | 9.0   |       | 0
-- 100110 | 2.0   |       | 1    # fble
-- 100110 | 9.0   |       | 0
-- 111111 | 5.0   |       | 0    # others
-- 111111 | 9.0   |       | 0
-- /TEST

entity branch_condition_checker is
  
  port (
    op        : in std_logic_vector(5 downto 0);
    a, b      : in std_logic_vector(31 downto 0);
    go_branch : out STD_LOGIC
  );

end branch_condition_checker;

architecture behave of branch_condition_checker is

  component comparator is
    port (
      a, b     : in std_logic_vector(31 downto 0);
      is_float : in STD_LOGIC;
      lt, eq   : out STD_LOGIC
      );
  end component;

  signal is_float, lt, eq : STD_LOGIC;

begin  -- behave

  comparator_inst : comparator port map (
    a => a,
    b => b,
    is_float => is_float,
    lt => lt,
    eq => eq
    );

  is_float <= op(2);

  go_branch <= '1' when eq = '1' and (op = "100000" or op = "100010" or op = "100100" or op = "100110") else
               '1' when lt = '1' and (op = "100001" or op = "100010" or op = "100101" or op = "100110") else
               '0';

end behave;
