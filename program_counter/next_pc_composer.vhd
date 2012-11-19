library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.all;

-- Calculate next PC value from given vectors and pc_src from controller.
-- PC is valid for least 16bits, others are roughly calculated.

-- TEST
-- alias NEXT 001
-- alias J    010
-- alias REL  011 # conditional relative branching
-- alias REG  100
-- alias LR   101
-- pc_src | current_pc | jump | relative | reg | stack_top | branch_condition | pc_next
--    CUR |        100 | 1111 |     2222 | 333 |      4444 |                0 | 100
--   NEXT |        100 | 1111 |     2222 | 333 |      4444 |                0 | 101
--   NEXT |          0 | 1111 |     2222 | 333 |      4444 |                0 | 1
--      J |        100 |      |          |     |           |                  | 1111
--    REL |            |      |     2222 |     |           |                0 | 101
--    REL |            |      |      -42 |     |           |                0 | 101
--    REL |            |      |     2222 |     |           |                1 | 2322
--    REL |            |      |      -42 |     |           |                1 | 58
--    REG |            |      |     2222 |     |           |                0 | 333
--     LR |            |      |          |     |           |                  | 4445
-- /TEST

entity next_pc_composer is

  port (
    pc_src           : in std_logic_vector(2 downto 0);
    current_pc       : in std_logic_vector(31 downto 0);
    jump             : in std_logic_vector(25 downto 0);
    relative         : in std_logic_vector(31 downto 0); -- sign extended
    reg              : in std_logic_vector(31 downto 0);
    stack_top        : in std_logic_vector(31 downto 0);
    branch_condition : in STD_LOGIC;
    pc_next          : out std_logic_vector(31 downto 0)
    );

end next_pc_composer;

architecture behave of next_pc_composer is

  -- sync with main_decoder and doctest
  constant F_CUR      : std_logic_vector(2 downto 0) := "000";
  constant F_NEXT  : std_logic_vector(2 downto 0) := "001";
  constant F_JUMP     : std_logic_vector(2 downto 0) := "010";
  constant F_RELATIVE : std_logic_vector(2 downto 0) := "011";
  constant F_REG      : std_logic_vector(2 downto 0) := "100";
  constant F_LR       : std_logic_vector(2 downto 0) := "101";

begin  -- behave

  pc_next <= current_pc + 1        when pc_src = F_NEXT or (pc_src = F_RELATIVE and branch_condition = '0') else
             "000000" & jump       when pc_src = F_JUMP else
             current_pc + relative when pc_src = F_RELATIVE and branch_condition = '1' else
             reg                   when pc_src = F_REG else
             -- next to current PC
             stack_top + 1         when pc_src = F_LR else
             current_pc; -- default

end behave;
