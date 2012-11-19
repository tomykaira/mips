library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.all;

-- Decoder for program counter
-- It handle JUMP, JUMP_REG, CALL, CALL_REG, RETURN, BRANCHES, and
-- parse them into internal representation.

-- TEST
-- alias J     11100000000000000000000000110101 # jump to 53
-- alias JR    11100100010000000000000000000000 # jump to $r2
-- alias CALL  11101000000000000000000000110101 # call to 53
-- alias CALLR 11101100010000000000000000000000 # call to $r2
-- alias RET   11110000000000000000000000000000
-- alias BEQ   10000000000000000000000000110101 # branch to pc + 53
-- alias BLT   10000100000000001111111111111110 # branch to pc - 2
-- alias ADD   00000000000000000000000000000000
-- inst b| current_pc | kind | is_jump_reg | is_branch | address | push_stack
-- J     |        102 |    2 |           0 |         0 |      53 |          0
-- JR    |            |    0 |           1 |         0 |       - |          0
-- CALL  |            |    2 |           0 |         0 |      53 |          1
-- CALLR |            |    0 |           1 |         0 |       - |          1
-- RET   |            |    3 |           0 |         0 |       - |          0
-- BEQ   |            |    1 |           0 |         1 |     155 |          0
-- BLT   |            |    1 |           0 |         1 |     100 |          0
-- ADD   |            |    1 |           0 |         0 |       - |          0
-- /TEST


entity program_counter_decoder is
  port(inst        : in std_logic_vector(31 downto 0);
       current_pc  : in std_logic_vector(31 downto 0);
       kind        : out std_logic_vector(1 downto 0);
       is_jump_reg : out STD_LOGIC;
       is_branch   : out STD_LOGIC;
       address     : out std_logic_vector(31 downto 0);
       push_stack  : out STD_LOGIC);
end program_counter_decoder;

architecture behave of program_counter_decoder is
  signal op : std_logic_vector(5 downto 0);
  signal is_jump, i_is_jump_reg, i_is_branch : STD_LOGIC;
begin

  is_branch <= i_is_branch;
  is_jump_reg <= i_is_jump_reg;

  op <= inst(31 downto 26);
  kind <= "00" when i_is_jump_reg = '1' else -- how about other stalls?
          "10" when is_jump = '1' else
          "11" when op = "111100" else
          "01";

  is_jump <= '1' when op = "111000" or op = "111010" else '0';
  i_is_jump_reg <= '1' when op = "111001" or op = "111011" else '0';
  i_is_branch <= '1' when op(5 downto 3) = "100" else '0';
  push_stack <= '1' when op = "111010" or op = "111011" else '0';

  process (inst, current_pc, i_is_branch, is_jump)
    variable imm : std_logic_vector(31 downto 0);
  begin
    if is_jump = '1' then
      address <= current_pc(31 downto 26) & inst(25 downto 0);
    elsif i_is_branch = '1' then
      imm := (others => inst(15));
      imm(15 downto 0) := inst(15 downto 0);
      address <= current_pc + imm;
    else
      address <= (others => 'Z');
    end if;
  end process;
end behave;
