library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.all;

-- Calculate next pc from internal state and other states, such as a register
-- and branch condition.

-- TEST
-- current_pc | current_kind | decoded_addr | prev_is_jump_reg | prev_register | prev_prev_is_branch | prev_prev_taken | prev_prev_address | stack_top | next_pc | pop_stack
--        100 |            0 |           77 |                0 |            88 |                   0 |               0 |                99 |        44 |     100 |         0
--            |              |              |                1 |               |                   0 |               0 |                   |           |      88 |         0
--            |              |              |                0 |               |                   1 |               0 |                   |           |     101 |         0
--            |              |              |                0 |               |                   1 |               1 |                   |           |      99 |         0
--            |              |              |                1 |               |                   1 |               0 |                   |           |     101 |         0
--            |            1 |              |                0 |               |                   0 |               0 |                   |           |     101 |         0
--            |              |              |                1 |               |                   0 |               0 |                   |           |      88 |         0
--            |              |              |                0 |               |                   1 |               0 |                   |           |     101 |         0
--            |              |              |                0 |               |                   1 |               1 |                   |           |      99 |         0
--            |              |              |                1 |               |                   1 |               0 |                   |           |     101 |         0
--            |            2 |              |                0 |               |                   0 |               0 |                   |           |      77 |         0
--            |              |              |                1 |               |                   0 |               0 |                   |           |      88 |         0 # previous inst has higher priority
--            |              |              |                0 |               |                   1 |               0 |                   |           |     101 |         0
--            |              |              |                0 |               |                   1 |               1 |                   |           |      99 |         0
--            |              |              |                1 |               |                   1 |               0 |                   |           |     101 |         0
--            |            3 |              |                0 |               |                   0 |               0 |                   |           |      44 |         1
--            |              |              |                1 |               |                   0 |               0 |                   |           |      88 |         0
--            |              |              |                0 |               |                   1 |               0 |                   |           |     101 |         0
--            |              |              |                0 |               |                   1 |               1 |                   |           |      99 |         0
--            |              |              |                1 |               |                   1 |               0 |                   |           |     101 |         0
-- /TEST

entity program_counter_calculator is
  port (current_pc          : in std_logic_vector(31 downto 0);
        current_kind        : in std_logic_vector(1 downto 0); -- jump(2) / return(3) / other(1) / current(0)
        decoded_addr        : in std_logic_vector(31 downto 0);
        prev_is_jump_reg    : in STD_LOGIC;                    -- jump_reg, call_reg
        prev_register       : in std_logic_vector(31 downto 0);
        prev_prev_is_branch : in STD_LOGIC;                    -- branch
        prev_prev_taken     : in STD_LOGIC;
        prev_prev_address   : in std_logic_vector(31 downto 0);
        stack_top           : in std_logic_vector(31 downto 0);-- return
        next_pc             : out std_logic_vector(31 downto 0);
        pop_stack           : out STD_LOGIC);
end program_counter_calculator;

architecture behave of program_counter_calculator is
begin
  pop_stack <= '1' when current_kind = "11" and prev_is_jump_reg = '0' and prev_prev_is_branch = '0' else '0';

  process (current_pc, current_kind, decoded_addr, prev_is_jump_reg,
           prev_register, prev_prev_is_branch, prev_prev_taken,
           prev_prev_address, stack_top)
  begin

    if prev_prev_is_branch = '1' then
      if prev_prev_taken = '1' then
        next_pc <= prev_prev_address;
      else
        next_pc <= current_pc + 1;
      end if;
    elsif prev_is_jump_reg = '1' then
      next_pc <= prev_register;
    elsif current_kind = "10" then
      next_pc <= decoded_addr;
    elsif current_kind = "11" then
      next_pc <= stack_top;
    elsif current_kind = "01" then
      next_pc <= current_pc + 1;
    else
      next_pc <= current_pc;
    end if;
      
  end process;
end behave;
