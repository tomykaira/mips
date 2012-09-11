-- call stack
-- thin wrapper for stack

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity call_stack is
  
  port (
    clk        : in STD_LOGIC;
    -- decide to push
    op         : in std_logic_vector(5 downto 0);
    -- as input data
    current_pc : in std_logic_vector(31 downto 0);
    pc_src     : in std_logic_vector(2 downto 0);

    stack_top  : out std_logic_vector(31 downto 0)
  );

end call_stack;

architecture behave of call_stack is

  component stack is
    generic (
      width : integer := 32;
      depth : integer := 256
      );
    port(   Clk         : in std_logic;  --Clock for the stack.
            Enable      : in std_logic;  --Enable the stack. Otherwise neither push nor pop will happen.
            Data_In     : in std_logic_vector(width-1 downto 0);  --Data to be pushed to stack
            Data_Out    : out std_logic_vector(width-1 downto 0);  --Data popped from the stack.
            PUSH_barPOP : in std_logic;  --active low for POP and active high for PUSH.
            Stack_Full  : out std_logic;  --Goes high when the stack is full.
            Stack_Empty : out std_logic  --Goes high when the stack is empty.
            );
  end component;

  signal full, empty, push_or_pop, do_push, do_pop : STD_LOGIC;
  
begin  -- behave

  pc_stack : stack port map (
    Clk         => clk,
    Enable      => do_push or do_pop,
    Data_In     => current_pc,
    Data_Out    => stack_top,
    PUSH_barPOP => do_push,
    Stack_Full  => full, -- not used
    Stack_Empty => empty -- not used
    );

  assert (do_pop = '0' or empty = '0') report "PC Stack is empty and popping";
  assert full = '0'  report "PC Stack is full";

  -- push: call and leaving this instruction
  do_push <= '1' when pc_src /= "000" and (op = "111010" or op = "111011")
             else '0';

  -- pop: return and top is loaded to PC-flipflop
  do_pop <= '1' when pc_src = "101" and op = "111100"
            else '0';
  
end behave;
