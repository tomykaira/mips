-- call stack
-- thin wrapper for stack

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity call_stack is
  
  port (
    clk        : in STD_LOGIC;

    do_push, do_pop : in STD_LOGIC;

    -- as input data
    current_pc : in std_logic_vector(31 downto 0);
    stack_top  : out std_logic_vector(31 downto 0)
  );

end call_stack;

architecture behave of call_stack is

  component stack is
    generic (
      width : integer := 32
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

  signal full, empty, enable : STD_LOGIC;
  
begin  -- behave

  pc_stack : stack port map (
    Clk         => clk,
    Enable      => enable,
    Data_In     => current_pc,
    Data_Out    => stack_top,
    PUSH_barPOP => do_push,
    Stack_Full  => full, -- not used
    Stack_Empty => empty -- not used
    );

  enable <= do_push or do_pop;

  assert (do_pop = '0' or empty = '0') report "PC Stack is empty and popping";
  assert full = '0'  report "PC Stack is full";

end behave;
