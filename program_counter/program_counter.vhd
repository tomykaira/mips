library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Manage program counter
-- PC and next PC are internally held

entity program_counter is
    port (
      clk   : in STD_LOGIC;
      reset : in std_logic;
      pc    : out std_logic_vector(31 downto 0);

      -- needed information
      pc_src           : in std_logic_vector(2 downto 0);
      jump             : in std_logic_vector(25 downto 0);
      relative         : in std_logic_vector(31 downto 0); -- sign extended
      reg              : in std_logic_vector(31 downto 0);
      stack_top        : in std_logic_vector(31 downto 0);
      branch_condition : in STD_LOGIC
    );

end program_counter;

architecture statemachine of program_counter is

  component next_pc_composer is

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

  end component;

  component flip_reset
    generic (width : integer := 32);

    port (
      clk, reset : in  std_logic;
      d          : in  std_logic_vector(width-1 downto 0);
      q          : out std_logic_vector(width-1 downto 0));
  end component;

  signal pc_buf, pc_next : std_logic_vector(31 downto 0) := (others => '0');

begin -- statemachine

  pc_register : flip_reset port map (
    clk   => clk,
    reset => reset,
    d     => pc_next,
    q     => pc_buf);

  pc_composer : next_pc_composer port map (
    pc_src     => pc_src,
    current_pc => pc_buf,
    jump       => jump,
    relative   => relative,
    reg        => reg,
    stack      => stack_top,
    branch     => branch_condition,
    pc         => pc_next);

  pc <= pc_buf;

end statemachine
