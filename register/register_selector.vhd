library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Select float register or integer register for each operand.
-- This works as a facade to registers.

entity register_selector is
  port (
    -- register signals, connected to each register_file
    clk                                 : in  std_logic;
    write_enable3                       : in  std_logic;
    read_addr1, read_addr2, write_addr3 : in  std_logic_vector(4 downto 0);
    write_data3                         : in  std_logic_vector(31 downto 0);
    read_data1, read_data2              : out std_logic_vector(31 downto 0);

    -- instruction operator to judge which port is used
    op : in std_logic_vector(6 downto 0)
);
end register_selector;

architecture behave of register_selector is

  -- use the same entity for floating and integer
  component register_file is
    port (
      clk                                 : in  std_logic;
      write_enable3                       : in  std_logic;
      read_addr1, read_addr2, write_addr3 : in  std_logic_vector(4 downto 0);
      write_data3                         : in  std_logic_vector(31 downto 0);
      read_data1, read_data2              : out std_logic_vector(31 downto 0));
  end component;

  component register_decoder is
    port (
      -- instruction operator to judge which port is used
      op : in std_logic_vector(6 downto 0);
      -- bounded register file. 0 for int, 1 for float
      addr1, addr2, addr3 : out STD_LOGIC
      );
  end component;

  -- read data
  signal int_read_data1, int_read_data2, float_read_data1, float_read_data2 : std_logic_vector(31 downto 0);

  -- write enable for each register file
  signal int_write, float_write : STD_LOGIC;

  -- which is connected
  signal addr1_float, addr2_float, addr3_float : STD_LOGIC;

begin -- behave

  int_register : register_file port map (
    clk           => clk,
    write_enable3 => int_write,
    read_addr1    => read_addr1,
    read_addr2    => read_addr2,
    write_addr3   => write_addr3,
    write_data3   => write_data3,
    read_data1    => int_read_data1,
    read_data2    => int_read_data2);
    
  float_register : register_file port map (
    clk           => clk,
    write_enable3 => float_write,
    read_addr1    => read_addr1,
    read_addr2    => read_addr2,
    write_addr3   => write_addr3,
    write_data3   => write_data3,
    read_data1    => float_read_data1,
    read_data2    => float_read_data2);

  decoder : register_decoder port map (
    op    => op,
    addr1 => addr1_float,
    addr2 => addr2_float,
    addr3 => addr3_float);

  -- write enable control
  int_write   <= '1' when addr3_float = '0' and write_enable3 = '1' else '0';
  float_write <= '1' when addr3_float = '1' and write_enable3 = '1' else '0';

  -- output MUX
  read_data1  <= float_read_data1 when addr1_float = '1' else int_read_data1;
  read_data2  <= float_read_data2 when addr2_float = '1' else int_read_data2;
  
end behave;
