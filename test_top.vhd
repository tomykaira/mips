library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity test_top is
  
  port (
    CLK, XRST              : in     std_logic;
    write_data,  data_addr : buffer std_logic_vector(31 downto 0);
    mem_write              : buffer std_logic);

end test_top;

architecture test of test_top is

  component mips
  end component;

  component imem
  end component;

  component dmem
  end component;

  signal pc, instruction, read_data : std_logic_vector(31 downto 0);

begin  -- test

  mips1 : mips port map(clk, reset, pc, instruction, mem_write, data_addr, write_data, read_data);
  imem1 : imem port map(pc(7 downto 2), instruction);
  dmem1 : dmem port map(clk, mem_write, data_addr, write_data, read_data);

end test;
