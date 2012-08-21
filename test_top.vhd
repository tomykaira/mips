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
    port (
      clk, reset          : in  STD_LOGIC;
      pc                  : out STD_LOGIC_VECTOR(31 downto 0);
      instruction         : out STD_LOGIC_VECTOR(31 downto 0);
      mem_write           : out STD_LOGIC;
      alu_out, write_data : out STD_LOGIC_VECTOR(31 downto 0);
      read_data           : in  STD_LOGIC_VECTOR(31 downto 0));
  end component;

  component instruction_memory
     port (
       a  : in  std_logic_vector(5 downto 0);
       rd : out std_logic_vector(31 downto 0));
  end component;

  component data_memory
    port (
      clk, we : in  std_logic;
      a, wd   : in  std_logic_vector(31 downto 0);
      rd      : out std_logic_vector(31 downto 0));
  end component;

  signal pc, instruction, read_data : std_logic_vector(31 downto 0);

begin  -- test

  mips1 : mips port map(clk, not xrst, pc, instruction, mem_write, data_addr, write_data, read_data);
  imem1 : instruction_memory port map(pc(7 downto 2), instruction);
  dmem1 : data_memory port map(clk, mem_write, data_addr, write_data, read_data);

end test;
