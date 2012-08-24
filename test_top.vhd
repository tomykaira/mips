library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity test_top is
  
  port (
    CLK, XRST              : in  std_logic;
    RS_RX                  : in  std_logic;
    write_data,  data_addr : out std_logic_vector(31 downto 0);
    mem_write              : out std_logic);

end test_top;

architecture test of test_top is

  component mips
    port (
      clk, reset          : in  STD_LOGIC;
      pc                  : out STD_LOGIC_VECTOR(31 downto 0);
      instruction         : in  STD_LOGIC_VECTOR(31 downto 0);
      mem_write           : out STD_LOGIC;
      alu_out, write_data : out STD_LOGIC_VECTOR(31 downto 0);
      read_data           : in  STD_LOGIC_VECTOR(31 downto 0);
      rx_enable           : out STD_LOGIC;
      rx_done             : in  STD_LOGIC);
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

  component i232c
    generic (wtime: std_logic_vector(15 downto 0) := x"0004");
    port ( clk    : in  STD_LOGIC;
           enable : in  STD_LOGIC;
           rx     : in  STD_LOGIC;
           data   : out STD_LOGIC_VECTOR (7 downto 0);
           changed: out STD_LOGIC);
  end component;

  signal pc, instruction, read_data, memory_data : std_logic_vector(31 downto 0);

  signal write_data_buf, data_addr_buf : std_logic_vector(31 downto 0);
  signal mem_write_buf : STD_LOGIC;

  signal rx_data : std_logic_vector(7 downto 0);
  signal rx_enable : STD_LOGIC;
  signal rx_done : STD_LOGIC;

begin  -- test

  mips1 : mips port map(clk, not xrst, pc, instruction, mem_write_buf, data_addr_buf, write_data_buf, read_data, rx_enable, rx_done);
  imem1 : instruction_memory port map(pc(7 downto 2), instruction);
  dmem1 : data_memory port map(clk, mem_write_buf, data_addr_buf, write_data_buf, memory_data);

  rx : i232c port map(clk, rx_enable, RS_RX, rx_data, rx_done);

  write_data <= write_data_buf;
  data_addr  <= data_addr_buf;
  mem_write  <= mem_write_buf;

  -- is this good design to judge here?
  read_data <= x"000000" & rx_data when rx_enable = '1' else memory_data;

end test;
