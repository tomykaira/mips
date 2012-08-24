library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity test_top is
  
  port (
    CLK, XRST              : in  std_logic;
    RS_RX                  : in  std_logic;
    send_data              : out std_logic_vector(31 downto 0);
    send_enable            : out std_logic);

end test_top;

architecture test of test_top is

  component mips
    port (
      clk, reset          : in  STD_LOGIC;
      pc                  : out STD_LOGIC_VECTOR(31 downto 0);
      instruction         : in  STD_LOGIC_VECTOR(31 downto 0);
      mem_write           : out STD_LOGIC;
      send_enable         : out STD_LOGIC;
      alu_out, write_data : out STD_LOGIC_VECTOR(31 downto 0);
      data_from_bus       : in  STD_LOGIC_VECTOR(31 downto 0);
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

  signal pc, instruction, data_from_bus, memory_data : std_logic_vector(31 downto 0);

  signal write_data, data_addr : std_logic_vector(31 downto 0);
  signal mem_write : STD_LOGIC;

  signal rx_data : std_logic_vector(7 downto 0);
  signal rx_enable : STD_LOGIC;
  signal rx_done : STD_LOGIC;

begin  -- test

  mips1 : mips port map(clk, not xrst, pc, instruction, mem_write, send_enable, data_addr, write_data, data_from_bus, rx_enable, rx_done);
  imem1 : instruction_memory port map(pc(7 downto 2), instruction);
  dmem1 : data_memory port map(clk, mem_write, data_addr, write_data, memory_data);

  rx : i232c port map(clk, rx_enable, RS_RX, rx_data, rx_done);

  send_data <= write_data;

  -- is this good design to judge here?
  -- ok for reading twice?
  data_from_bus <= x"000000" & rx_data when rx_enable = '1' or rx_done = '1' else memory_data;

end test;
