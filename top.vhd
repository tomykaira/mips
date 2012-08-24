library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

library UNISIM;
use UNISIM.VComponents.all;

entity top is

  port (
    CLK, XRST, RS_RX       : in  std_logic;
    RS_TX                  : out std_logic);

end top;

architecture top of top is

  component mips
    port (
      clk, reset          : in  STD_LOGIC;
      pc                  : out STD_LOGIC_VECTOR(31 downto 0);
      instruction         : in  STD_LOGIC_VECTOR(31 downto 0);
      mem_write           : out STD_LOGIC;
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

  component u232c
    port (
      clk  : in  STD_LOGIC;
      data : in  STD_LOGIC_VECTOR (7 downto 0);
      go   : in  STD_LOGIC;
      busy : out STD_LOGIC;
      tx   : out STD_LOGIC);
  end component;

  component i232c
    port ( clk    : in  STD_LOGIC;
           enable : in  STD_LOGIC;
           rx     : in  STD_LOGIC;
           data   : out STD_LOGIC_VECTOR (7 downto 0);
           changed: out STD_LOGIC);
  end component;

  signal pc, instruction, data_from_bus, memory_data : std_logic_vector(31 downto 0);

  signal write_data_buf, data_addr_buf : std_logic_vector(31 downto 0);
  signal mem_write_buf : STD_LOGIC;

  signal rx_data, tx_data : std_logic_vector(7 downto 0);
  signal busy, go : std_logic;

  signal mclk, iclk : std_logic;

  signal rx_enable, rx_done : STD_LOGIC;

begin  -- test

  ib: IBUFG port map (
    i=>CLK,
    o=>mclk);
  bg: BUFG port map (
    i=>mclk,
    o=>iclk);

  mips1 : mips port map(iclk, not xrst, pc, instruction, mem_write_buf, data_addr_buf, write_data_buf, data_from_bus, rx_enable, rx_done);
  imem1 : instruction_memory port map(pc(7 downto 2), instruction);
  dmem1 : data_memory port map(iclk, mem_write_buf, data_addr_buf, write_data_buf, memory_data);
  receiver : i232c port map (iclk, rx_enable, RS_RX, rx_data, rx_done);
  sender : u232c port map (iclk, tx_data, go, busy, RS_TX);

  tx_data <= write_data_buf(7 downto 0);
  go <= '1' when busy = '0' and conv_integer(data_addr_buf) = 88 else '0';

  -- is this good design to judge here?
  -- ok for reading twice?
  data_from_bus <= x"000000" & rx_data when rx_enable = '1' or rx_done = '1' else memory_data;

end top;
