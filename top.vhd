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

  component rs232c_buffer is
    generic (wtime : std_logic_vector(15 downto 0) := x"008F");

    port (
      clk       : in std_logic;
      reset     : in std_logic;
      push      : in std_logic;           -- 1 to push data
      push_data : in std_logic_vector(31 downto 0);
      tx        : out std_logic);

  end component;

  component i232c
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

  signal mclk, iclk : std_logic;

  signal rx_enable, rx_done, send_enable : STD_LOGIC;

begin  -- test

  ib: IBUFG port map (
    i=>CLK,
    o=>mclk);
  bg: BUFG port map (
    i=>mclk,
    o=>iclk);

  imem1 : instruction_memory port map(pc(7 downto 2), instruction);
  dmem1 : data_memory port map(iclk, mem_write, data_addr, write_data, memory_data);

  mips1 : mips port map (
    clk           => iclk,
    reset         => not xrst,
    pc            => pc,
    instruction   => instruction,
    mem_write     => mem_write,
    send_enable   => send_enable,
    alu_out       => data_addr,
    write_data    => write_data,
    data_from_bus => data_from_bus,
    rx_enable     => rx_enable,
    rx_done       => rx_done);

  receiver : i232c port map (
    clk     => iclk,
    enable  => rx_enable,
    rx      => RS_RX,
    data    => rx_data,
    changed => rx_done);

  sender : rs232c_buffer port map (
    clk       => iclk,
    reset     => not xrst,
    push      => send_enable,
    push_data => write_data,
    tx        => RS_TX);
  
  -- is this good design to judge here?
  -- ok for reading twice?
  data_from_bus <= x"000000" & rx_data when rx_enable = '1' or rx_done = '1' else memory_data;

end top;
