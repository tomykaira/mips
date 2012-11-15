library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

library UNISIM;
use UNISIM.VComponents.all;

entity top is

  port (
    ZD     : inout std_logic_vector(31 downto 0);
    ZDP    : inout std_logic_vector(3 downto 0);

    ZA     : out std_logic_vector(19 downto 0);
    XE1    : out std_logic;
    E2A    : out std_logic;
    XE3    : out std_logic;
    XZBE   : out std_logic_vector(3 downto 0);
    XGA    : out std_logic;
    XWA    : out std_logic;
    XZCKE  : out std_logic;
    ZCLKMA : out std_logic_vector(1 downto 0);

    ADVA   : out std_logic;
    XFT    : out std_logic;
    XLBO   : out std_logic;
    ZZA    : out std_logic;

    CLK, XRST, RS_RX       : in  std_logic;
    RS_TX                  : out std_logic);

end top;

architecture top of top is

  component mips
    port (
      clk, reset       : in  STD_LOGIC;
      mem_write        : out STD_LOGIC;
      send_enable      : out STD_LOGIC;
      mem_addr         : out STD_LOGIC_VECTOR(31 downto 0);
      write_data       : out std_logic_vector(31 downto 0);
      memory_data      : in  STD_LOGIC_VECTOR(31 downto 0);
      rx_received_data : in std_logic_vector(7 downto 0);
      rx_waiting       : in  STD_LOGIC;
      rx_pop           : out STD_LOGIC);
  end component;

  component sramc is
  Port (
    ZD           : inout std_logic_vector(31 downto 0);
    ZDP          : inout std_logic_vector(3 downto 0);
    ZA           : out std_logic_vector(19 downto 0);
    XWA          : out std_logic;

    clk          : in STD_LOGIC;
    data_read    : out std_logic_vector(31 downto 0);
    data_write   : in std_logic_vector(31 downto 0);
    address      : in std_logic_vector(19 downto 0);
    write_enable : in std_logic
    );
  end component;

  component rs232c_buffer is
    generic (wtime : std_logic_vector(15 downto 0) := x"0005");

    port (
      clk       : in std_logic;
      reset     : in std_logic;
      push      : in std_logic;           -- 1 to push data
      push_data : in std_logic_vector(7 downto 0);
      tx        : out std_logic);

  end component;

  component i232c_buffer is
    
    generic (wtime : std_logic_vector(15 downto 0) := x"0005");

    port (
      clk       : in std_logic;
      reset     : in std_logic;
      rx        : in std_logic;
      do_pop    : in std_logic;           -- 1 to pop data
      waiting   : out STD_LOGIC;
      pop_data  : out std_logic_vector(7 downto 0));

  end component;

  signal data_from_bus, memory_data : std_logic_vector(31 downto 0);

  signal write_data, data_addr : std_logic_vector(31 downto 0);
  signal mem_write : STD_LOGIC;

  signal rx_data : std_logic_vector(7 downto 0);

  signal mclk, iclk : std_logic;

  signal rx_waiting, rx_pop, send_enable : STD_LOGIC;

begin  -- test

  ib: IBUFG port map (
    i=>CLK,
    o=>mclk);
  bg: BUFG port map (
    i=>mclk,
    o=>iclk);

  data_memory : sramc port map (
    ZD  => ZD, 
    ZDP => ZDP,
    ZA  => ZA,
    XWA => XWA,

    clk          => iclk,
    data_read    => memory_data,
    data_write   => write_data,
    address      => data_addr(19 downto 0),
    write_enable => mem_write);

  mips1 : mips port map (
    clk              => iclk,
    reset            => not xrst,
    mem_write        => mem_write,
    send_enable      => send_enable,
    mem_addr         => data_addr,
    write_data       => write_data,
    memory_data      => memory_data,
    rx_received_data => rx_data,
    rx_waiting       => rx_waiting,
    rx_pop           => rx_pop);

  i232c_buffer_inst : i232c_buffer port map (
    clk => iclk,
    reset => not xrst,
    rx => RS_RX,
    do_pop => rx_pop,
    waiting => rx_waiting,
    pop_data => rx_data);

  sender : rs232c_buffer port map (
    clk       => iclk,
    reset     => not xrst,
    push      => send_enable,
    push_data => write_data(7 downto 0),
    tx        => RS_TX);

  XZBE<= "0000";
  XE1 <= '0';
  E2A <= '1';
  XE3 <= '0';
  XGA <= '0';
  XZCKE <= '0';
  ZCLKMA(0) <= iclk;
  ZCLKMA(1) <= iclk;
  ADVA <= '0';
  XFT <= '1';
  XLBO <= '1';
  ZZA <= '0';
  ZDP <=  (others => 'Z');

end top;
