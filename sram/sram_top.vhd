library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

library UNISIM;
use UNISIM.VComponents.all;

entity sram_top is

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

end sram_top;

architecture sram_top of sram_top is

  constant WRITE_LIMIT : integer := 1;
  constant READ_LIMIT  : integer := 1;

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
    generic (wtime : std_logic_vector(15 downto 0) := x"008F");

    port (
      clk       : in std_logic;
      reset     : in std_logic;
      push      : in std_logic;           -- 1 to push data
      push_data : in std_logic_vector(7 downto 0);
      tx        : out std_logic);

  end component;

  component i232c
    port ( clk    : in  STD_LOGIC;
           rx     : in  STD_LOGIC;
           data   : out STD_LOGIC_VECTOR (7 downto 0);
           changed: out STD_LOGIC);
  end component;

  constant LIMIT : integer := 3;

  signal memory_data : std_logic_vector(31 downto 0);
  signal data_write : std_logic_vector(31 downto 0);
  signal data_addr : std_logic_vector(31 downto 0) := x"00000082";
  signal mem_write_enable : STD_LOGIC;

  signal rx_data : std_logic_vector(7 downto 0);

  signal mclk, iclk : std_logic;

  signal rx_done, push_enable : STD_LOGIC;

  signal counter : std_logic_vector(9 downto 0) := (others => '0');

  type statetype is (INPUT, READING, WRITING);
  signal state : statetype := INPUT;

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
    data_write   => data_write,
    address      => data_addr(19 downto 0),
    write_enable => mem_write_enable);

  sender : rs232c_buffer port map (
    clk       => iclk,
    reset     => not xrst,
    push      => push_enable,
    push_data => memory_data(7 downto 0),
    tx        => RS_TX);

  push_enable <= '1' when state = READING else '0';
  
  receiver : i232c port map (
    clk     => iclk,
    rx      => RS_RX,
    data    => rx_data,
    changed => rx_done);

  XZBE<= "0000";
  XE1 <= '0';
  E2A <= '1';
  XE3 <= '0';
  XGA <= '0';
  XZCKE <= '0';
  ZCLKMA(0) <= iclk;
  ZCLKMA(1) <= iclk;
  ADVA <= '0';
  XFT <= '0';
  XLBO <= '1';
  ZZA <= '0';
  ZDP <=  (others => 'Z');

  statemachine : process (iclk, xrst)
  begin
    if xrst = '0' then
      state <= INPUT;
      counter <= (others => '0');
    elsif rising_edge(iclk) then
      counter <= counter + 1;
      mem_write_enable <= '0';
      data_addr <= (others => '0');
      data_write <= (others => '0');

      if state = INPUT and rx_done = '1' then
        counter <= (others => '0');
        state <= WRITING;
        data_addr  <= x"000000" & rx_data;
        mem_write_enable <= '1';
        data_write <= x"000000" & rx_data;

      elsif state = WRITING and counter >= WRITE_LIMIT then
        counter <= (others => '0');
        state <= READING;
        data_addr  <= x"000000" & rx_data;

      elsif state = READING and counter >= READ_LIMIT then
        counter <= (others => '0');
        state <= INPUT;
      end if;
    end if;
  end process;


end sram_top;
