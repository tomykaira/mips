library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity i232c_buffer is
  
  generic (wtime : std_logic_vector(15 downto 0) := x"008F");

  port (
    clk       : in std_logic;
    reset     : in std_logic;
    rx        : in std_logic;
    do_pop    : in std_logic;           -- 1 to pop data
    waiting   : out STD_LOGIC;
    pop_data  : out std_logic_vector(7 downto 0));

end i232c_buffer;

architecture behave of i232c_buffer is

  component i232c
    generic (wtime : std_logic_vector(15 downto 0) := wtime);
    Port ( clk    : in  STD_LOGIC;
         rx     : in  STD_LOGIC;
         data   : out STD_LOGIC_VECTOR (7 downto 0);
         changed: out STD_LOGIC);
  end component;

  -- Current config
  -- Common clock, built-in FIFO
  -- Width: 8, Depth: 1024 (it should be determined from max length of ppm file)
  -- others are default
  COMPONENT fifo
    PORT (
      clk : IN STD_LOGIC;
      rst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC
      );
  END COMPONENT;

  signal rx_changed : std_logic;

  signal full : std_logic := '0';      -- full is not used
  signal rx_data : std_logic_vector(7 downto 0);

begin  -- behave

  receiver : i232c port map (
    clk     => clk,
    rx      => rx,
    data    => rx_data,
    changed => rx_changed);

  received_data_queue : fifo
    PORT MAP (
      clk   => clk,
      rst   => reset,
      din   => rx_data,
      wr_en => rx_changed,
      rd_en => do_pop,
      dout  => pop_data,
      full  => full,
      empty => waiting
      );

end behave;
