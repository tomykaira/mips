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
    RS_TX                  : out std_logic;

		r_data : out std_logic_vector(7 downto 0);
		g_data : out std_logic_vector(7 downto 0);
		b_data : out std_logic_vector(7 downto 0);
		vs_data: out std_logic;
		hs_data: out std_logic;

		KEY_CLK : inout STD_LOGIC;
		KEY_DATA : inout STD_LOGIC;

		SD_CS   : out STD_LOGIC;
		SD_DI   : in STD_LOGIC;
		SD_SCLK : out STD_LOGIC;
		SD_DO   : out STD_LOGIC);

end top;

architecture top of top is

  component mimic
    port (
      clk, reset       : in  STD_LOGIC;

      mem_write_enable : out STD_LOGIC;
      mem_addr         : out std_logic_vector(31 downto 0);
      mem_write_data   : out std_logic_vector(31 downto 0);
      mem_read_data    : in std_logic_vector(31 downto 0);

      tx_send_enable   : out STD_LOGIC;
      tx_send_data     : out std_logic_vector(7 downto 0);

      rx_received_data : in std_logic_vector(7 downto 0);
      rx_waiting       : in STD_LOGIC;
      rx_fifo_pop      : out STD_LOGIC;

      display_buffer_write_enable : out STD_LOGIC;
      display_position : out std_logic_vector(11 downto 0);
      display_char_code : out std_logic_vector(6 downto 0);

			key_status : in std_logic_vector(7 downto 0);
			keycode    : in std_logic_vector(7 downto 0);

			sd_read_data  : in std_logic_vector(7 downto 0);
			sd_write_data : out std_logic_vector(7 downto 0);
			sd_addr       : out std_logic_vector(31 downto 0);
			sd_read       : out STD_LOGIC;
			sd_write      : out STD_LOGIC;
			sd_ready      : in STD_LOGIC);
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

  COMPONENT my_dcm
    PORT(
      CLKIN_IN : IN std_logic;
      RST_IN : IN std_logic;
      CLK_MAIN_OUT : OUT std_logic;
      CLK_100_OUT : OUT std_logic
      );
	END COMPONENT;

  component display is
    port (
      clk : in STD_LOGIC;
      clk100 : in STD_LOGIC;
      reset : in STD_LOGIC;

			buffer_write_enable : in STD_LOGIC;
      position            : in std_logic_vector(11 downto 0);
      char_code           : in std_logic_vector(6 downto 0);

      r_data, g_data, b_data : out std_logic_vector(7 downto 0);
      vs_data, hs_data : out STD_LOGIC);
  end component;

	component keyboard_driver is
		port (
			clk        : in STD_LOGIC;
			key_clk    : in STD_LOGIC;
			key_data   : in STD_LOGIC;
			key_status : out std_logic_vector(7 downto 0);
			keycode    : out std_logic_vector(7 downto 0));
	end component;

	component sdcard is
		port (
			clk    : in STD_LOGIC;
			sd_clk : out STD_LOGIC;
			sd_ce  : out STD_LOGIC;
			sd_out : out STD_LOGIC;
			sd_in  : in STD_LOGIC;

			sd_read_data  : out std_logic_vector(7 downto 0);
			sd_write_data : in std_logic_vector(7 downto 0);
			sd_addr       : in std_logic_vector(31 downto 0);
			sd_read       : in STD_LOGIC;
			sd_write      : in STD_LOGIC;
			sd_ready      : out STD_LOGIC;

			debug : out std_logic_vector(7 downto 0));
	end component;

  signal iclk, clk100 : std_logic;

  signal memory_write : STD_LOGIC;
  signal memory_data_addr, memory_write_data, memory_data : std_logic_vector(31 downto 0);

  signal tx_send_enable, core_tx_send_enable : STD_LOGIC;
  signal tx_send_data, core_tx_send_data : std_logic_vector(7 downto 0);

  signal rx_pop, rx_waiting : STD_LOGIC;
  signal rx_data : std_logic_vector(7 downto 0);

  -- display
  signal display_buffer_write_enable : STD_LOGIC;
  signal display_position            : std_logic_vector(11 downto 0);
  signal display_char_code           : std_logic_vector(6 downto 0);

	-- keyboard
	signal key_status : std_logic_vector(7 downto 0);
	signal keycode    : std_logic_vector(7 downto 0);

	-- sd
	signal sd_read_data, sd_write_data : std_logic_vector(7 downto 0);
	signal sd_addr : std_logic_vector(31 downto 0);
	signal sd_read, sd_write, sd_ready : STD_LOGIC;

	signal debug : std_logic_vector(7 downto 0);

begin  -- test

  data_memory : sramc port map (
    ZD  => ZD,
    ZDP => ZDP,
    ZA  => ZA,
    XWA => XWA,

    clk          => iclk,
    data_read    => memory_data,
    data_write   => memory_write_data,
    address      => memory_data_addr(19 downto 0),
    write_enable => memory_write);

  mimic_inst : mimic port map (
    clk              => iclk,
    reset            => not xrst,
    mem_write_enable => memory_write,
    mem_addr         => memory_data_addr,
    mem_write_data   => memory_write_data,
    mem_read_data    => memory_data,

    tx_send_enable   => core_tx_send_enable,
    tx_send_data     => core_tx_send_data,

    rx_received_data => rx_data,
    rx_waiting       => rx_waiting,
    rx_fifo_pop      => rx_pop,

    display_buffer_write_enable => display_buffer_write_enable,
    display_position            => display_position,
    display_char_code           => display_char_code,

		key_status => key_status,
		keycode    => keycode,

		sd_read_data  => sd_read_data,
		sd_write_data => sd_write_data,
		sd_addr       => sd_addr,
		sd_read       => sd_read,
		sd_write      => sd_write,
		sd_ready      => sd_ready);

  i232c_buffer_inst : i232c_buffer port map (
    clk      => iclk,
    reset    => not xrst,
    rx       => RS_RX,
    do_pop   => rx_pop,
    waiting  => rx_waiting,
    pop_data => rx_data);

  sender : rs232c_buffer port map (
    clk       => iclk,
    reset     => not xrst,
    push      => tx_send_enable,
    push_data => tx_send_data,
    tx        => RS_TX);

	-- workaround for not-stable VGA signal
	tx_send_enable <= core_tx_send_enable;
	tx_send_data   <= core_tx_send_data when core_tx_send_enable = '1' else debug;

	Inst_dcm: my_dcm PORT MAP(
		CLKIN_IN        => CLK,
		RST_IN          => not xrst,
		CLK_MAIN_OUT    => iclk,
		CLK_100_OUT     => clk100);

  display_inst : display port map(
    clk     => iclk,
    clk100  => clk100,
    reset   => not xrst,

    buffer_write_enable => display_buffer_write_enable,
    position            => display_position,
    char_code           => display_char_code,

    r_data  => r_data,
    g_data  => g_data,
    b_data  => b_data,
    vs_data => vs_data,
    hs_data => hs_data);

	keyboard_inst : keyboard_driver port map
		(clk        => iclk,
		 key_clk    => KEY_CLK,
		 key_data   => KEY_DATA,
		 key_status => key_status,
		 keycode    => keycode);

	sd_inst : sdcard port map
		(clk      => iclk,

		 sd_clk   => SD_SCLK,
		 sd_ce    => SD_CS,
		 sd_out   => SD_DO,
		 sd_in    => SD_DI,

		 sd_read_data  => sd_read_data,
		 sd_write_data => sd_write_data,
		 sd_addr       => sd_addr,
		 sd_read       => sd_read,
		 sd_write      => sd_write,
		 sd_ready      => sd_ready,

		 debug => debug);


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

	KEY_CLK <= 'Z';
	KEY_DATA <= 'Z';

end top;
