library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_1164.ALL;

entity sd_cont is
	port(
		clk    : in  std_logic;

		sd_clk : out std_logic;
		sd_ce  : out std_logic;
		sd_out : out std_logic;
		sd_in  : in  std_logic;

		sd_index        : in  std_logic_vector(8 downto 0);
		sd_read_data    : out std_logic_vector(7 downto 0);
		sd_write_data   : in std_logic_vector(7 downto 0);
		sd_write_enable : in STD_LOGIC;

		sd_block  : in  std_logic_vector(22 downto 0);
		sd_read  : in  std_logic;
		sd_write : in	STD_LOGIC;
		sd_busy  : out STD_LOGIC := '1';

		debug : out std_logic_vector(9 downto 0)
	);
end sd_cont;

architecture Behavioral of sd_cont is
	component spi_cont
		port(
			clk	: in  std_logic;

			spi_clk	: out std_logic;
			spi_out	: out std_logic;
			spi_in	: in  std_logic;
			spi_busy: out std_logic;

			spi_rdata	: out std_logic_vector(7 downto 0);
			spi_sdata	: in  std_logic_vector(7 downto 0);
			spi_go		: in  std_logic;
			spi_delay	: in  std_logic_vector(7 downto 0)
		);
	end component;
	component sd_memory
		port(
			clk 	: in  std_logic;
			addra	: in  std_logic_vector(8 downto 0);
			dina	: in  std_logic_vector(7 downto 0);
			wea	: in  std_logic;
			addrb	: in  std_logic_vector(8 downto 0);
			doutb	: out std_logic_vector(7 downto 0)
		);
	end component;

	signal spi_rdata	: std_logic_vector(7 downto 0);
	signal spi_sdata	: std_logic_vector(7 downto 0);
	signal spi_busy		: std_logic;
	signal spi_go		: std_logic;
	signal spi_delay	: std_logic_vector(7 downto 0) := (others => '1');

	signal state	: std_logic_vector(9 downto 0) := (others => '0');
	signal index	: std_logic_vector(5 downto 0);
	signal argument	: std_logic_vector(31 downto 0);
	signal resp	: std_logic_vector(7 downto 0);

	-- block RAM
	signal write_addr, ram_write_addr	: std_logic_vector(8 downto 0) := (others => '0');
	signal write_data, ram_write_data	: std_logic_vector(7 downto 0);
	signal we_a, ram_write_enable	: STD_LOGIC;
	signal sd_write_mode : STD_LOGIC;
	signal read_addr, ram_read_addr : std_logic_vector(8 downto 0);
	signal ram_data : std_logic_vector(7 downto 0);
begin

	sd_read_data <= ram_data;
	debug <= state;

	process(clk)begin
		if rising_edge(clk)then

			if state(1 downto 0) = "01" then
				spi_go <= '1';
				state <= state + '1';
			elsif state(1 downto 0) = "10" then
				spi_go <= '0';
				state <= state + '1';
			elsif state(1 downto 0) = "11" then
				if spi_busy = '0' then
					state <= state + '1';
				end if;
			--------------------------------------------------------------------------
			elsif state(9 downto 7) = "000" then		-- CLK 8 * 16
				spi_sdata <= "11111111";
				sd_ce <= '1';
				spi_delay <= "11111111";
				state <= state + '1';
				sd_busy <= '1';
			--------------------------------------------------------------------------
			elsif state(6 downto 2) = "00001" then		-- Command Index
				spi_sdata <= "01" & index;
				sd_ce     <= '0';
				state <= state + '1';
			elsif state(6 downto 2) = "00010" then		-- Argument 1
				spi_sdata <= argument(31 downto 24);
				state <= state + '1';
			elsif state(6 downto 2) = "00011" then		-- Argument 2
				spi_sdata <= argument(23 downto 16);
				state <= state + '1';
			elsif state(6 downto 2) = "00100" then		-- Argument 3
				spi_sdata <= argument(15 downto 8);
				state <= state + '1';
			elsif state(6 downto 2) = "00101" then		-- Argument 4
				spi_sdata <= argument(7  downto 0);
				state <= state + '1';
			elsif state(6 downto 2) = "00110" then		-- CRC
				spi_sdata <= "10010101";
				state <= state + '1';
			elsif state(6 downto 2) = "00111" then		-- Response
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(6 downto 2) = "01000" then		-- Response Check
				if spi_rdata = "11111111" then	-- Not received
					state <= state - "100";
				else				-- Received
					state <= state + "100";
					resp  <= spi_rdata;
				end if;
			elsif state(6 downto 2) = "01001" then		-- Finish or Continue(Cmd17,  Cmd24)
				if index = 17 then	-- CMD17
					state <= state + "100";
				elsif index = 24 then	-- CMD24
					state(6 downto 2) <= "10001";
				else			-- Finish
					state(6 downto 2) <= "00000";
					state(9 downto 7) <= state(9 downto 7) + '1';
					sd_ce <= '1';
				end if;

			----------------
			elsif state(6 downto 2) = "01010" then		-- Wait Data Token
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(6 downto 2) = "01011" then		-- Check if rdata is Data Token
				if spi_rdata = "11111110" then
					state <= state + "100";
				else
					state <= state - "100";
				end if;
				write_addr <= "111111111";
			elsif state(6 downto 2) = "01100" then		-- Receive Data
				spi_sdata <= "11111111";
				we_a      <= '0';
				state <= state + '1';
			elsif state(6 downto 2) = "01101" then		-- Loop
				write_addr  <= write_addr + '1';
				we_a    <= '1';
				write_data  <= spi_rdata;
				if write_addr = "111111110" then
					state <= state + "100";
				else
					state <= state - "100";
				end if;
			elsif state(6 downto 2) = "01110" then		-- CRC1
				spi_sdata <= "11111111";
				we_a      <= '0';
				state <= state + '1';
			elsif state(6 downto 2) = "01111" then		-- CRC2
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(6 downto 2) = "10000" then		-- Finish read
				state(6 downto 2) <= "00000";
				state(9 downto 7) <= state(9 downto 7) + '1';

			----------------
			-- usually you should wait 1 byte here, but my card should not
			elsif state(6 downto 2) = "10001" then
				state <= state + "100";
			elsif state(6 downto 2) = "10010" then		-- Send token and setup to read from BRAM
				spi_sdata <= x"fe";
				state <= state + '1';
				sd_write_mode <= '1';
				read_addr <= (others => '0');
			elsif state(6 downto 2) = "10011" then		-- Wait BRAM
				state <= state + "100";
			elsif state(6 downto 2) = "10100" then		-- Send data
				spi_sdata <= ram_data;
				state <= state + '1';
			elsif state(6 downto 2) = "10101" then		-- Loop
				read_addr <= read_addr + 1;
				if read_addr = "111111111" then
					state(6 downto 2) <= state(6 downto 2) + 1;
				else
					state(6 downto 2) <= state(6 downto 2) - 2;
				end if;
			elsif state(6 downto 2) = "10110" then		-- CRC1 (dummy)
				spi_sdata <= "11111111";
				sd_write_mode <= '0';
				state <= state + '1';
			elsif state(6 downto 2) = "10111" then		-- CRC2
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(6 downto 2) = "11000" then		-- Read data response
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(6 downto 2) = "11001" then		-- polling
				state <= state + '1';
			elsif state(6 downto 2) = "11010" then		-- Wait while busy, then Finish
				if sd_in = '0' then
					state <= state - "100";
				else
					state(6 downto 2) <= "00000";
					state(9 downto 7) <= state(9 downto 7) + '1';
				end if;

			--------------------------------------------------------------------------------------
			---- Initialization Sequence ----
			elsif state(9 downto 7) = "001" then		-- CMD 0
				index <= "000000";
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(9 downto 7) = "010" then		-- CMD 55
				index <= conv_std_logic_vector(55, 6);
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(9 downto 7) = "011" then		-- CMD 41
				index <= conv_std_logic_vector(41, 6);
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(9 downto 7) = "100" then		-- Response Check
				if resp = "00000000" then
					state(9 downto 7) <= state(9 downto 7) + "1";
				else
					state(9 downto 7) <= state(9 downto 7) - "10";
				end if;
			elsif state(9 downto 7) = "101" then		-- CMD 16
				spi_delay <= "00000101";
				index    <= conv_std_logic_vector(16, 6);
				argument <= conv_std_logic_vector(512, 32);
				state <= state + "100";
				sd_ce <= '1';
			-----------------------------------------------------------------------------------------
			---- Loop ----
			elsif state(9 downto 7) = "110" then
				sd_ce <= '1';
				if sd_read = '1' or sd_write = '1' then
					if sd_read = '1' then
						index <= conv_std_logic_vector(17, 6);
					elsif sd_write = '1' then
						index <= conv_std_logic_vector(24, 6);
					end if;
					argument <= sd_block & "000000000";
					state <= state + "100";
					sd_busy <= '1';	-- C: Set <- Busy
				else
					sd_busy <= '0';
				end if;
			elsif state(9 downto 7) = "111" then
				sd_ce <= '1';
				state(9 downto 7) <= state(9 downto 7) - "1";
				sd_busy <= '0';		-- C: Clear
			end if;
		end if;
	end process;


	ram_write_addr <= write_addr when we_a = '1' else sd_index;
	ram_write_data <= write_data when we_a = '1' else sd_write_data;
	ram_read_addr <= read_addr when sd_write_mode = '1' else sd_index;

	ram_write_enable <= we_a or sd_write_enable;

	sdmem: sd_memory
		port map(
			clk 	=> clk,
			addra	=> ram_write_addr,
			dina	=> ram_write_data,
			wea	=> ram_write_enable,

			addrb	=> ram_read_addr,
			doutb	=> ram_data
		);

	spic: spi_cont
		port map(
			clk	=> clk,

			spi_clk	=> sd_clk,
			spi_out	=> sd_out,
			spi_in	=> sd_in,

			spi_busy	=> spi_busy,
			spi_rdata	=> spi_rdata,
			spi_sdata	=> spi_sdata,
			spi_go		=> spi_go,
			spi_delay	=> spi_delay
		);
end Behavioral;
