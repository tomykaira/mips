library IEEE;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_1164.ALL;

entity sd_cont is
	port(
		clk	: in  std_logic;

		sd_clk	: out std_logic;
		sd_ce	: out std_logic;
		sd_out	: out std_logic;
		sd_in	: in  std_logic;

		sd_data	: out std_logic_vector(7 downto 0);
		sd_addr	: in  std_logic_vector(22 downto 0);
		sd_index: in  std_logic_vector(8 downto 0);
		sd_go	: in  std_logic;
		sd_flag	: out std_logic_vector(3 downto 0) := "0001"
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
	component SDMemory
		port(
			clka	: in  std_logic;
			addra	: in  std_logic_vector(8 downto 0);
			dina	: in  std_logic_vector(7 downto 0);
			wea	: in  std_logic_vector(0 downto 0);
			clkb	: in  std_logic;
			addrb	: in  std_logic_vector(8 downto 0);
			doutb	: out std_logic_vector(7 downto 0)
		);
	end component;

	signal spi_rdata	: std_logic_vector(7 downto 0);
	signal spi_sdata	: std_logic_vector(7 downto 0);
	signal spi_busy		: std_logic;
	signal spi_go		: std_logic;
	signal spi_delay	: std_logic_vector(7 downto 0);

	signal state	: std_logic_vector(8 downto 0) := "000000000";
	signal index	: std_logic_vector(5 downto 0);
	signal argument	: std_logic_vector(31 downto 0);
	signal resp	: std_logic_vector(7 downto 0);

	signal addr_a	: std_logic_vector(8 downto 0);
	signal data_a	: std_logic_vector(7 downto 0);
	signal we_a	: std_logic_vector(0 downto 0);
	signal data_b	: std_logic_vector(7 downto 0);
begin

	process(clk)begin
		if rising_edge(clk)then
			sd_data <= data_b;

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
			elsif state(8 downto 6) = "000" then		-- CLK 8 * 16
				spi_sdata <= "11111111";
				sd_ce <= '1';
				spi_delay <= "11111111";
				state <= state + '1';
				sd_flag <= "0001";
			--------------------------------------------------------------------------
			elsif state(5 downto 2) = "0001" then		-- Command Index
				spi_sdata <= "01" & index;
				sd_ce     <= '0';
				state <= state + '1';
			elsif state(5 downto 2) = "0010" then		-- Argument 1
				spi_sdata <= argument(31 downto 24);
				state <= state + '1';
			elsif state(5 downto 2) = "0011" then		-- Argument 2
				spi_sdata <= argument(23 downto 16);
				state <= state + '1';
			elsif state(5 downto 2) = "0100" then		-- Argument 3
				spi_sdata <= argument(15 downto 8);
				state <= state + '1';
			elsif state(5 downto 2) = "0101" then		-- Argument 4
				spi_sdata <= argument(7  downto 0);
				state <= state + '1';
			elsif state(5 downto 2) = "0110" then		-- CRC
				spi_sdata <= "10010101";
				state <= state + '1';
			elsif state(5 downto 2) = "0111" then		-- Response
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(5 downto 2) = "1000" then		-- Response Check
				if spi_rdata = "11111111" then	-- Not received
					state <= state - "100";
				else				-- Received
					state <= state + "100";
					resp  <= spi_rdata;
				end if;
			elsif state(5 downto 2) = "1001" then		-- Finish or Continue(Cmd17)
				if index = 17 then	-- CMD17
					state <= state + "100";
				else			-- Finish
					state(5 downto 2) <= "0000";
					state(8 downto 6) <= state(8 downto 6) + '1';
					sd_ce <= '1';
				end if;
			elsif state(5 downto 2) = "1010" then		-- Wait Data Token
				spi_sdata <= "11111111";
				state <= state + '1';
			elsif state(5 downto 2) = "1011" then		-- Check if rdata is Data Token
				if spi_rdata = "11111110" then
					state <= state + "100";
				else
					state <= state - "100";
				end if;
				addr_a <= "111111111";
			elsif state(5 downto 2) = "1100" then		-- Receive Data
				spi_sdata <= "11111111";
				we_a(0)   <= '0';
				state <= state + '1';
			elsif state(5 downto 2) = "1101" then		-- Loop
				addr_a  <= addr_a + '1';
				we_a(0) <= '1';
				data_a  <= spi_rdata;
				if addr_a = "111111110" then
					state <= state + "100";
				else
					state <= state - "100";
				end if;
			elsif state(5 downto 2) = "1110" then		-- CRC1
				spi_sdata <= "11111111";
				we_a(0)   <= '0';
				state <= state + '1';
			elsif state(5 downto 2) = "1111" then		-- CRC2 and Finish
				spi_sdata <= "11111111";
				state <= state + '1';
			--------------------------------------------------------------------------------------
			---- Initialization Sequence ----
			elsif state(8 downto 6) = "001" then		-- CMD 0
				index <= "000000";
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(8 downto 6) = "010" then		-- CMD 55
				index <= conv_std_logic_vector(55, 6);
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(8 downto 6) = "011" then		-- CMD 41
				index <= conv_std_logic_vector(41, 6);
				argument <= (others => '0');
				state <= state + "100";
				sd_ce <= '1';
			elsif state(8 downto 6) = "100" then		-- Response Check
				if resp = "00000000" then
					state <= state + "1000000";
				else
					state <= state - "10000000";
				end if;
			elsif state(8 downto 6) = "101" then		-- CMD 16
				spi_delay <= "00000101";
				index    <= conv_std_logic_vector(16, 6);
				argument <= conv_std_logic_vector(512, 32);
				state <= state + "100";
				sd_ce <= '1';
			-----------------------------------------------------------------------------------------
			---- Loop ----
			elsif state(8 downto 6) = "110" then
				sd_ce <= '1';
				if sd_go = '1' then
					index <= conv_std_logic_vector(17, 6);
					argument <= sd_addr & "000000000";
					state <= state + "100";
					sd_flag <= "0001";	-- C: Set <- Busy
				else
					sd_flag <= "0000";
				end if;
			elsif state(8 downto 6) = "111" then
				sd_ce <= '1';
				state <= state - "1000000";
				sd_flag <= "0000";		-- C: Clear
			end if;
		end if;
	end process;

	sdmem: SDMemory
		port map(
			clka	=> clk,
			addra	=> addr_a,
			dina	=> data_a,
			wea	=> we_a,

			clkb	=> clk,
			addrb	=> sd_index,
			doutb	=> data_b
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
