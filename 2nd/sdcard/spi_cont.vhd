library IEEE;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_1164.ALL;

entity spi_cont is
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
end spi_cont;

architecture Behavioral of spi_cont is
	signal delay	: std_logic_vector(7 downto 0);
	signal rbuf	: std_logic_vector(7 downto 0);
	signal sbuf	: std_logic_vector(7 downto 0);
	signal state	: std_logic_vector(4 downto 0);
begin

	process(clk)begin
		if rising_edge(clk)then
			if state = "00000" then
				spi_clk <= '0';
				spi_out <= '1';
				sbuf  <= spi_sdata;
				spi_rdata <= rbuf;
				delay <= spi_delay;
				if spi_go = '1' then
					spi_busy <= '1';
					state <= state + '1';
				else
					spi_busy <= '0';
				end if;
			elsif state(0) = '0' then
				spi_busy <= '1';
				if delay = 0 then
					spi_clk <= '1';
					state <= state + '1';
					delay <= spi_delay;
				else
					delay <= delay - '1';
				end if;
			elsif state = "10001" then
				spi_busy <= '1';
				if delay = 0 then
					spi_clk <= '0';
					spi_out <= '1';
					rbuf <= rbuf(6 downto 0) & spi_in;
					state <= "00000";
					delay <= spi_delay;
				else
					delay <= delay - '1';
				end if;
			else
				spi_busy <= '1';
				if delay = 0 then
					spi_clk <= '0';
					spi_out <= sbuf(7);
					sbuf  <= sbuf(6 downto 0) & '0';
					rbuf  <= rbuf(6 downto 0) & spi_in;
					state <= state + '1';
					delay <= spi_delay;
				else
					delay <= delay - '1';
				end if;
			end if;
		end if;
	end process;
end Behavioral;
