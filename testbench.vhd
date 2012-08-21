library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity testbench is
end testbench;

architecture test of testbench is

  component test_top
    port (
      CLK, XRST               : in     std_logic;
      write_data, data_addr   :slv;
      mem_write               : buffer std_logic);
  end component;

  signal write_data, data_addr : std_logic_vector(31 downto 0);
  signal clk, reset, mem_write : std_logic;

begin  -- test

  dut : top port map (
    clk        => clk,
    xreset     => not reset,
    write_data => write_data,
    data_addr  => data_addr,
    mem_write  => mem_write);

  clkgen: process
  begin  -- process clkgen
    clk <= '1';
    wait for 5 ns;
    clk <= '0';
    wait for 5 ns;
  end process clkgen;

  initialize: process
  begin  -- process initialize
    reset <= '1';
    wait for 22 ns;
    reset <= '0';
    wait;
  end process initialize;

  process (clk)
  begin
    if clk'event and clk = '0' and mem_write = '1' then
      if conv_integer(data_addr) = 84 and conv_integer(write_data) = 7 then
        report "Pass: [84] <= 7" severity note;
      elsif conv_integer(data_addr) = 80 and conv_integer(write_data) = 7 then
        report "Pass: [80] <= 7" severity note;
      else
        report "Unexpected memory write" severity failure;
      end if;
    end if;
  end;                
end test;
