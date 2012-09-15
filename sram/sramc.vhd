library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_MISC.ALL;

-- not yet tested at all
-- Constant lines should be trimmed on top entity.
--  XZBE<= "0000";
--  XE1 <= '0';
--  E2A <= '1';
--  XE3 <= '0';
--  XGA <= '0';
--  XZCKE <= '0';
--  ZCLKMA(0) <= clk;
--  ZCLKMA(1) <= clk;
--  ADVA <= '0';
--  XFT <= '1';
--  XLBO <= '1';
--  ZZA <= '0';
--  ZDP <=  (others => 'Z');

entity sramc is
  Port (
    ZD           : inout std_logic_vector(31 downto 0);
    ZDP          : inout std_logic_vector(3 downto 0);
    ZA           : out std_logic_vector(19 downto 0);
    XWA          : out std_logic;

    data_read    : out std_logic_vector(31 downto 0);
    data_write   : in std_logic_vector(31 downto 0);
    address      : in std_logic_vector(19 downto 0);
    write_enable : in std_logic;
    clk : in std_logic
    );
end sramc;

architecture blackbox of sramc is
begin  -- blackbox

  ZA   <= address;
  ZDP <= (others => 'Z');

  -- data_read is not refreshed in simulation
  -- workaround: forcefully update it with clock timing
  -- TODO: FIXME: I am not sure it works on FPGA
  process (address, data_write, write_enable, ZD, clk)
  begin

    if write_enable = '1' then
      XWA  <= '0';
      ZD <= data_write;
      data_read <= (others => 'Z');
    else
      XWA  <= '1';
      ZD <= (others => 'Z');
      data_read <= ZD;
    end if;
  end process;

end blackbox;
