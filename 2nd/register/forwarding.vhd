library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.all;

-- Forward data from previous EX stage to next EX stage.
-- From Patterson, Hennessy p.339

-- forward when write_enable and addrs are the same.
-- do not forward 0.

-- TEST
-- addr | data | float | write_addr | write_data | write_enable | write_float | forward_data
--    5 |   33 |     0 |          0 |         42 |            0 |           0 |         33
--      |      |       |          5 |            |            0 |             |         33
--      |      |       |          5 |            |            1 |             |         42
--      |      |     1 |          0 |            |            0 |           1 |         33
--      |      |       |          5 |            |            0 |             |         33
--      |      |       |          5 |            |            1 |             |         42
-- # float and general
--      |      |     0 |          5 |            |            1 |           1 |         33
-- # when addr is 0
--    0 |      |     0 |          0 |            |            1 |           0 |         33
--    0 |      |     1 |          0 |            |            1 |           1 |         33
-- /TEST

entity forwarding is
  port (addr         : in std_logic_vector(4 downto 0);
        data         : in std_logic_vector(31 downto 0);
        float        : in STD_LOGIC;
        write_addr   : in std_logic_vector(4 downto 0);
        write_data   : in std_logic_vector(31 downto 0);
        write_enable : in STD_LOGIC;
        write_float  : in STD_LOGIC;
        forward_data : out std_logic_vector(31 downto 0));
end forwarding;

architecture behave of forwarding is
begin
  process (addr, data, float, write_addr, write_data, write_enable, write_float)
  begin
    if write_enable = '1' and addr = write_addr and float = write_float and addr /= "00000" then
      forward_data <= write_data;
    else
      forward_data <= data;
    end if;
  end process;
end behave;
