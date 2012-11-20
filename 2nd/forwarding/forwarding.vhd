library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.all;

-- Forward data from previous EX stage to next EX stage.
-- From Patterson, Hennessy p.339

-- forward when write_enable and addrs are the same.
-- do not forward 0.

-- TEST
-- rs_addr | rs_data | rs_float | rt_addr | rt_data | rt_float | write_addr | write_data | write_enable | write_float | rs_forward | rt_forward
--       5 |      33 |        0 |       8 |      29 |        0 |          0 |         42 |            0 |           0 |         33 |         29
--       5 |         |          |       8 |         |          |          5 |            |            0 |             |         33 |         29
--       5 |         |          |       8 |         |          |          5 |            |            1 |             |         42 |         29
--       5 |         |          |       8 |         |          |          8 |            |            0 |             |         33 |         29
--       5 |         |          |       8 |         |          |          8 |            |            1 |             |         33 |         42
--       5 |      33 |        1 |       8 |      29 |        1 |          0 |         42 |            0 |           1 |         33 |         29
--       5 |         |          |       8 |         |          |          5 |            |            0 |             |         33 |         29
--       5 |         |          |       8 |         |          |          5 |            |            1 |             |         42 |         29
--       5 |         |          |       8 |         |          |          8 |            |            0 |             |         33 |         29
--       5 |         |          |       8 |         |          |          8 |            |            1 |             |         33 |         42
-- # float and general
--       5 |         |        0 |       8 |         |        0 |          5 |            |            1 |           1 |         33 |         29
--       5 |         |          |       8 |         |          |          8 |            |            1 |             |         33 |         29
-- # rs = rt
--       5 |         |        0 |       5 |      33 |        0 |          5 |            |            0 |           0 |         33 |         33
--       5 |         |          |       5 |      33 |          |          5 |            |            1 |             |         42 |         42
-- # when addr is 0
--       0 |       0 |          |       8 |      29 |          |          0 |            |            1 |             |          0 |         29
--       5 |      33 |          |       0 |       0 |          |          0 |            |            1 |             |         33 |         0
-- /TEST

entity forwarding is
  port (rs_addr      : in std_logic_vector(4 downto 0);
        rs_data      : in std_logic_vector(31 downto 0);
        rs_float     : in STD_LOGIC;
        rt_addr      : in std_logic_vector(4 downto 0);
        rt_data      : in std_logic_vector(31 downto 0);
        rt_float     : in STD_LOGIC;
        write_addr   : in std_logic_vector(4 downto 0);
        write_data   : in std_logic_vector(31 downto 0);
        write_enable : in STD_LOGIC;
        write_float  : in STD_LOGIC;
        rs_forward   : out std_logic_vector(31 downto 0);
        rt_forward   : out std_logic_vector(31 downto 0));
end forwarding;

architecture behave of forwarding is
begin
  process (rs_addr, rs_data, rs_float, write_addr, write_data, write_enable, write_float)
  begin
    if write_enable = '1' and rs_addr = write_addr and rs_float = write_float and rs_addr /= "00000" then
      rs_forward <= write_data;
    else
      rs_forward <= rs_data;
    end if;
  end process;

  process (rt_addr, rt_data, rt_float, write_addr, write_data, write_enable, write_float)
  begin
    if write_enable = '1' and rt_addr = write_addr and rt_float = write_float and rt_addr /= "00000" then
      rt_forward <= write_data;
    else
      rt_forward <= rt_data;
    end if;
  end process;
end behave;
