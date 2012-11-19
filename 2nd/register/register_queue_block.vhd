library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Decode operator and judge which register file to use.

-- TEST
-- enable_left | addr_left | data_left | float_left | enable_up | addr_up | data_up | float_up | enable_down | addr_down | data_down | float_down
--           0 |        14 |       999 |          0 |         0 |       2 |     666 |        1 |           0 |         - |         - |          -
--           1 |        14 |       999 |          0 |         0 |       2 |     666 |        1 |           1 |        14 |       999 |          0
--           0 |        14 |       999 |          0 |         1 |       2 |     666 |        1 |           1 |         2 |       666 |          1
--           1 |        14 |       999 |          0 |         1 |       2 |     666 |        1 |           1 |        14 |       999 |          0
-- #
--           0 |        14 |       999 |          1 |         0 |       2 |     666 |        0 |           0 |         - |         - |          -
--           1 |        14 |       999 |          1 |         0 |       2 |     666 |        0 |           1 |        14 |       999 |          1
--           0 |        14 |       999 |          1 |         1 |       2 |     666 |        0 |           1 |         2 |       666 |          0
--           1 |        14 |       999 |          1 |         1 |       2 |     666 |        0 |           1 |        14 |       999 |          1
-- /TEST


entity register_queue_block is
  port (
    clk : in STD_LOGIC;

    enable_left : in STD_LOGIC;
    addr_left   : in std_logic_vector(4 downto 0);
    data_left   : in std_logic_vector(31 downto 0);
    float_left  : in STD_LOGIC;

    enable_up : in STD_LOGIC;
    addr_up   : in std_logic_vector(4 downto 0);
    data_up   : in std_logic_vector(31 downto 0);
    float_up  : in STD_LOGIC;

    enable_down : out STD_LOGIC;
    addr_down   : out std_logic_vector(4 downto 0);
    data_down   : out std_logic_vector(31 downto 0);
    float_down  : out STD_LOGIC);
end register_queue_block;

architecture behave of register_queue_block is

begin -- behave

  process (clk,
           enable_left, addr_left, data_left, float_left,
           enable_up, addr_up, data_up, float_up)
  begin
    if rising_edge(clk) then
      if enable_left = '1' then
        enable_down <= enable_left;
        addr_down   <= addr_left;
        data_down   <= data_left;
        float_down  <= float_left;
      else
        enable_down <= enable_up;
        addr_down   <= addr_up;
        data_down   <= data_up;
        float_down  <= float_up;
      end if;
    end if;
  end process;
    
end behave;
