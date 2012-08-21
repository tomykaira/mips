library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity dmem is
  
  port (
    clk, we : in  std_logic;
    a, wd   : in  std_logic_vector(31 downto 0);
    rd      : out std_logic_vector(31 downto 0));

end dmem;

-- why??
-- is this work on the FPGA?

architecture behave of dmem is

begin  -- behave

  process is
    type ram_type is array ((63 donwto 0)) of std_logic_vector(31 downto 0);
    variable mem : ram_type;
  begin

    loop
      if clk'event and clk = '1' then
        if we = '1' then
          mem(cov_integer(a(7 downto 2))) := wd;
        end if;
      end if;

      rd <= mem(conv_integer(a(7 downto 2)));

      wait o clk, a;
    end loop;
  end process;

end behave;
