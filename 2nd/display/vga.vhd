-- from @h013
library ieee;

use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity vga is
  port(clk, reset : in std_logic;
       red, green, blue : in std_logic_vector(7 downto 0);
       r, g, b : out std_logic_vector(7 downto 0);
       hsync, vsync : out std_logic;
       row : out std_logic_vector(8 downto 0);
       column : out std_logic_vector(9 downto 0)
       );
end vga;

architecture synt of vga is
  signal videoon, videov, videoh : std_logic := '0';
  signal hcount, vcount : std_logic_vector(9 downto 0) := (others => '0');

  constant h_offset : integer := 0;
begin

  hcounter: process (clk, reset)
  begin
    if reset='1'
    then hcount <= (others => '0');
    else if (clk'event and clk='1')
         then if hcount = 799
              then hcount <= (others => '0');
              else hcount <= hcount + 1;
              end if;
         end if;
    end if;
  end process;

  process (hcount)
  begin
    videoh <= '1';

    if hcount > 639 then
      videoh <= '0';
    end if;

    if hcount >= (800 - h_offset) then
      column <= hcount - (800 - h_offset);
    elsif hcount < (640 - h_offset) then
      column <= hcount + h_offset;
    else
      -- this is to remove latches
      -- if something wrong, remove this
      column <= hcount;
    end if;
  end process;

  vcounter: process (clk, reset)
  begin
    if reset='1'
    then vcount <= (others => '0');
    else if (clk'event and clk='1')
         then if hcount = 699
              then if vcount = 524
                   then vcount <= (others => '0');
                   else vcount <= vcount + 1;
                   end if;
              end if;
         end if;
    end if;
  end process;

  process (vcount)
  begin
    videov <= '1';
      row <= vcount(8 downto 0);
    if vcount>479
    then videov <= '0';
         row <= (others => '0');
    end if;
  end process;

  sync: process (clk, reset)
  begin
    if reset='1'
    then hsync <= '0';
         vsync <= '0';
    else if (clk'event and clk='1')
         then if (hcount < 756 and hcount >= 659)
              then hsync <= '0';
              else hsync <= '1';
              end if;
              if (vcount < 495 and vcount >= 493)
              then vsync <= '0';
              else vsync <= '1';
              end if;
         end if;
    end if;
  end process;
  videoon <= videoh and videov;

  colors: process (clk, reset)
  begin
    if reset='1'
    then r <= "00000000";
         g <= "00000000";
         b <= "00000000";
    elsif (clk'event and clk = '1')
    then
      if videoon = '1' then
        r <= red;
        g <= green;
        b <= blue;
      else
        r <= "00000000";
        g <= "00000000";
        b <= "00000000";
      end if;
    end if;
  end process;
end synt;
