-- 115 Hz

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_MISC.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity fadd is
  Port (
    clk: in  STD_LOGIC;
    i1 : in  STD_LOGIC_VECTOR (31 downto 0);
    i2 : in  STD_LOGIC_VECTOR (31 downto 0);
    o  : out STD_LOGIC_VECTOR (31 downto 0)
    );
end fadd;

architecture blackbox of fadd is
  subtype float is std_logic_vector(31 downto 0);  -- type for float
  subtype calf is std_logic_vector(26 downto 0);  -- type for float
  signal a : float := (others => '0');
  signal b : float := (others => '0');
  signal am : calf := (others => '0');
  signal bm : calf := (others => '0');
  signal expdiff : integer range 0 to 255;
  signal sumexp1, sumexp : integer range 0 to 255 := 127;   -- exponent of sum
  signal op : std_logic := '0';         -- 0: add 1: subtract
  signal op2 : std_logic := '0';        -- op in step 2
  signal msb : integer range 0 to 26 := 25;
  signal sm : calf;
  signal exceptional, exceptional2 : std_logic := '0';
  signal special_buffer, special_buffer2 : float;
  signal sign : std_logic;
  signal rbm : std_logic;
begin

  -- Step. 1
  -- set a and b, then expdiff se am bm
  calc_step1: process(clk, i1, i2)
    variable vop: std_logic;
    variable va, vb :float;
    variable vexpdiff : integer range 0 to 255;
    variable vam, vbm :calf;
  begin
    if rising_edge(clk) then
      if i1(31) /= i2(31) then
        vop := '1';
      else
        vop := '0';
      end if;

      op <= vop;

      if i1(30 downto 0) >= i2(30 downto 0) then
        va := i1;
        vb := i2;
      else
        vb := i1;
        va := i2;
      end if;

      a <= va;
      b <= vb;

      if (or_reduce(va(30 downto 23)) = '0' and or_reduce(vb(30 downto 23)) = '0') then
        vexpdiff := 0;
      elsif (or_reduce(va(30 downto 23)) = '0') then
        vexpdiff := 1 - conv_integer(vb(30 downto 23));
      elsif (or_reduce(vb(30 downto 23)) = '0') then
        vexpdiff := conv_integer(va(30 downto 23)) - 1;
      else
        vexpdiff := conv_integer(va(30 downto 23)) - conv_integer(vb(30 downto 23));
      end if;

      expdiff <= vexpdiff;

      if or_reduce(va(30 downto 23)) = '0' then
        sumexp1 <= 1;
      else
        sumexp1 <= conv_integer(va(30 downto 23));
      end if;

      if va = 0 and vb = x"80000000" then
        exceptional <= '1';
        special_buffer <= (others => '0');
      elsif (va(30 downto 23) = x"ff" and or_reduce(va(22 downto 0)) = '1') or vb = 0 or vb = x"80000000" or vexpdiff > 24 then
        exceptional <= '1';
        special_buffer <= va;
      elsif (vb(30 downto 23) = x"ff" and or_reduce(vb(22 downto 0)) = '1') or va = 0 or va = x"80000000" then
        exceptional <= '1';
        special_buffer <= vb;
      elsif (va = x"7f800000" and vb = x"ff800000") or (vb = x"7f800000" and va = x"ff800000") then
        exceptional <= '1';
        special_buffer <= x"ffc00000";
      else
        exceptional <= '0';
      end if;
    
      if vop = '0' and or_reduce(va(30 downto 23)) /= '0' then
        vam := "01"&va(22 downto 0)&"00";
      elsif vop = '0' and or_reduce(va(30 downto 23)) = '0' then
        vam := "00"&va(22 downto 0)&"00";
      elsif vop = '1' and or_reduce(va(30 downto 23)) /= '0' then
        vam := "1"&va(22 downto 0)&"000";
      else
        vam := "0"&va(22 downto 0)&"000";
      end if;

      if vop = '0' and or_reduce(vb(30 downto 23)) /= '0' then
        vbm := "01"&vb(22 downto 0)&"00";
      elsif vop = '0' and or_reduce(vb(30 downto 23)) = '0' then
        vbm := "00"&vb(22 downto 0)&"00";
      elsif vop = '1' and or_reduce(vb(30 downto 23)) /= '0' then
        vbm := "1"&vb(22 downto 0)&"000";
      else
        vbm := "0"&vb(22 downto 0)&"000";
      end if;

      am <= vam;
      bm <= vbm;

      if vexpdiff < 26 then
        rbm <= or_reduce(vbm(vexpdiff downto 0));
      end if;
    end if;
  end process;


  -- Step. 2
  -- using a, b, ..., set sm and msb
  -- on exceptional cases, set exceptional flag
  calc_step2: process(clk, a, b, op, expdiff, am, bm, sumexp1, exceptional)
    variable bmm : calf := (others => '0');
    variable vsm : calf := (others => '0'); -- mag of sum
  begin
    if rising_edge(clk) then

      sumexp <= sumexp1;
      sign <= a(31);
      op2 <= op;
      exceptional2 <= exceptional;
      special_buffer2 <= special_buffer;

      if exceptional = '0' then
        if op = '0' then
          bmm := (others => '0');
          bmm(25-expdiff downto 0) := bm(25 downto 1+expdiff) & rbm;
          vsm := conv_std_logic_vector(conv_integer(am)+conv_integer(bmm), 27);
          if vsm(26) = '1' then
            if (vsm(2) = '1') and (vsm(3) = '1' or vsm(0) = '1' or vsm(1) = '1') then
              vsm(26 downto 2) := vsm(26 downto 2) + 1;
            end if;
            msb <= 26;
          elsif (vsm(1) = '1') and (vsm(2) = '1' or vsm(0) = '1') then
            vsm(26 downto 1) := vsm(26 downto 1) + 1;
            if vsm(26) = '1' then
              msb <= 26;
            else
              msb <= 25;
            end if;
          else
            msb <= 25;
          end if;

          sm <= vsm;
        else
          bmm := (others => '0');
          bmm(26-expdiff downto 0) := bm(26 downto 1+expdiff) & rbm;
          vsm := conv_std_logic_vector(conv_integer(am)-conv_integer(bmm), 27);
          if vsm(26) = '0' then
            if (vsm(1) = '1') and (vsm(2) = '1' or vsm(0) = '1') then
              vsm(25 downto 1) := vsm(25 downto 1) + 1;
            end if;
          elsif (vsm(2) = '1') and (vsm(3) = '1' or vsm(1) = '1' or vsm(0) = '1') then
            vsm(26 downto 2) := vsm(26 downto 2) + 1;
          end if;

          sm <= vsm;
          if vsm(26) = '1' then
            msb <= 26;
          elsif vsm(25) = '1' then
            msb <= 25;
          elsif vsm(24) = '1' then
            msb <= 24;
          elsif vsm(23) = '1' then
            msb <= 23;
          elsif vsm(22) = '1' then
            msb <= 22;
          elsif vsm(21) = '1' then
            msb <= 21;
          elsif vsm(20) = '1' then
            msb <= 20;
          elsif vsm(19) = '1' then
            msb <= 19;
          elsif vsm(18) = '1' then
            msb <= 18;
          elsif vsm(17) = '1' then
            msb <= 17;
          elsif vsm(16) = '1' then
            msb <= 16;
          elsif vsm(15) = '1' then
            msb <= 15;
          elsif vsm(14) = '1' then
            msb <= 14;
          elsif vsm(13) = '1' then
            msb <= 13;
          elsif vsm(12) = '1' then
            msb <= 12;
          elsif vsm(11) = '1' then
            msb <= 11;
          elsif vsm(10) = '1' then
            msb <= 10;
          elsif vsm(9) = '1' then
            msb <= 9;
          elsif vsm(8) = '1' then
            msb <= 8;
          elsif vsm(7) = '1' then
            msb <= 7;
          elsif vsm(6) = '1' then
            msb <= 6;
          elsif vsm(5) = '1' then
            msb <= 5;
          elsif vsm(4) = '1' then
            msb <= 4;
          elsif vsm(3) = '1' then
            msb <= 3;
          elsif vsm(2) = '1' then
            msb <= 2;
          elsif vsm(1) = '1' then
            msb <= 1;
          else
            msb <= 0;
          end if;
        end if;
      end if;
    end if;
  end process;

  -- Step. 3 output
  calc_step3: process(clk, msb, sm, exceptional2, special_buffer2, sumexp)
    variable vsm : calf := (others => '0'); -- mag of sum
  begin
    if rising_edge(clk) then
      if exceptional2 = '1' then
        o <= special_buffer2;
      elsif op2 = '0' then
        if sumexp + msb - 25 >= 255 then
          o <= (31=>sign, 30 downto 23 => '1', others => '0');
        elsif sm(msb) = '0' then       -- it was denormal
          o <= sign & x"00" & sm(24 downto 2);
        else
          o <= sign & conv_std_logic_vector(sumexp + msb - 25, 8) & sm(msb-1 downto msb-23);
        end if;
      else
        if msb = 0 then
          o <= x"00000000";
        elsif sumexp - 26 + msb <= 0 then
          vsm(25 downto sumexp) := sm(26-sumexp downto 1);
          vsm(sumexp-1 downto 0) := (others => '0');
          o <= sign & x"00" & vsm(25 downto 3);
        else
          vsm(26 downto 27-msb) := sm(msb downto 1);
          vsm(26-msb downto 0) := (others => '0');
          o <= sign & conv_std_logic_vector(sumexp - 26 + msb, 8) & vsm(25 downto 3);
        end if;
      end if;
    end if;
  end process;
end blackbox;

