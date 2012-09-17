library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity i232c is
  -- TODO: 最後の 1 のビットの前に countdown が終ってしまうことがあるので
  -- wtime を長めにとって対処した。ステートマシンを整えることで対処すべき
  generic (wtime: std_logic_vector(15 downto 0) := x"0095");
  Port ( clk    : in  STD_LOGIC;
         enable : in  STD_LOGIC;
         rx     : in  STD_LOGIC;
         data   : out STD_LOGIC_VECTOR (7 downto 0);
         changed: out STD_LOGIC);
end i232c;

-- states
-- 543210
-- 000000 : waiting
-- 000001 : first 0 comes
-- 000011 : (first) push in the middle of cnt
-- 000111 : push in the middle of cnt
-- 001111 : push in the middle of cnt
-- 011111 : push in the middle of cnt
-- 111111 : skipped
-- 111110 : push in the middle of cnt
-- 111100 : push in the middle of cnt
-- 111000 : push in the middle of cnt
-- 110000 : push in the middle of cnt
-- 100000 : set data, change = 1

architecture blackbox of i232c is
  signal cnt     : std_logic_vector(15 downto 0) := (others=>'1');
  signal state   : std_logic_vector(5 downto 0) := "000000";
  signal rxdfd   : std_logic := '1';      -- inner RXD
  signal fd      : std_logic_vector(7 downto 0) := (others=>'1');
begin
  -- 入力をクロックと同期
  get_input: process(clk)
  begin
    if rising_edge(clk) then
      rxdfd <= rx;
    end if;
  end process;

  write_out: process(clk)
  begin
    if rising_edge(clk) then
      if state(5)='1' and state(4)='0' and enable = '1' then
        data <= fd;
        changed <= '1';
      else
        changed <= '0';
      end if;
    end if;
  end process;

  statemachine: process(clk)
  begin
    if rising_edge(clk) then
      if enable = '0' then
        state <= "000000"; -- reset
      elsif state(5) = '0' and rxdfd = '0' then
        state(0) <= '1';
      elsif state(5) = '1' then
        state(0) <= '0';
      end if;

      if cnt=0 then
        state(5 downto 1) <= state(4 downto 0);
      end if;
    end if;
  end process;

  cntdown: process(clk)
  begin
    if rising_edge(clk) then
      if state = "000000" then
        cnt <= wtime;
      elsif enable = '1' then
        if cnt = 0 then
          cnt <= wtime;
        else
          cnt <= cnt-1;
        end if;
      end if;
    end if;
  end process;

  serial_to_parallel: process(clk)
  begin
    if rising_edge(clk) then
      if state(1) = '1' or state(4) = '1' then
        if enable = '1' and cnt(14 downto 0) = wtime(15 downto 1) then
          fd(7) <= rxdfd;
          fd(6 downto 0) <= fd (7 downto 1);
        end if;
      end if;
    end if;
  end process;
end blackbox;
