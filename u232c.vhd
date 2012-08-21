library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity u232c is
  generic (wtime: std_logic_vector(15 downto 0) := x"008F");
  Port ( clk  : in  STD_LOGIC;
         data : in  STD_LOGIC_VECTOR (7 downto 0);
         go   : in  STD_LOGIC;
         busy : out STD_LOGIC;
         tx   : out STD_LOGIC);
end u232c;

architecture blackbox of u232c is
  signal countdown: std_logic_vector(15 downto 0) := (others=>'0');
  signal sendbuf: std_logic_vector(8 downto 0) := (others=>'1');
  signal state: std_logic_vector(5 downto 0) := "000000";
  signal prev_state : std_logic_vector(5 downto 0) := "000000";
begin
  counter: process(clk)
  begin
    if rising_edge(clk) then
      if state = prev_state then
        countdown <= countdown - 1;
      else
        prev_state <= state;
        countdown <= wtime;
      end if;
    end if;
  end process;

  statemachine: process(clk)
  begin
    if rising_edge(clk) then
      if state(5)='0' and state(0)='0' and go = '1' then
        sendbuf<=data&"0";
        state(0) <= '1';
      elsif state(5) = '1' then
        state(0) <= '0';
      end if;

      if countdown = 0 then
        sendbuf<="1"&sendbuf(8 downto 1);
        state(5 downto 1) <= state(4 downto 0);
      end if;
    end if;
  end process;
  tx<=sendbuf(0);
  busy<= '0' when state(5)='0' and state(0)='0' else '1';
end blackbox;

