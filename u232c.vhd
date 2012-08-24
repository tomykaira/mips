library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity u232c is
  generic (wtime: std_logic_vector(15 downto 0) := x"008F";
           len: integer range 1 to 8 := 4);
  Port ( clk  : in  STD_LOGIC;
         data : in  STD_LOGIC_VECTOR (len*8-1 downto 0);
         go   : in  STD_LOGIC;
         busy : out STD_LOGIC;
         tx   : out STD_LOGIC);
end u232c;

architecture blackbox of u232c is
  signal countdown: std_logic_vector(15 downto 0) := (others=>'0');
  signal sendbuf: std_logic_vector(8 downto 0) := (others=>'1');
  signal databuf : std_logic_vector(len*8-1 downto 0) := (others => '1');
  signal state: std_logic_vector(5 downto 0) := "000000";
  signal prev_state : std_logic_vector(5 downto 0) := "000000";

  signal inner_go : std_logic := '0';
  signal inner_busy : std_logic := '0';
  signal chunk : std_logic_vector(7 downto 0) := (others => '1');
  signal ptr : integer range 0 to len := 0;
begin

  -- purpose: push 8 bits from databuf to sendbuf
  -- inputs : clk, go
  -- outputs: sendbuf
  push_buf: process (clk, go)
  begin  -- process push_buf
    if rising_edge(clk) then
      if go = '1' and ptr = 0 then
        databuf <= data;
        ptr <= 1;
      end if;

      if inner_go = '0' and inner_busy = '0' and ptr /= 0 then
        inner_go <= '1';
        chunk <= databuf(len*8-1 downto len*8-8);
        if len = 1 then
          databuf <= x"ff";
        else
          databuf <= databuf(len*8-9 downto 0)&x"ff";
        end if;
        if ptr < len then
          ptr <= ptr + 1;
        else
          ptr <= 0;
        end if;
      else
        inner_go <= '0';
      end if;
    end if;
  end process push_buf;

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
      if state(5)='0' and state(0)='0' and inner_go = '1' then
        sendbuf<=chunk&"0";
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
  inner_busy<= '0' when state(5)='0' and state(0)='0' else '1';
  busy<= '0' when (ptr = 0) else '1';
end blackbox;
