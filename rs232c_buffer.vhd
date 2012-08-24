library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity rs232c_buffer is
  
  generic (
    wtime : std_logic_vector(15 downto 0) := x"008F");

  port (
    clk       : in std_logic;
    push      : in std_logic;           -- 1 to push data
    push_data : in std_logic_vector(31 downto 0);
    tx        : out std_logic);

end rs232c_buffer;

architecture behave of rs232c_buffer is

  component u232c
    port (
      clk  : in  STD_LOGIC;
      data : in  STD_LOGIC_VECTOR (7 downto 0);
      go   : in  STD_LOGIC;
      busy : out STD_LOGIC;
      tx   : out STD_LOGIC);
  end component;

  -- Capacity : 32 / 4 = 8
  type ram_type is array (31 downto 0) of std_logic_vector(7 downto 0);
  signal queue : ram_type;

  signal read_ptr, write_ptr : std_logic_vector(5 downto 0) := (others => '0');
  signal go, busy : std_logic;

begin  -- behave

  sender : u232c port map (
    clk  => clk,
    data => queue(conv_integer(read_ptr)),
    go   => go,
    busy => busy,
    tx   => tx);

  push_to_queue: process (clk)
  begin  -- process push
    if clk'event and clk = '1' then  -- rising clock edge
      if push = '1' then
        queue(conv_integer(write_ptr))     <= push_data(31 downto 24);
        queue(conv_integer(write_ptr) + 1) <= push_data(23 downto 16);
        queue(conv_integer(write_ptr) + 2) <= push_data(15 downto 8);
        queue(conv_integer(write_ptr) + 3) <= push_data(7  downto 0);
        write_ptr <= write_ptr + 4;
      end if;
    end if;
  end process push_to_queue;

  pop: process (clk)
  begin  -- process pop
    if clk'event and clk = '1' then  -- rising clock edge
      if busy = '0' and read_ptr /= write_ptr then
        go <= '1';
      end if;
    end if;
  end process pop;

  load_next: process (busy)
  begin  -- process load_next
    -- falling edge
    if busy'event and busy = '0' and read_ptr /= write_ptr then
      read_ptr <= read_ptr + 1;
    end if;
  end process load_next;
  
end behave;
