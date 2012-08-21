library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity register_file is
  
  port (
    clk                                 : in  std_logic;
    write_enable3                       : in  std_logic;
    read_addr1, read_addr2, write_addr3 : in  std_logic_vector(4 downto 0);
    write_data3                         : in  std_logic_vector(31 downto 0);
    read_data1, read_data2              : out std_logic_vector(31 downto 0));

end register_file;

architecture behave of register_file is

  type ram_type is array (31 downto 0) of std_logic_vector(31 downto 0);
  signal mem : ram_type;
  
begin  -- behave

  process(clk)
  begin
    if clk'event and clk = '1' then
      if write_enable3 = '1' then
        mem(conv_integer(write_addr3)) <= write_data3;
      end if;
    end if;
  end process;

  process (read_addr1, read_addr2)
  begin
    if conv_integer(read_addr1) = 0 then
      read_data1 <=  x"00000000";
    else
      read_data1 <= mem(conv_integer(read_addr1));
    end if;

    if conv_integer(read_addr2) = 0 then
      read_data2 <=  x"00000000";
    else
      read_data2 <= mem(conv_integer(read_addr2));
    end if;
  end;

end behave;
