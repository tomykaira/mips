library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity controller is
  port (op                    : in STD_LOGIC_VECTOR(5 downto 0);
        zero                  : in STD_LOGIC;
        rx_done               : in STD_LOGIC;
        bus_to_reg, mem_write : out STD_LOGIC;
        pc_src, alu_src       : out STD_LOGIC;
        reg_dst, reg_write    : out STD_LOGIC;
        jump                  : out STD_LOGIC;
        rx_enable             : out STD_LOGIC;
        send_enable           : out STD_LOGIC;
        alu_control           : out STD_LOGIC_VECTOR(2 downto 0));
end;

architecture struct of controller is
  component main_decoder
    port (
      op                    : in  STD_LOGIC_VECTOR(5 downto 0);
      rx_done               : in  STD_LOGIC;
      bus_to_reg, mem_write : out STD_LOGIC;
      alu_src               : out STD_LOGIC;
      branch                : out STD_LOGIC;
      reg_dst, reg_write    : out STD_LOGIC;
      jump                  : out STD_LOGIC;
      rx_enable             : out STD_LOGIC;
      send_enable           : out STD_LOGIC;
      alu_control           : out STD_LOGIC_VECTOR(2 downto 0));
  end component;

  signal branch : STD_LOGIC;

begin
  md : main_decoder port map (
    op          => op,
    rx_done     => rx_done,
    bus_to_reg  => bus_to_reg,
    mem_write   => mem_write,
    branch      => branch,
    alu_src     => alu_src,
    reg_dst     => reg_dst,
    reg_write   => reg_write,
    jump        => jump,
    rx_enable   => rx_enable,
    send_enable => send_enable,
    alu_control => alu_control);

  pc_src <= branch and zero;
end;
