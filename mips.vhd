library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity mips is

  port (
    clk, reset          : in  STD_LOGIC;
    pc                  : out STD_LOGIC_VECTOR(31 downto 0);
    instruction         : in  STD_LOGIC_VECTOR(31 downto 0);
    mem_write           : out STD_LOGIC;
    alu_out, write_data : out STD_LOGIC_VECTOR(31 downto 0);
    read_data           : in  STD_LOGIC_VECTOR(31 downto 0);
    rx_enable           : out STD_LOGIC;
    rx_done             : in  STD_LOGIC);

end;

architecture struct of mips is

  component controller
    port (op                  : in STD_LOGIC_VECTOR(5 downto 0);
        zero                  : in STD_LOGIC;
        rx_done               : in STD_LOGIC;
        bus_to_reg, mem_write : out STD_LOGIC;
        pc_src, alu_src       : out STD_LOGIC;
        reg_dst, reg_write    : out STD_LOGIC;
        jump                  : out STD_LOGIC;
        rx_enable             : out STD_LOGIC;
        alu_control           : out STD_LOGIC_VECTOR(2 downto 0));
  end component;

  component data_path
    port (
    clk, reset          : in  std_logic;
    bus_to_reg, pc_src  : in  std_logic;
    alu_src, reg_dst    : in  std_logic;
    reg_write, jump     : in  std_logic;
    alu_control         : in  std_logic_vector(2 downto 0);
    zero                : out std_logic;
    pc                  : out std_logic_vector(31 downto 0);
    instruction         : in  std_logic_vector(31 downto 0);
    alu_out, write_data : out std_logic_vector(31 downto 0);
    read_data           : in  std_logic_vector(31 downto 0);
    stall               : in  STD_LOGIC);
  end component;

  signal bus_to_reg,alu_src,reg_dst,reg_write,jump,pc_src : STD_LOGIC;
  signal zero : std_logic;
  signal alu_control : std_logic_vector(2 downto 0);
  signal rx_enable_buf : STD_LOGIC;

begin
  cont : controller port map (
    op          => instruction(31 downto 26),
    zero        => zero,
    rx_done     => rx_done,
    bus_to_reg  => bus_to_reg,
    mem_write   => mem_write,
    pc_src      => pc_src,
    alu_src     => alu_src,
    reg_dst     => reg_dst,
    reg_write   => reg_write,
    jump        => jump,
    rx_enable   => rx_enable_buf,
    alu_control => alu_control);

  dp : data_path port map (
    clk         => clk,
    reset       => reset,
    bus_to_reg  => bus_to_reg,
    pc_src      => pc_src,
    alu_src     => alu_src,
    reg_dst     => reg_dst,
    reg_write   => reg_write,
    jump        => jump,
    alu_control => alu_control,
    zero        => zero,
    pc          => pc,
    instruction => instruction,
    alu_out     => alu_out,
    write_data  => write_data,
    read_data   => read_data,
    stall       => rx_enable_buf);

  rx_enable <= rx_enable_buf;

end;
