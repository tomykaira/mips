library IEE;
use IEEE.STD_LOGIC_1164.all;

entity mips is

  port (
    CLK,  XRST          : in  STD_LOGIC;
    pc                  : out STD_LOGIC_VECTOR(31 downto 0);
    instruction         : out STD_LOGIC_VECTOR(31 downto 0);
    mem_write           : out STD_LOGIC;
    alu_out, write_data : out STD_LOGIC_VECTOR(31 downto 0);
    read_data           : in  STD_LOGIC_VECTOR(31 downto 0));

end;

architecture struct of mips is

  component controller
    port (op, funct             : in STD_LOGIC_VECTOR(5 downto 0);
        zero                  : in STD_LOGIC;
        mem_to_reg, mem_write : out STD_LOGIC;
        pc_src, alu_src       : out STD_LOGIC;
        reg_dst, reg_write    : out STD_LOGIC;
        jump                  : out STD_LOGIC;
        alu_control           : out STD_LOGIC_VECTOR(2 downto 0));
  end component;

  component data_path
    
  end component;

begin
  cont : controller port map (
    op          => instruction(31 downto 26),
    funct       => instruction(5 downto 0),
    zero        => zero,
    mem_to_reg  => mem_to_reg,
    mem_write   => mem_write,
    pc_src      => pc_src,
    alu_src     => alu_src,
    reg_dst     => reg_dst,
    reg_write   => reg_write,
    jump        => jump,
    alu_control => alu_control);

  dp : data_path port map (
    clk         => clk,
    reset       => reset,
    mem_to_reg  => mem_to_reg,
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
    read_dat    => read_data);

end;
    
