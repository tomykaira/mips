library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity mips is

  port (
    clk, reset    : in  STD_LOGIC;
    mem_write     : out STD_LOGIC;
    send_enable   : out STD_LOGIC;
    mem_addr      : out STD_LOGIC_VECTOR(31 downto 0);
    write_data    : out std_logic_vector(31 downto 0);
    data_from_bus : in  STD_LOGIC_VECTOR(31 downto 0);
    rx_enable     : out STD_LOGIC;
    rx_waiting    : in  STD_LOGIC;
    rx_pop        : out STD_LOGIC);

end;

architecture struct of mips is

  component instruction_memory
     port (
       clk : in STD_LOGIC;
       a  : in  std_logic_vector(15 downto 0);
       rd : out std_logic_vector(31 downto 0));
  end component;

  component main_decoder is

    port (
      op                    : in  STD_LOGIC_VECTOR(5 downto 0);
      rx_wait               : in  STD_LOGIC;
      stage                 : in  std_logic_vector(3 downto 0);
      next_stage            : out std_logic_vector(3 downto 0);
      bus_to_reg            : out STD_LOGIC;
      alu_src               : out STD_LOGIC;
      pc_src                : out std_logic_vector(2 downto 0);
      reg_dst               : out STD_LOGIC;
      alu_control           : out STD_LOGIC_VECTOR(3 downto 0);
      mem_write             : out STD_LOGIC;
      send_enable           : out STD_LOGIC;
      reg_write             : out STD_LOGIC;
      rx_enable             : out STD_LOGIC;
      rx_pop                : out STD_LOGIC
      );

  end component;

  component data_path is
  
    port (
      clk, reset           : in  std_logic;
      bus_to_reg           : in  std_logic;
      pc_src               : in  std_logic_vector(2 downto 0);
      alu_src, reg_dst     : in  std_logic;
      reg_write            : in  std_logic;
      alu_control          : in  std_logic_vector(3 downto 0);
      instruction          : in  std_logic_vector(31 downto 0);
      data_from_bus        : in  std_logic_vector(31 downto 0);
      pc                   : out std_logic_vector(31 downto 0);
      mem_addr, write_data : out std_logic_vector(31 downto 0)
      ); 
  end component;


  component flip_reset is
      generic (
      width : integer := 4);

    port (
      clk, reset : in  std_logic;
      d          : in  std_logic_vector(width-1 downto 0);
      q          : out std_logic_vector(width-1 downto 0));

  end component;

  signal pc, instruction : std_logic_vector(31 downto 0);

  signal bus_to_reg,alu_src,reg_dst,reg_write : STD_LOGIC;
  signal pc_src : std_logic_vector(2 downto 0);
  signal alu_control : std_logic_vector(3 downto 0);

  signal current_stage, next_stage : std_logic_vector(3 downto 0);

begin
  cont : main_decoder port map (
    op          => instruction(31 downto 26),
    rx_wait     => rx_waiting,
    stage       => current_stage,
    next_stage  => next_stage,
    bus_to_reg  => bus_to_reg,
    alu_src     => alu_src,
    pc_src      => pc_src,
    reg_dst     => reg_dst,
    alu_control => alu_control,
    mem_write   => mem_write,
    send_enable => send_enable,
    reg_write   => reg_write,
    rx_enable   => rx_enable,
    rx_pop      => rx_pop);

  stage_flip : flip_reset port map (
    clk   => clk,
    reset => reset,
    d     => next_stage,
    q     => current_stage);

  dp : data_path port map (
    clk           => clk,
    reset         => reset,
    bus_to_reg    => bus_to_reg,
    pc_src        => pc_src,
    alu_src       => alu_src,
    reg_dst       => reg_dst,
    reg_write     => reg_write,
    alu_control   => alu_control,
    pc            => pc,
    instruction   => instruction,
    mem_addr      => mem_addr,
    write_data    => write_data,
    data_from_bus => data_from_bus);

  imem : instruction_memory port map(
    clk => clk,
    a   => pc(15 downto 0),
    rd  => instruction);

end;
