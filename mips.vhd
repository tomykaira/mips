library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity mips is

  port (
    clk, reset       : in  STD_LOGIC;
    mem_write        : out STD_LOGIC;
    send_enable      : out STD_LOGIC;
    mem_addr         : out STD_LOGIC_VECTOR(31 downto 0);
    write_data       : out std_logic_vector(31 downto 0);
    memory_data      : in  STD_LOGIC_VECTOR(31 downto 0);
    rx_received_data : in std_logic_vector(7 downto 0);
    rx_waiting       : in  STD_LOGIC;
    rx_pop           : out STD_LOGIC);

end;

architecture struct of mips is

  component instruction_memory
     port (
       clk          : in  STD_LOGIC;
       write_enable : in  STD_LOGIC;
       address      : in  std_logic_vector(15 downto 0);
       write_data   : in std_logic_vector(31 downto 0);
       read_data    : out std_logic_vector(31 downto 0));
  end component;

  signal inst_address : std_logic_vector(15 downto 0);
  signal inst_write_data : std_logic_vector(31 downto 0);
  signal inst_write_enable : STD_LOGIC;

  component instruction_loader
     port (
       clk           : in  STD_LOGIC;
       reset         : in  STD_LOGIC;
       input_enable  : in  STD_LOGIC;
       received_data : in std_logic_vector(7 downto 0);
       in_execution  : out STD_LOGIC;
       write_enable  : out STD_LOGIC;
       write_address : out std_logic_vector(15 downto 0);
       write_data    : out std_logic_vector(31 downto 0));
  end component;

  signal in_execution : STD_LOGIC;
  signal rx_input_enable : STD_LOGIC;
  signal inst_write_address : std_logic_vector(15 downto 0);

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
      rx_pop                : out STD_LOGIC);
  end component;

  signal rx_enable : STD_LOGIC;

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
      mem_addr, write_data : out std_logic_vector(31 downto 0));
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
  signal cpu_rx_pop : STD_LOGIC;

  signal data_from_bus : std_logic_vector(31 downto 0);
  
  signal bus_to_reg,alu_src,reg_dst,reg_write : STD_LOGIC;
  signal pc_src : std_logic_vector(2 downto 0);
  signal alu_control : std_logic_vector(3 downto 0);

  signal current_stage, next_stage, decoded_next_stage : std_logic_vector(3 downto 0);

begin
  cont : main_decoder port map (
    op          => instruction(31 downto 26),
    rx_wait     => rx_waiting,
    stage       => current_stage,
    next_stage  => decoded_next_stage,
    bus_to_reg  => bus_to_reg,
    alu_src     => alu_src,
    pc_src      => pc_src,
    reg_dst     => reg_dst,
    alu_control => alu_control,
    mem_write   => mem_write,
    send_enable => send_enable,
    reg_write   => reg_write,
    rx_enable   => rx_enable,
    rx_pop      => cpu_rx_pop);

  stage_flip : flip_reset port map (
    clk   => clk,
    reset => reset,
    d     => next_stage,
    q     => current_stage);

  -- temporary fix
  -- next_stage is from main_decoder in normal operation, but hold to Fetch in data_load mode
  next_stage <= decoded_next_stage when in_execution = '1' else "0000";

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
    clk          => clk,
    write_enable => inst_write_enable,
    write_data   => inst_write_data,
    address      => inst_address,
    read_data    => instruction);

  inst_address <= pc(15 downto 0) when in_execution = '1' else inst_write_address;

  instruction_loader_inst : instruction_loader port map (
    clk           => clk,
    reset         => reset,
    input_enable  => rx_input_enable,
    received_data => rx_received_data, 
    in_execution  => in_execution,
    write_address => inst_write_address,
    write_enable  => inst_write_enable,
    write_data    => inst_write_data);

  -- is this good design to judge here?
  -- ok for reading twice?
  data_from_bus <= x"000000" & rx_received_data when rx_enable = '1' else memory_data;
  rx_pop <= '1' when in_execution = '0' or cpu_rx_pop = '1' else '0';

  -- rx_waiting = queue empty is 1 clock faster than real data comes
  process (clk)
  begin
    if rising_edge(clk) then
      rx_input_enable <= not rx_waiting;
    end if;
  end process;

end;
