library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity data_path is

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
end data_path;

architecture struct of data_path is

  component alu
    port (
      a, b    : in  std_logic_vector(31 downto 0);
      control : in  std_logic_vector(3 downto 0);
      output  : out std_logic_vector(31 downto 0)
      );
  end component;

  component register_selector
    port (
      clk                                 : in  std_logic;
      write_enable3                       : in  std_logic;
      read_addr1, read_addr2, write_addr3 : in  std_logic_vector(4 downto 0);
      write_data3                         : in  std_logic_vector(31 downto 0);
      read_data1, read_data2              : out std_logic_vector(31 downto 0);

      op : in std_logic_vector(5 downto 0));
  end component;

  component program_counter is
    port (
      clk   : in STD_LOGIC;
      reset : in std_logic;
      pc    : out std_logic_vector(31 downto 0);

      -- needed information
      pc_src           : in std_logic_vector(2 downto 0);
      jump             : in std_logic_vector(25 downto 0);
      relative         : in std_logic_vector(31 downto 0); -- sign extended
      reg              : in std_logic_vector(31 downto 0);
      stack_top        : in std_logic_vector(31 downto 0);
      branch_condition : in STD_LOGIC
    );

  end component;

  component branch_condition_checker is

    port (
      op        : in std_logic_vector(5 downto 0);
      a, b      : in std_logic_vector(31 downto 0);
      go_branch : out STD_LOGIC
      );

  end component;

  component call_stack is

    port (
      clk        : in STD_LOGIC;
      -- decide to push
      op         : in std_logic_vector(5 downto 0);
      -- as input data
      current_pc : in std_logic_vector(31 downto 0);
      pc_src     : in std_logic_vector(2 downto 0);

      stack_top  : out std_logic_vector(31 downto 0)
      );

  end component;

  signal write_reg_addr : std_logic_vector(4 downto 0);
  signal sign_immediate : std_logic_vector(31 downto 0);
  signal read_data1, read_data2, src_b, result, alu_out_buf : std_logic_vector(31 downto 0) := (others => '0');

  signal op : std_logic_vector(5 downto 0);

  signal stack_top, pc_buf : std_logic_vector(31 downto 0);

  signal branch_condition : STD_LOGIC;

begin  -- struct

  rf : register_selector port map (
    clk           => clk,
    op            => op,
    write_enable3 => reg_write,
    read_addr1    => instruction(25 downto 21),
    read_addr2    => instruction(20 downto 16),
    write_addr3   => write_reg_addr,
    write_data3   => result,
    read_data1    => read_data1,
    read_data2    => read_data2);

  main_alu : alu port map (
    a       => read_data1,
    b       => src_b,
    control => alu_control,
    output  => alu_out_buf
    );

  pc_manager : program_counter port map (
    clk              => clk,
    reset            => reset,
    pc               => pc_buf,
    pc_src           => pc_src,
    jump             => instruction(25 downto 0),
    relative         => sign_immediate,
    reg              => read_data1,
    stack_top        => stack_top,
    branch_condition => branch_condition);

  -- comparation inputs are always from register
  branch_condition_checker_inst : branch_condition_checker port map (
    op => op,
    a => read_data1,
    b => read_data2,
    go_branch => branch_condition
    );

  call_stack_inst : call_stack port map (
    clk        => clk,
    op         => op,
    current_pc => pc_buf,
    pc_src     => pc_src,
    stack_top  => stack_top
    );

  op <= instruction(31 downto 26); -- alias

  write_reg_addr <= instruction(15 downto 11) when reg_dst = '1' else instruction(20 downto 16);
  result <= data_from_bus when bus_to_reg = '1' else alu_out_buf;

  sign_immediate <= x"ffff" & instruction(15 downto 0) when instruction(15) = '1'
                    else x"0000" & instruction(15 downto 0);

  src_b <= sign_immediate when alu_src = '1' else read_data2;


  mem_addr   <= alu_out_buf;
  write_data <= read_data2;
  pc <= pc_buf;

end struct;
