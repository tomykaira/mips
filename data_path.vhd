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

	component fpu is
		port (
			clk     : in  STD_LOGIC;
			a, b    : in  std_logic_vector(31 downto 0);
			control : in  std_logic_vector(2 downto 0);
			output  : out std_logic_vector(31 downto 0));
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

	component flip_reset is

		generic (
			width : integer := 32);

		port (
			clk, reset : in  std_logic;
			d          : in  std_logic_vector(width-1 downto 0);
			q          : out std_logic_vector(width-1 downto 0));

	end component;

  signal write_reg_addr : std_logic_vector(4 downto 0);
  signal sign_immediate : std_logic_vector(31 downto 0);
  signal read_data1, read_data2, rs, rt : std_logic_vector(31 downto 0) := (others => '0');
  signal src_b, write_back, alu_out, execute_result, fpu_out, arithmetic_result : std_logic_vector(31 downto 0) := (others => '0');

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
    write_data3   => write_back,
    read_data1    => read_data1,
    read_data2    => read_data2);

  main_alu : alu port map (
    a       => rs,
    b       => src_b,
    control => alu_control,
    output  => alu_out
    );

	main_fpu : fpu port map (
		clk => clk, 
		a => read_data1, -- Never use FF before FPU
		b => read_data2, -- Never use FF before FPU
		control => alu_control(2 downto 0),
		output => fpu_out);

  pc_manager : program_counter port map (
    clk              => clk,
    reset            => reset,
    pc               => pc_buf,
    pc_src           => pc_src,
    jump             => instruction(25 downto 0),
    relative         => sign_immediate,
    reg              => rs,
    stack_top        => stack_top,
    branch_condition => branch_condition);

  -- comparation inputs are always from register
  branch_condition_checker_inst : branch_condition_checker port map (
    op => op,
    a => rs,
    b => rt,
    go_branch => branch_condition
    );

  call_stack_inst : call_stack port map (
    clk        => clk,
    op         => op,
    current_pc => pc_buf,
    pc_src     => pc_src,
    stack_top  => stack_top
    );

	read_data1_flip : flip_reset port map (
		clk   => clk,
		reset => reset,
		d     => read_data1,
		q     => rs);

	read_data2_flip : flip_reset port map (
		clk   => clk,
		reset => reset,
		d     => read_data2,
		q     => rt);

	result_flip : flip_reset port map (
		clk   => clk,
		reset => reset,
		d     => arithmetic_result, -- TODO: mux output from ALU and FPU
		q     => execute_result);

	arithmetic_result <= fpu_out when op(5 downto 3) = "110" else alu_out;

  op <= instruction(31 downto 26); -- alias

  write_reg_addr <= instruction(15 downto 11) when reg_dst = '1' else instruction(20 downto 16);

  write_back <= data_from_bus when bus_to_reg = '1' else execute_result;

  sign_immediate <= x"ffff" & instruction(15 downto 0) when instruction(15) = '1'
                    else x"0000" & instruction(15 downto 0);

  src_b <= sign_immediate when alu_src = '1' else rt;


  mem_addr   <= execute_result;
  write_data <= rt;
  pc <= pc_buf;

end struct;
