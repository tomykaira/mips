library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity data_path is
  
  port (
    clk, reset          : in  std_logic;
    bus_to_reg, pc_src  : in  std_logic;
    alu_src, reg_dst    : in  std_logic;
    reg_write, jump     : in  std_logic;
    write_pc            : in  STD_LOGIC;
    alu_control         : in  std_logic_vector(3 downto 0);
    zero                : out std_logic;
    pc                  : out std_logic_vector(31 downto 0);
    instruction         : in  std_logic_vector(31 downto 0);
    alu_out, write_data : out std_logic_vector(31 downto 0);
    data_from_bus       : in  std_logic_vector(31 downto 0));

end data_path;

architecture struct of data_path is

  component alu
    port (
      a, b    : in  std_logic_vector(31 downto 0);
      control : in  std_logic_vector(2 downto 0);
      output  : out std_logic_vector(31 downto 0);
      zero    : out std_logic);
  end component;

  component register_selector
    port (
      clk                                 : in  std_logic;
      write_enable3                       : in  std_logic;
      read_addr1, read_addr2, write_addr3 : in  std_logic_vector(4 downto 0);
      write_data3                         : in  std_logic_vector(31 downto 0);
      read_data1, read_data2              : out std_logic_vector(31 downto 0);

      op : in std_logic_vector(6 downto 0));
  end component;

  signal write_reg_addr : std_logic_vector(4 downto 0);
  signal pc_jump, pc_next, pc_next_branch, pc_plus4, pc_branch : std_logic_vector(31 downto 0);
  signal sign_immediate, sign_immediate_sh : std_logic_vector(31 downto 0);
  signal src_a, src_b, result : std_logic_vector(31 downto 0);

  signal pc_buf, write_data_buf, alu_out_buf : std_logic_vector(31 downto 0) := (others => '0');

begin  -- struct

  pc_jump <= pc_plus4(31 downto 28) & instruction(25 downto 0) & "00" when alu_src = '1' else
             write_data_buf;

  pc_plus4 <= pc_buf + 4;
  sign_immediate_sh <= sign_immediate(29 downto 0) & "00";
  pc_branch <= pc_plus4 + sign_immediate_sh;

  pc_next_branch <= pc_branch when pc_src = '1' else pc_plus4;
  pc_next <= pc_jump when jump = '1' else pc_next_branch;

  rf : register_selector port map (
    clk           => clk,
    op            => instruction(31 downto 26),
    write_enable3 => reg_write,
    read_addr1    => instruction(25 downto 21),
    read_addr2    => instruction(20 downto 16),
    write_addr3   => write_reg_addr,
    write_data3   => result,
    read_data1    => src_a,
    read_data2    => write_data_buf);

  write_reg_addr <= instruction(15 downto 11) when reg_dst = '1' else instruction(20 downto 16);
  result <= data_from_bus when bus_to_reg = '1' else alu_out_buf;

  sign_immediate <= x"ffff" & instruction(15 downto 0) when instruction(15) = '1'
                    else x"0000" & instruction(15 downto 0);

  src_b <= sign_immediate when alu_src = '1' else write_data_buf;

  main_alu : alu port map (
    a       => src_a,
    b       => src_b,
    control => alu_control,
    output  => alu_out_buf,
    zero    => zero);


  alu_out    <= alu_out_buf;
  pc         <= pc_buf;
  write_data <= pc_buf + 8 when write_pc = '1' else write_data_buf;
  
end struct;
