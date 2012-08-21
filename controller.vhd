library IEE;
use IEEE.STD_LOGIC_1164.all;

entity controller is
  port (op, funct             : in STD_LOGIC_VECTOR(5 downto 0);
        zero                  : in STD_LOGIC;
        mem_to_reg, mem_write : out STD_LOGIC;
        pc_src, alu_src       : out STD_LOGIC;
        reg_dst, reg_write    : out STD_LOGIC;
        jump                  : out STD_LOGIC;
        alu_control           : out STD_LOGIC_VECTOR(2 downto 0));
end;

architecture struct of controller is
  component main_decoder
    port (
      op                    : in  STD_LOGIC_VECTOR(5 downto 0);
      mem_to_reg, mem_write : out STD_LOGIC;
      branch                : out STD_LOGIC;
      reg_dst, reg_write    : out STD_LOGIC;
      jump                  : out STD_LOGIC;
      alu_op                : out STD_LOGIC_VECTOR(1 downto 0));
  end component;

  component alu_decoder
    port (
      funct        : in  std_logic_vector(5 downto 0);
      alu_op       : in  std_logic_vector(1 downto 0);
      alu_control  : out std_logic_vector(2 downto 0));
  end component;

  signal alu_op : STD_LOGIC_VECTOR (1 downto 0);
  signal branch : STD_LOGIC;

begin
  md : main_decoder port map (
    op         => op,
    mem_to_reg => mem_to_reg,
    mem_write  => mem_write,
    branch     => branch,
    alu_src    => alu_src,
    reg_dst    => reg_dst,
    reg_write  => reg_write,
    jump       => jump,
    alu_op     => alu_op);

  ad : alu_decoder port map (
    funct       => funct,
    alu_op      => alu_op,
    alu_control => alu_control);
end;
