library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.all;

-- TEST
-- alias ADD 000000
-- alias SUB 000001
-- alias XOR 000010
-- alias ADDI 000011
-- alias SUBI 000100
-- alias XORI 000101
-- alias SLLI 000110
-- alias SRAI 000111
-- alias FMVLO 010010
-- alias FMVHI 010011
-- alias IMOVF 010110
-- alias FMOVI 010111
-- def i { |x| (x + "00001000100001000000000000").to_i(2) }
-- inst i| rs | rt | imm | enable | addr | data | float
-- ADD   |  9 | 18 |   3 |      1 |    2 |   27 |     0
-- SUB   |  9 | 18 |   3 |      1 |    2 |   -9 |     0
-- XOR   |  9 | 18 |   3 |      1 |    2 |   27 |     0
-- ADDI  |  9 | 18 |   3 |      1 |    2 |   12 |     0
-- SUBI  |  9 | 18 |   3 |      1 |    2 |    6 |     0
-- XORI  |  9 | 18 |   3 |      1 |    2 |   10 |     0
-- SLLI  |  1 |  0 |   3 |      1 |    2 |    8 |     0
-- SRAI  | 25 |  0 |   3 |      1 |    2 |    3 |     0
-- SRAI  | -1 |  0 |   5 |      1 |    2 |   -1 |     0
-- SRAI  | -9 |  0 |   1 |      1 |    2 |   -5 |     0
-- FMVHI |  5 |  2 |   2 |      1 |    2 |131072|     1
-- FMVLO |131072|2 |   3 |      1 |    2 |131075|     1
-- IMOVF |  5 |  2 |   0 |      1 |    2 |    5 |     1
-- FMOVI |  5 |  2 |   0 |      1 |    2 |    5 |     0
-- 111111|  0 |  0 |   0 |      0 |    - |    - |     -
-- /TEST

entity alu is

  port (
    inst        : in std_logic_vector(31 downto 0);
    rs, rt, imm : in std_logic_vector(31 downto 0);
    enable      : out STD_LOGIC;
    addr        : out std_logic_vector(4 downto 0);
    data        : out std_logic_vector(31 downto 0);
    float       : out STD_LOGIC);

end alu;

architecture behave of alu is


  constant ADD : std_logic_vector(5 downto 0) := "000000";
  constant SUB : std_logic_vector(5 downto 0) := "000001";
  constant cXOR : std_logic_vector(5 downto 0) := "000010";
  constant ADDI : std_logic_vector(5 downto 0) := "000011";
  constant SUBI : std_logic_vector(5 downto 0) := "000100";
  constant XORI : std_logic_vector(5 downto 0) := "000101";
  constant SLLI : std_logic_vector(5 downto 0) := "000110";
  constant SRAI : std_logic_vector(5 downto 0) := "000111";

  constant FMVLO : std_logic_vector(5 downto 0) := "010010";
  constant FMVHI : std_logic_vector(5 downto 0) := "010011";
  constant IMOVF : std_logic_vector(5 downto 0) := "010110";
  constant FMOVI : std_logic_vector(5 downto 0) := "010111";

  signal op : std_logic_vector(5 downto 0);
  signal rt_addr, rd_addr : std_logic_vector(4 downto 0);
  signal other : std_logic_vector(31 downto 0);
  signal out_buf : std_logic_vector(31 downto 0);

begin  -- behave

  float <= '1' when op = FMVHI or op = FMVLO or OP = IMOVF else '0';

  op <= inst(31 downto 26);
  rt_addr <= inst(20 downto 16);
  rd_addr <= inst(15 downto 11);

  process(op, rt, imm, rt_addr, rd_addr)
  begin
    case op is
      when ADD | SUB | cXOR =>
        enable <= '1';
        addr <= rd_addr;
        other <= rt;
      when ADDI | SUBI | XORI | SLLI | SRAI | FMVLO | FMVHI | IMOVF | FMOVI =>
        enable <= '1';
        addr <= rt_addr;
        other <= imm;
      when others =>
        enable <= '0';
    end case;
  end process;

  process(op, rs, other, imm)
    variable shift_amount : integer;
  begin
    case op is
      when ADD | ADDI =>
        out_buf <= rs + other;
      when SUB | SUBI =>
        out_buf <= rs - other;
      when cXOR | XORI =>
        out_buf <= rs xor other;
      when SLLI =>
        out_buf <= (others => '0');
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        assert imm < x"100000" report "shift amount is too large";
        shift_amount := conv_integer(imm(4 downto 0));
        out_buf(31 downto shift_amount) <= rs(31-shift_amount downto 0);
      when SRAI =>
        out_buf <= (others => rs(31));
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        assert imm < x"100000" report "shift amount is too large";
        shift_amount := conv_integer(imm(4 downto 0));
        out_buf(31-shift_amount downto 0) <= rs(31 downto shift_amount);
      when FMOVI | IMOVF =>
        out_buf <= rs;
      when FMVLO =>
        out_buf <= rs(31 downto 16) & imm(15 downto 0);
      when FMVHI =>
        out_buf <= imm(15 downto 0) & x"0000";
      when others =>
        out_buf <= (others => 'Z');
    end case;
  end process;

  data <= out_buf;

end behave;
