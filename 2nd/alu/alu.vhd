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

  signal op : std_logic_vector(5 downto 0);
  signal rt_addr, rd_addr : std_logic_vector(4 downto 0);
  signal other : std_logic_vector(31 downto 0);
  signal out_buf : std_logic_vector(31 downto 0);

begin  -- behave

  float <= '0';

  op <= inst(31 downto 26);
  rt_addr <= inst(20 downto 16);
  rd_addr <= inst(15 downto 11);

  process(op, rt_addr, rd_addr)
  begin
    case op is
      when "000000" | "000001" | "000010" =>
        enable <= '1';
        addr <= rd_addr;
        other <= rt;
      when "000011" | "000100" | "000101" | "000110" | "000111" =>
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
      when "000000" | "000011" =>
        out_buf <= rs + other;
      when "000001" | "000100" =>
        out_buf <= rs - other;
      when "000010" | "000101"=>
        out_buf <= rs xor other;
      when "000110" =>
        out_buf <= (others => '0');
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        assert imm < x"100000" report "shift amount is too large";
        shift_amount := conv_integer(imm(4 downto 0));
        out_buf(31 downto shift_amount) <= rs(31-shift_amount downto 0);
      when "000111" =>
        out_buf <= (others => rs(31));
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        assert imm < x"100000" report "shift amount is too large";
        shift_amount := conv_integer(imm(4 downto 0));
        out_buf(31-shift_amount downto 0) <= rs(31 downto shift_amount);
      when others =>
        out_buf <= (others => 'Z');
    end case;
  end process;

  data <= out_buf;

end behave;
