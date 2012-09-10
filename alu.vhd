library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.all;

-- new ALU specification
-- 0000: add
-- 0001: sub
-- 0010: mul
-- 0011: and
-- 0100: or
-- 0101: nor
-- 0110: xor
-- 0111: ---
-- 1000: mvlo: put immediate into low 16 bits
-- 1001: mvhi: put immediate into high 16 bits
-- 1010: SLLi Shift left w/ immediate
-- 1011: SRAi shift right arithmetic w/ immediate
-- 1100: imovf: move int to float. just a signal for data path
-- 1101: fmovi: move float to int

-- TEST
-- a  |b   |control b|output
-- 18 |9   |0000     |27    # add
-- -9 |9   |         |0     
-- -18|9   |         |-9    
-- 18 |9   |0001     |9     # sub
-- 9  |18  |         |-9    
-- 9  |9   |         |0     
-- 9  |9   |0010     |81    # mul
-- 0  |9   |         |0     
-- -2 |9   |         |-18   
-- -2 |-2  |         |4     
-- 9  |18  |0011     |0     # and
-- 9  |1   |         |1     
-- 9  |10  |         |8     
-- 9  |18  |0100     |27    # or
-- 9  |0   |         |9     
-- 9  |18  |0101     |4294967268   # nor
-- 1  |0   |0110     |1     # xor
-- 3  |5   |         |6  
-- 2  |3   |         |1  
-- 2  |2   |         |0  
-- 5  |3   |1010     |40    # SLL
-- 0  |3   |         |0  
-- -1 |1   |         |-2 
-- 5  |1   |1011     |2     # SRA
-- 2  |1   |         |1  
-- -1 |5   |         |-1 
-- -5 |1   |         |-3 
-- -5 |2   |         |-2 
-- 1  |1   |         |0  
-- 0  |1   |         |0  
-- # mul between large numbers
-- 2147483647 |1     | 0010 | 2147483647  # mul
-- 32768      |65536 |      | 2147483648  # mul
-- -32768     |65536 |      | -2147483648 # mul
-- /TEST

entity alu is

  port (
    a, b    : in  std_logic_vector(31 downto 0);
    control : in  std_logic_vector(3 downto 0);
    output  : out std_logic_vector(31 downto 0));

end alu;

architecture behave of alu is

  signal out_buf : std_logic_vector(31 downto 0);

begin  -- behave

  process(a, b, control)
    variable out_buf2 : std_logic_vector(63 downto 0);
  begin
    case control is
      when "0000" =>
        out_buf <= a + b;
      when "0001" =>
        out_buf <= a - b;
      when "0010" =>
        out_buf2 := a * b;
        out_buf <= out_buf2(31 downto 0);
      when "0011" =>
        out_buf <= a and b;
      when "0100" =>
        out_buf <= a or b;
      when "0101" =>
        out_buf <= not (a or b);
      when "0110" =>
        out_buf <= a xor b;
      when "1010" =>
        out_buf <= (others => '0');
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        out_buf(31 downto conv_integer(b(30 downto 0))) <= a(31-conv_integer(b(30 downto 0)) downto 0);
      when "1011" =>
        out_buf <= (others => a(31));
        -- conv_integer accepts array which is shorter than 32
        -- b must be smaller than 32
        out_buf(31-conv_integer(b(30 downto 0)) downto 0) <= a(31 downto conv_integer(b(30 downto 0)));
      when others => null;
    end case;
  end process;
  
  output <= out_buf;

end behave;
