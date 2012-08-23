library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- alu_src: 0: register  1: immediate
-- branch: 1: take branch (+ immediate)
-- reg_dst: 0: 20 to 16  1: 15 to 11
-- reg_write: write enable
-- TEST
-- op | mem_to_reg | mem_write | alu_src | branch | reg_dst | reg_write | jump | alu_control b
-- 0  | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 000  # and
-- 1  | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 001  # or
-- 2  | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 010  # add
-- 4  | 0          | 0         | 0       | 1      | 0       | 0         | 0    | 110  # BEQ (temp)
-- 6  | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 110  # sub
-- 7  | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 111  # slt
-- 8  | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 000  # andi  immediate
-- 9  | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 001  # ori
-- 10 | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 010  # addi
-- 14 | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 110  # subi
-- 15 | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 111  # slti
-- 35 | 1          | 0         | 1       | 0      | 0       | 1         | 0    | 010  # lw
-- 43 | 0          | 1         | 1       | 0      | 0       | 0         | 0    | 010  # sw
-- 63 | 0          | 0         | 0       | 0      | 0       | 0         | 1    | 000  # jump (temp)
-- /TEST

entity main_decoder is
  
  port (
    op                    : in  STD_LOGIC_VECTOR(5 downto 0);
    mem_to_reg, mem_write : out STD_LOGIC;
    alu_src               : out STD_LOGIC;
    branch                : out STD_LOGIC;
    reg_dst, reg_write    : out STD_LOGIC;
    jump                  : out STD_LOGIC;
    alu_control           : out STD_LOGIC_VECTOR(2 downto 0));

end main_decoder;

architecture behave of main_decoder is

  signal controls : std_logic_vector(9 downto 0);

begin  -- behave

  process(op)
  begin
    case op is
      when "000100" => controls <= "0001000110"; -- BEQ
      when "100011" => controls <= "1010010010"; -- LW
      when "101011" => controls <= "0110000010"; -- SW
      when "111111" => controls <= "0000001000"; -- J
      when others   =>
        case op(5 downto 3) is
          when "000" => controls <= "0000110" & op(2 downto 0); -- Basic arith
          when "001" => controls <= "0010010" & op(2 downto 0); -- Basic arith i
          when others => controls <=  "----------";
        end case;
    end case;
  end process;

  mem_to_reg  <= controls(9);
  mem_write   <= controls(8);
  alu_src     <= controls(7);
  branch      <= controls(6);
  reg_dst     <= controls(5);
  reg_write   <= controls(4);
  jump        <= controls(3);
  alu_control <= controls(2 downto 0);

end behave;
