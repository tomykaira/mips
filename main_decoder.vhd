library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- alu_src: 0: register  1: immediate
-- branch: 1: take branch (+ immediate)
-- reg_dst: 0: 20 to 16  1: 15 to 11
-- reg_write: write enable
-- TEST
-- op | rx_done | bus_to_reg | mem_write | alu_src | branch | reg_dst | reg_write | jump | rx_enable | send_enable | alu_control b
-- 0  | 0       | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 0         | 0           | 000
-- 1  | 0       | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 0         | 0           | 001
-- 2  | 0       | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 0         | 0           | 010
-- 4  | 0       | 0          | 0         | -       | 0      | 0       | 0         | 0    | 0         | 1           | ---
-- 6  | 0       | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 0         | 0           | 110
-- 7  | 0       | 0          | 0         | 0       | 0      | 1       | 1         | 0    | 0         | 0           | 111
-- 8  | 0       | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 000
-- 9  | 0       | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 001
-- 10 | 0       | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 010
-- 12 | 0       | 1          | 0         | -       | 0      | 0       | 1         | 0    | 1         | 0           | ---
-- 12 | 1       |            |           |         |        |         |           |      | 0         | 0           | ---
-- 14 | 0       | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 110
-- 15 | 0       | 0          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 111
-- 35 | 0       | 1          | 0         | 1       | 0      | 0       | 1         | 0    | 0         | 0           | 010
-- 43 | 0       | 0          | 1         | 1       | 0      | 0       | 0         | 0    | 0         | 0           | 010
-- 62 | 0       | 0          | 0         | 0       | 1      | 0       | 0         | 0    | 0         | 0           | 110
-- 63 | 0       | 0          | 0         | 0       | 0      | 0       | 0         | 1    | 0         | 0           | 000
-- /TEST

entity main_decoder is
  
  port (
    op                    : in  STD_LOGIC_VECTOR(5 downto 0);
    rx_done               : in  STD_LOGIC;
    bus_to_reg, mem_write : out STD_LOGIC;
    alu_src               : out STD_LOGIC;
    branch                : out STD_LOGIC;
    reg_dst, reg_write    : out STD_LOGIC;
    jump                  : out STD_LOGIC;
    rx_enable             : out STD_LOGIC;
    send_enable           : out STD_LOGIC;
    alu_control           : out STD_LOGIC_VECTOR(2 downto 0));

end main_decoder;

architecture behave of main_decoder is

  signal controls : std_logic_vector(11 downto 0);

begin  -- behave

  process(op)
  begin
    case op is
      when "000100" => controls <= "000000001000"; -- Send via RS232C
      when "001100" => controls <= "100001010000"; -- Load from RS232C
      when "100011" => controls <= "101001000010"; -- LW
      when "101011" => controls <= "011000000010"; -- SW
      when "111110" => controls <= "000100000110"; -- BEQ
      when "111111" => controls <= "000000100000"; -- J
      when others   =>
        case op(5 downto 3) is
          when "000" => controls <= "000011000" & op(2 downto 0); -- Basic arith
          when "001" => controls <= "001001000" & op(2 downto 0); -- Basic arith i
          when others => controls <=  "------------";
        end case;
    end case;
  end process;

  bus_to_reg  <= controls(11);
  mem_write   <= controls(10);
  alu_src     <= controls(9);
  branch      <= controls(8);
  reg_dst     <= controls(7);
  reg_write   <= controls(6);
  jump        <= controls(5);
  rx_enable   <= not rx_done and controls(4);
  send_enable <= controls(3);
  alu_control <= controls(2 downto 0);

end behave;
