library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity main_decoder is
  
  port (
    op                    : in  STD_LOGIC_VECTOR(5 downto 0);
    mem_to_reg, mem_write : out STD_LOGIC;
    alu_src               : out STD_LOGIC;
    branch                : out STD_LOGIC;
    reg_dst, reg_write    : out STD_LOGIC;
    jump                  : out STD_LOGIC;
    alu_op                : out STD_LOGIC_VECTOR(1 downto 0));

end main_decoder;

architecture behave of main_decoder is

  signal controls : std_logic_vector(8 downto 0);

begin  -- behave

  process(op)
  begin
    case op is
      when "000000" => controls <= "110000010"; -- Rtype
      when "100011" => controls <= "101001000"; -- LW
      when "101011" => controls <= "001010000"; -- SW
      when "000100" => controls <= "000100001"; -- BEQ
      when "001000" => controls <= "101000000"; -- ADDI
      when "000010" => controls <= "000000100"; -- J
      when others   => controls <= "---------";
    end case;
  end process;

  reg_write  <= controls(8);
  reg_dst    <= controls(7);
  alu_src    <= controls(6);
  branch     <= controls(5);
  mem_write  <= controls(4);
  mem_to_reg <= controls(3);
  jump       <= controls(2);
  alu_op      <= controls(1 downto 0);

end behave;
