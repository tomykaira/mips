library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity alu_decoder is
  
  port (
    funct        : in  std_logic_vector(5 downto 0);
    alu_op       : in  std_logic_vector(1 downto 0);
    alu_control  : out std_logic_vector(2 downto 0));

end alu_decoder;

architecture behave of alu_decoder is

begin  -- behave

  process(alu_op, funct)
  begin  -- process alu_op, funct

    case alu_op is
      when "00" => alu_control <= "010"; -- ADD
      when "01" => alu_control <= "110"; -- DEC
      when others => case funct is
        when "100000" => alu_control <= "010"; -- add
        when "100010" => alu_control <= "110"; -- sub
        when "100100" => alu_control <= "000"; -- and
        when "100101" => alu_control <= "001"; -- or
        when "101010" => alu_control <= "111"; -- slt
        when others   => alu_control <= "---";
      end case;
    end case;
    
  end process;

end behave;
