library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity testbench_alu is
  
end testbench_alu;

architecture sim of testbench_alu is

  component alu
    port (
      a, b    : in  std_logic_vector(31 downto 0);
      control : in  std_logic_vector(2 downto 0);
      output  : out std_logic_vector(31 downto 0);
      zero    : out std_logic);
  end component;

  signal a, b, output : std_logic_vector(31 downto 0) := (others => '0');
  signal control : std_logic_vector(2 downto 0) := (others => '0');
  signal zero : STD_LOGIC := '0';

  function to_string (value : STD_ULOGIC) return STRING;
  function to_string (value : STD_ULOGIC_VECTOR) return STRING;
  function to_string (value : STD_LOGIC_VECTOR) return STRING;

  alias TO_BSTRING is TO_STRING [STD_ULOGIC_VECTOR return STRING];
  alias TO_BINARY_STRING is TO_STRING [STD_ULOGIC_VECTOR return STRING];
  function TO_OSTRING (VALUE : STD_ULOGIC_VECTOR) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [STD_ULOGIC_VECTOR return STRING];
  function TO_HSTRING (VALUE : STD_ULOGIC_VECTOR) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [STD_ULOGIC_VECTOR return STRING];

  alias TO_BSTRING is TO_STRING [STD_LOGIC_VECTOR return STRING];
  alias TO_BINARY_STRING is TO_STRING [STD_LOGIC_VECTOR return STRING];
  function TO_OSTRING (VALUE : STD_LOGIC_VECTOR) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [STD_LOGIC_VECTOR return STRING];
  function TO_HSTRING (VALUE : STD_LOGIC_VECTOR) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [STD_LOGIC_VECTOR return STRING];

  type char_indexed_by_MVL9 is array (STD_ULOGIC) of CHARACTER;
  constant MVL9_to_char : char_indexed_by_MVL9 := "UX01ZWLH-";
  constant NUS  : STRING(2 to 1) := (others => ' ');     -- null STRING

  -----------------------------------------------------------------------------
  -- New string functions for vhdl-200x fast track
  -----------------------------------------------------------------------------
  function to_string (value     : STD_ULOGIC) return STRING is
    variable result : STRING (1 to 1);
  begin
    result (1) := MVL9_to_char (value);
    return result;
  end function to_string;
  -------------------------------------------------------------------    
  -- TO_STRING (an alias called "to_bstring" is provide)
  -------------------------------------------------------------------   
  function to_string (value     : STD_ULOGIC_VECTOR) return STRING is
    alias ivalue    : STD_ULOGIC_VECTOR(1 to value'length) is value;
    variable result : STRING(1 to value'length);
  begin
    if value'length < 1 then
      return NUS;
    else
      for i in ivalue'range loop
        result(i) := MVL9_to_char(iValue(i));
      end loop;
      return result;
    end if;
  end function to_string;

  -------------------------------------------------------------------    
  -- TO_HSTRING
  -------------------------------------------------------------------   
  function to_hstring (value     : STD_ULOGIC_VECTOR) return STRING is
    constant ne     : INTEGER := (value'length+3)/4;
    variable pad    : STD_ULOGIC_VECTOR(0 to (ne*4 - value'length) - 1);
    variable ivalue : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable result : STRING(1 to ne);
    variable quad   : STD_ULOGIC_VECTOR(0 to 3);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value (value'left) = 'Z' then
        pad := (others => 'Z');
      else
        pad := (others => '0');
      end if;
      ivalue := pad & value;
      for i in 0 to ne-1 loop
        quad := To_X01Z(ivalue(4*i to 4*i+3));
        case quad is
          when x"0"   => result(i+1) := '0';
          when x"1"   => result(i+1) := '1';
          when x"2"   => result(i+1) := '2';
          when x"3"   => result(i+1) := '3';
          when x"4"   => result(i+1) := '4';
          when x"5"   => result(i+1) := '5';
          when x"6"   => result(i+1) := '6';
          when x"7"   => result(i+1) := '7';
          when x"8"   => result(i+1) := '8';
          when x"9"   => result(i+1) := '9';
          when x"A"   => result(i+1) := 'A';
          when x"B"   => result(i+1) := 'B';
          when x"C"   => result(i+1) := 'C';
          when x"D"   => result(i+1) := 'D';
          when x"E"   => result(i+1) := 'E';
          when x"F"   => result(i+1) := 'F';
          when "ZZZZ" => result(i+1) := 'Z';
          when others => result(i+1) := 'X';
        end case;
      end loop;
      return result;
    end if;
  end function to_hstring;

  -------------------------------------------------------------------    
  -- TO_OSTRING
  -------------------------------------------------------------------   
  function to_ostring (value     : STD_ULOGIC_VECTOR) return STRING is
    constant ne     : INTEGER := (value'length+2)/3;
    variable pad    : STD_ULOGIC_VECTOR(0 to (ne*3 - value'length) - 1);
    variable ivalue : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable result : STRING(1 to ne);
    variable tri    : STD_ULOGIC_VECTOR(0 to 2);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value (value'left) = 'Z' then
        pad := (others => 'Z');
      else
        pad := (others => '0');
      end if;
      ivalue := pad & value;
      for i in 0 to ne-1 loop
        tri := To_X01Z(ivalue(3*i to 3*i+2));
        case tri is
          when o"0"   => result(i+1) := '0';
          when o"1"   => result(i+1) := '1';
          when o"2"   => result(i+1) := '2';
          when o"3"   => result(i+1) := '3';
          when o"4"   => result(i+1) := '4';
          when o"5"   => result(i+1) := '5';
          when o"6"   => result(i+1) := '6';
          when o"7"   => result(i+1) := '7';
          when "ZZZ"  => result(i+1) := 'Z';
          when others => result(i+1) := 'X';
        end case;
      end loop;
      return result;
    end if;
  end function to_ostring;

  function to_string (value     : STD_LOGIC_VECTOR) return STRING is
  begin
    return to_string (to_stdulogicvector (value));
  end function to_string;

  function to_hstring (value     : STD_LOGIC_VECTOR) return STRING is
  begin
    return to_hstring (to_stdulogicvector (value));
  end function to_hstring;

  function to_ostring (value     : STD_LOGIC_VECTOR) return STRING  is
  begin
    return to_ostring (to_stdulogicvector (value));
  end function to_ostring;

begin  -- sim

  dut : alu port map (
    a       => a,
    b       => b,
    control => control,
    output  => output,
    zero    => zero);

  process
  begin
    a <= x"00000012";
    b <= x"00000009";
    control <= "000";
    wait for 10 ns;
    assert output = 0 and zero = '1' report "FAILED: 18 - 9 - b000 expected to output = 0, zero = 1, but output = " & to_string(output) & ", zero = " & std_logic'image(zero) severity warning;
    -- assert false report "TEST done" severity failure;
    wait;
  end process;

end sim;
