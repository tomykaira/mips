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

  signal a, b, output : std_logic_vector(31 downto 0);
  signal control : std_logic_vector(2 downto 0);
  signal zero : STD_LOGIC;

begin  -- sim

  dut : alu port map (
    a       => a,
    b       => b,
    control => control,
    output  => output,
    zero    => zero);

  process
  begin
    a <= conv_std_logic_vector(18);
    b <= conv_std_logic_vector(9);
    control <= "000";
    wait for 10 ns;
    assert output = 0 and zero = '1' report "FAILED: 18 - 9 - b000 expected to output = 0, zero = 1, but output = " & integer'image(output) & ", zero = " & integer'image(zero) severity warning;
    assert false report "TEST done" severity failure;
  end process;

end sim;
