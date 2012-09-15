library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

-- fake implementation of SRAM for testing
-- it needs more configuration to match with actual SRAM

entity fake_sram is
  
  port (
    -- data and parity
    ZD:  inout std_logic_vector(31 downto 0);
    ZDP: inout std_logic_vector(3 downto 0);

    -- address
    ZA     : in std_logic_vector(19 downto 0);

    -- X write enable
    XWA    : in std_logic;

    -- use for clock
    ZCLKMA : in std_logic_vector(1 downto 0);

    -- ignored
    XE1    : in std_logic; -- 0
    E2A    : in std_logic; -- 1
    XE3    : in std_logic; -- 0

    XZBE   : in std_logic_vector(3 downto 0); -- 0000

    XGA    : in std_logic; -- 0

    XZCKE  : in std_logic; -- 0
    ADVA   : in std_logic; -- 0
    XFT    : in std_logic; -- 1
    XLBO   : in std_logic; -- 1
    ZZA    : in std_logic  -- 0
);

end fake_sram;

architecture behave of fake_sram is

  constant MEM_SIZE : integer := 1024;

  -- address max for 20bits: 1048576
  -- blockram is capable
  -- 1024 for test
  type ram_type is array (MEM_SIZE downto 0) of std_logic_vector(31 downto 0);
  signal mem : ram_type;

  signal sram_data : std_logic_vector(31 downto 0);

  signal read1, write1, write2 : std_logic_vector(31 downto 0);
  signal addr1, addr2 : std_logic_vector(19 downto 0);

  signal clk : STD_LOGIC;

begin  -- behave

  ZD  <= sram_data when XWA='1' else (others => 'Z');
  ZDP <= (others => 'Z'); -- not available

  clk <= ZCLKMA(0);

  assert XE1   = '0' report "Fake SRAM: XE1";
  assert E2A   = '1' report "Fake SRAM: E2A";
  assert XE3   = '0' report "Fake SRAM: XE3";
  assert XGA   = '0' report "Fake SRAM: XGA";
  assert XZCKE = '0' report "Fake SRAM: XZCKE";
  assert ADVA  = '0' report "Fake SRAM: ADVA";
  assert XFT   = '1' report "Fake SRAM: XFT";
  assert XLBO  = '1' report "Fake SRAM: XLBO";
  assert ZZA   = '0' report "Fake SRAM: ZZA";

  assert XZBE  = "0000" report "Fake SRAM: XZBE";

  sram_mock: process (clk, ZD, ZA, XWA)
  begin  -- process sram_mock
    if rising_edge(clk) then
      if XWA = '0' then
        -- ZA is always connected. problem is in when it is writing
        assert ZA <= MEM_SIZE report "Writing.. ZA is greater than 1024.";
        if ZA <= MEM_SIZE then
          write1     <= ZD;
          addr1      <= ZA;
          write2 <= write1;
          addr2 <= addr1;
          -- not essential, for safety
          mem(conv_integer(addr2)) <= write2;
        end if;
      else
        if ZA <= MEM_SIZE then
          read1     <= mem(conv_integer(ZA));
          sram_data <= read1;
        end if;
      end if;
    end if;
  end process sram_mock;

end behave;
