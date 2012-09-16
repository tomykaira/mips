-- FPU specification
-- units takes basically 3 clk
-- a, b should flow through.
-- Signal should in units' register at the end of decode phase.
-- doctest cannot test this, because it uses the clock.

-- 110000  000 # fmov 1 clk
-- 110001  001 # fneg 1 clk
-- 110010  010 # fadd
-- 110011  011 # fsub
-- 110100  100 # fmul
-- 110101  101 # fmuln
-- 110110  110 # finv
-- 110111  111 # fsqrt

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.all;

entity fpu is
  port (
    clk     : in  STD_LOGIC;
    a, b    : in  std_logic_vector(31 downto 0);
    control : in  std_logic_vector(2 downto 0);
    output  : out std_logic_vector(31 downto 0));
end fpu;

architecture behave of fpu is

  component fadd is
    Port (
      clk: in  STD_LOGIC;
      i1 : in  STD_LOGIC_VECTOR (31 downto 0);
      i2 : in  STD_LOGIC_VECTOR (31 downto 0);
      o  : out STD_LOGIC_VECTOR (31 downto 0)
      );
  end component;

  component fmul is
    Port (
      clk: in  STD_LOGIC;
      a : in  STD_LOGIC_VECTOR (31 downto 0);
      b : in  STD_LOGIC_VECTOR (31 downto 0);
      s : out STD_LOGIC_VECTOR (31 downto 0)
      );
  end component;

  component finv is
    Port (
      clk: in  STD_LOGIC;
      a  : in  STD_LOGIC_VECTOR (31 downto 0);
      s  : in  STD_LOGIC_VECTOR (31 downto 0)
      );
  end component;

  component fsqrt is
    Port (
      clk: in  STD_LOGIC;
      a  : in  STD_LOGIC_VECTOR (31 downto 0);
      s  : in  STD_LOGIC_VECTOR (31 downto 0)
      );
  end component;

  component fneg is
    port (
      a : in std_logic_vector(31 downto 0);
      s : out std_logic_vector(31 downto 0)
      );
  end component;

  signal b0, b_neg : std_logic_vector(31 downto 0);
  signal fmov_out, fneg_out, fadd_out, fmul_out, finv_out, fsqrt_out : std_logic_vector(31 downto 0);

begin  -- behave

  fadd_instance : fadd port map (
    clk => clk,
    i1  => a,
    i2  => b0,
    o   => fadd_out);

  fmul_instance : fmul port map (
    clk => clk,
    a   => a,
    b   => b0,
    s   => fmul_out);

  finv_instance : finv port map (
    clk => clk,
    a   => a,
    s   => finv_out);

  fsqrt_instance : fsqrt port map (
    clk => clk,
    a   => a,
    s   => fsqrt_out);

  fneg_a : fneg port map(a => a, s => fneg_out);
  fneg_b : fneg port map(a => b, s => b_neg);

  fmov_out <= a;

  b0 <= b_neg when control = "011" or control = "101" else b;

  with (control) select
    output <= fmov_out when "000",
    fneg_out when "001",
    fadd_out when "010" | "011",
    fmul_out when "100" | "101",
    finv_out when "110",
    fsqrt_out when "111"
    (others => '0') when others;

end behave;
