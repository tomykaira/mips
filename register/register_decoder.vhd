library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Decode operator and judge which register file to use.

-- TEST
-- alias I 0
-- alias F 1
-- op   b | addr1 | addr2 | addr3
-- 000000 | I     | I     | I    # add
-- 000001 | I     | I     | I    # sub
-- 000010 | I     | I     | I    # mul
-- 000011 | I     | I     | I    # and
-- 000100 | I     | I     | I    # or
-- 000101 | I     | I     | I    # nor
-- 000110 | I     | I     | I    # xor
-- #
-- 001000 | I     | -     | I    # addi
-- 001001 | I     | -     | I    # subi
-- 001010 | I     | -     | I    # muli
-- 000111 | I     | -     | I    # slli
-- 001111 | I     | -     | I    # srai
-- 001011 | I     | -     | I    # andi
-- 001100 | I     | -     | I    # ori
-- 001101 | I     | -     | I    # nori
-- 001110 | I     | -     | I    # xori
-- #
-- 010000 | I     | -     | I    # mvlo
-- 010001 | I     | -     | I    # mvhi
-- 010010 | F     | -     | F    # fmvlo
-- 010011 | F     | -     | F    # fmvhi
-- 010110 | I     | -     | F    # imovf
-- 010111 | F     | -     | I    # fmovi
-- #
-- 110000 | F     | -     | F    # fmov
-- 110001 | F     | -     | F    # fneg
-- 110010 | F     | F     | F    # fadd
-- 110011 | F     | F     | F    # fsub
-- 110100 | F     | F     | F    # fmul
-- 110101 | F     | F     | F    # fmuln
-- 110110 | F     | F     | F    # fdiv
-- 110111 | F     | -     | F    # fsqrt
-- #
-- 101000 | I     | I     | I    # ldi
-- 101001 | I     | I     | I    # sti
-- 101100 | I     | I     | I    # ldr
-- 101010 | I     | I     | F    # fldi
-- 101011 | I     | F     | I    # fsti
-- 101110 | I     | I     | F    # fldr
-- #
-- 100000 | I     | I     | -    # beq
-- 100001 | I     | I     | -    # blt
-- 100010 | I     | I     | -    # ble
-- 100100 | F     | F     | -    # fbeq
-- 100101 | F     | F     | -    # fblt
-- 100110 | F     | F     | -    # fble
-- #
-- # j does not use register
-- 111001 | I     | -     | -    # jr
-- 111010 | I     | I     | I    # call
-- 111011 | I     | I     | I    # callr
-- 111100 | I     | I     | I    # return
-- 111101 | -     | -     | I    # inputb
-- 111110 | -     | I     | -    # outputb
-- # halt does not use register
-- /TEST


entity register_decoder is
  port (
    -- instruction operator to judge which port is used
    op : in std_logic_vector(5 downto 0);
    -- bounded register file. 0 for int, 1 for float
    addr1, addr2, addr3 : out STD_LOGIC
);
end register_decoder;

architecture behave of register_decoder is

  signal op_group, op_id : std_logic_vector(2 downto 0);
  signal vector : std_logic_vector(2 downto 0);

begin -- behave
    
  op_group <= op(5 downto 3);
  op_id <= op(2 downto 0);

  addr1 <= vector(2);
  addr2 <= vector(1);
  addr3 <= vector(0);

  process (op, op_group, op_id)
  begin
    vector <= "000";
    case op_group is
      when "000" | "001" | "111" =>
        vector <= "000";

      -- move
      when "010" =>
        case op_id is
          when "000" | "001" | "100" | "101" =>
            vector <= "000";
          when "010" | "011" =>
            vector <= "101";
          when "110" =>
            vector <= "001";
          when "111" =>
            vector <= "100";
          when others =>
            assert false report "unknown operator";
        end case;
        
      -- branch
      when "100" =>
        -- this is naive,  but correct
        if op_id(2) = '1' then
          vector <= "110";
        else
          vector <= "000";
        end if;

      -- load / store
      when "101" =>
        if op_id = "010" or op_id = "110" then
          vector <= "001";
        elsif op_id = "011" then
          vector <= "010";
        else
          vector <= "000";
        end if;

      -- floating arith
      when "110" =>
        vector <= "111";
      when others => null;

    end case;
  end process;
  
end behave;
