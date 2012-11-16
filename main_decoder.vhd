library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- main decoder
-- next_stage goes to flipflop, and will back in next clock
-- other control signals go to modules, affect the operation in current clock (? which is better?)

-- F: Update PC.  instruction comes at the next clock
-- D: Read register. they should go into a latch
-- E: Calculate the answer.  the result go into a latch
-- M: Memory read / write. 3 clocks. Instruction should be the same in this sequence.
--    mem_write is true only in this
-- W: Write back to register. reg_write is true only in this
--    Decide next PC and keep it in a latch

-- TEST
-- alias BREG 100
-- alias REG  0
-- alias IMM  1
-- alias CUR  000
-- alias NEXT 001
-- alias J    010
-- alias REL  011
-- alias LR   101
-- alias FRD  1
-- alias FRT  0
-- alias RD   1
-- alias RT   0
-- alias INT   0
-- alias FLOAT 1
-- # stage aliases
-- alias F  0
-- alias D  1
-- alias E  2
-- alias E1 3
-- alias E2 4
-- alias M  5
-- alias M1 6
-- alias M2 7
-- alias W  8
-- alias W1 9
-- op b   | rx_wait | stage | next_stage | bus_to_reg | alu_src |pc_src b| reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable | rx_pop
-- # reg x reg arithmetic
-- 000000 |       0 |     F | D          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # add
-- 000000 |       0 |     D | E          |            | -       | CUR    | -       | -             |           |             |           |           | 0 
-- 000000 |       0 |     E | W          |            | REG     | CUR    | -       | 000           |           |             |           |           | 0 
-- 000000 |       0 |     W | F          |            | -       | NEXT   | RD      | -             |           |             | 1         |           | 0 
-- 000001 |       0 |     E | W          |            | REG     | CUR    | -       | 001           |           |             | 0         |           | 0 # sub
-- 000010 |       0 |     E | W          |            |         |        |         | 010           |           |             |           |           | 0 # mul
-- 000011 |       0 |     E | W          |            |         |        |         | 011           |           |             |           |           | 0 # and
-- 000100 |       0 |     E | W          |            |         |        |         | 100           |           |             |           |           | 0 # or
-- 000101 |       0 |     E | W          |            |         |        |         | 101           |           |             |           |           | 0 # nor
-- 000110 |       0 |     E | W          |            |         |        |         | 110           |           |             |           |           | 0 # xor
-- # imm x reg arithmetic
-- 001000 |       0 |     E | W          |            | IMM     |        |         | 000           |           |             | 0         |           | 0 # addi # execute
-- 001001 |       0 |       |            |            | IMM     |        |         | 001           |           |             | 0         |           | 0 # subi
-- 001010 |       0 |       |            |            | IMM     |        |         | 010           |           |             | 0         |           | 0 # muli
-- 001011 |       0 |       |            |            | IMM     |        |         | 011           |           |             | 0         |           | 0 # andi
-- 001100 |       0 |       |            |            | IMM     |        |         | 100           |           |             | 0         |           | 0 # ori
-- 001101 |       0 |       |            |            | IMM     |        |         | 101           |           |             | 0         |           | 0 # nori
-- 001110 |       0 |       |            |            | IMM     |        |         | 110           |           |             | 0         |           | 0 # xori
-- 001000 |       0 |     W | F          |            | -       | NEXT   | RT      | -             | 0         | 0           | 1         | 0         | 0 # addi # write
-- 001001 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # subi
-- 001010 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # muli
-- 001011 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # andi
-- 001100 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # ori
-- 001101 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # nori
-- 001110 |       0 |       |            |            |         |        |         |               |           |             | 1         |           | 0 # xori
-- # move and other immediate instructions
-- 010000 |         |     D | E          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # mvlo
-- 010000 |         |     E | W          |            | IMM     |        |         | 1000          |           |             | 0         |           | 0 # mvlo
-- 010001 |         |     E | W          |            | IMM     |        |         | 1001          |           |             | 0         |           | 0 # mvhi
-- 010010 |         |     E | W          |            | IMM     |        |         | 1000          |           |             | 0         |           | 0 # fmvlo
-- 010011 |         |     E | W          |            | IMM     |        |         | 1001          |           |             | 0         |           | 0 # fmvhi
-- 010100 |         |     E | W          |            | IMM     |        |         | 1010          |           |             | 0         |           | 0 # slli
-- 010101 |         |     E | W          |            | IMM     |        |         | 1011          |           |             | 0         |           | 0 # srai
-- 010110 |         |     E | W          |            | IMM     |        |         | 0000          |           |             | 0         |           | 0 # imovf # add 0 + reg
-- 010111 |         |     E | W          |            | IMM     |        |         | 0000          |           |             | 0         |           | 0 # fmovi
-- 010000 |         |     W | F          |            | -       | NEXT   | RT      | -             |           |             | 1         |           | 0 # mvlo
-- 010010 |         |     W | F          |            | -       | NEXT   | FRT     | -             |           |             | 1         |           | 0 # fmvlo
-- 010010 |         |     W | F          |            | -       | NEXT   | FRT     | -             |           |             | 1         |           | 0 # fmvhi
-- 010100 |         |     W | F          |            | IMM     | NEXT   | RT      | -             |           |             | 1         |           | 0 # slli
-- 010101 |         |     W | F          |            | IMM     | NEXT   | RT      | -             |           |             | 1         |           | 0 # srai
-- 010110 |         |     W | F          |            | -       | NEXT   | FRT     | -             |           |             | 1         |           | 0 # imovf
-- 010111 |         |     W | F          |            | -       | NEXT   | RT      | -             |           |             | 1         |           | 0 # fmovi
-- # floating
-- # alu control codes are the same as integer, but has different meaning
-- 110000 |       0 |     E | W          | 0          | REG     | CUR    | -       | 000           | 0         | 0           | 0         | 0         | 0 # fmov
-- 110000 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110001 |         |     E | W          |            |         | CUR    | -       | 001           |           |             | 0         |           | 0 # fneg
-- 110001 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110010 |         |     E | E1         |            |         | CUR    | -       | 010           |           |             | 0         |           | 0 # fadd
-- 110010 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110010 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110010 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110011 |         |     E | E1         |            |         | CUR    | -       | 011           |           |             | 0         |           | 0 # fsub
-- 110011 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110011 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110011 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110100 |         |     E | E1         |            |         | CUR    | -       | 100           |           |             | 0         |           | 0 # fmul
-- 110100 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110100 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110100 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110101 |         |     E | E1         |            |         | CUR    | -       | 101           |           |             | 0         |           | 0 # fmuln
-- 110101 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110101 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110101 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110110 |         |     E | E1         |            |         | CUR    | -       | 110           |           |             | 0         |           | 0 # finv
-- 110110 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110110 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110110 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0 
-- 110111 |         |     E | E1         |            |         | CUR    | -       | 111           |           |             | 0         |           | 0 # fsqrt
-- 110111 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110111 |         |    E2 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 110111 |         |     W | F          |            |         | NEXT   | FRD     | -             |           |             | 1         |           | 0
-- # memory read - write
--#op b   | rx_wait | stage | next_stage | bus_to_reg | alu_src | pc_src | reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable | rx_pop
-- 101000 |         |     D | E          | 0          | IMM     | CUR    | RT      | 000           | 0         | 0           | 0         | 0         | 0 # ldi
-- 101000 |         |     E | M          |            | IMM     | CUR    | RT      |               |           |             |           |           | 0 
-- 101000 |         |     M | M1         |            | IMM     | CUR    | RT      |               |           |             |           |           | 0 
-- 101000 |         |    M1 | W          | 0          | IMM     | CUR    | RT      |               |           |             |           |           | 0 
-- 101000 |         |     W | F          | 1          | -       | NEXT   | RT      |               |           |             | 1         |           | 0 
-- 101001 |         |     D | E          | 0          | IMM     | CUR    | -       | 000           | 0         |             | 0         |           | 0 # sti
-- 101001 |         |     E | M          |            | IMM     | CUR    |         |               | 0         |             |           |           | 0 
-- 101001 |         |     M | M1         |            | IMM     | CUR    |         |               | 1         |             |           |           | 0 
-- 101001 |         |    M1 | F          | 0          | IMM     | NEXT   |         |               | 0         |             |           |           | 0 
-- 101100 |         |     D | E          | 0          | REG     | CUR    | RD      | 000           | 0         |             | 0         |           | 0 # ldr
-- 101100 |         |     E | M          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 101100 |         |     M | M1         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 101100 |         |    M1 | W          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 101100 |         |     W | F          | 1          | -       | NEXT   | RD      |               |           |             | 1         |           | 0 
-- 101010 |         |     D | E          | 0          | IMM     | CUR    | FRT     | 000           | 0         |             | 0         |           | 0 # fldi
-- 101010 |         |     E | M          |            | IMM     | CUR    |         |               |           |             |           |           | 0 
-- 101010 |         |     M | M1         |            | IMM     | CUR    |         |               |           |             |           |           | 0 
-- 101010 |         |    M1 | W          |            | IMM     | CUR    |         |               |           |             |           |           | 0 
-- 101010 |         |     W | F          | 1          | -       | NEXT   | FRT     |               |           |             | 1         |           | 0 
-- 101011 |         |     D | E          | 0          | IMM     | CUR    | -       | 000           | 0         |             | 0         |           | 0 # fsti
-- 101011 |         |     E | M          |            | IMM     | CUR    |         |               | 0         |             |           |           | 0 
-- 101011 |         |     M | M1         |            | IMM     | CUR    |         |               | 1         |             |           |           | 0 
-- 101011 |         |    M1 | F          | 0          | IMM     | NEXT   |         |               | 0         |             | 0         |           | 0 
-- 101110 |         |     D | E          | 0          | REG     | CUR    | FRD     | 000           | 0         |             | 0         |           | 0 # fldr
-- 101110 |         |     E | M          |            |         | CUR    |         |               |           |             |           |           | 0 
-- 101110 |         |     M | M1         |            |         | CUR    |         |               |           |             |           |           | 0 
-- 101110 |         |    M1 | W          | 0          |         | CUR    |         |               |           |             |           |           | 0 
-- 101110 |         |     W | F          | 1          | -       | NEXT   | FRD     |               |           |             | 1         |           | 0 
-- # conditional branch - no execute, because next PC is implicitly decided in a clock
--#op b   | rx_wait | stage | next_stage | bus_to_reg | alu_src | pc_src | reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable | rx_pop
-- 100000 |         |     D | W          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # beq
-- 100000 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100001 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # blt
-- 100001 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100010 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # ble
-- 100010 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100011 |         |     D | W          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # beq
-- 100011 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100100 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # fbeq
-- 100100 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100101 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # fblt
-- 100101 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100110 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # fble
-- 100110 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 100111 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # fbeq
-- 100111 |         |     W | F          |            |         | REL    |         |               |           |             |           |           | 0 
-- 111000 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # j
-- 111000 |         |     W | F          |            |         | J      |         |               |           |             |           |           | 0 
-- 111001 |         |     D | W          |            |         | CUR    |         |               |           |             |           |           | 0 # jr
-- 111001 |         |     W | F          |            |         | BREG   |         |               |           |             |           |           | 0 # jr
-- # call - stack works on pc_src != CUR
-- 111010 |         |     D | W          | 0          | -       | CUR    |         |               | 0         | 0           | 0         | 0         | 0 # call
-- 111010 |         |     W | F          |            | -       | J      |         |               |           |             |           |           | 0 # push to stack and jump
-- 111011 |         |     D | W          | 0          | -       | CUR    |         |               |           |             |           |           | 0 # callr
-- 111011 |         |     W | F          |            | -       | BREG   |         |               |           |             |           |           | 0 
-- 111100 |         |     D | W          | 0          | -       | CUR    |         |               | 0         | 0           | 0         | 0         | 0 # return
-- 111100 |         |     W | F          |            | -       | LR     |         |               |           |             |           |           | 0 # pop stack and use the address
-- 111101 |       0 |     D | E          | -          | -       | CUR    | -       | -             | 0         | 0           | 0         | 1         | 0 # inputb
-- 111101 |       0 |     E | W          | -          | -       | CUR    |         | -             |           |             | 0         | 1         | 1
-- 111101 |       0 |     W | F          | 1          | -       | NEXT   | RT      | -             |           |             | 1         | 1         | 0
-- 111101 |       1 |     D | E          | -          |         | CUR    |         |               |           |             | 0         | 1         | 0
-- 111101 |       1 |     E | E          | -          |         | CUR    |         |               |           |             | 0         | 1         | 0
-- 111110 |       0 |     D | E          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # outputb
-- 111110 |       0 |     E | F          | 0          |         | NEXT   | -       | -             |           | 1           |           |           | 0 
-- 111111 |       0 |     F | F          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0         | 0 # halt
-- 111111 |       1 |     F | F          | 0          |         |        |         |               |           |             |           |           | 0  
-- 111111 |       0 |     D | D          | 0          |         |        |         |               |           |             |           |           | 0  
-- 111111 |       0 |     F | F          | 0          |         |        |         |               |           |             |           |           | 0  
-- 111111 |       0 |     M | M          | 0          |         |        |         |               |           |             |           |           | 0  
-- 111111 |       0 |     W | W          | 0          |         |        |         |               |           |             |           |           | 0  
-- /TEST
-- op b   | rx_wait | stage | next_stage | bus_to_reg | alu_src | pc_src | reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable | rx_pop


entity main_decoder is
  
  port (
    op                    : in  STD_LOGIC_VECTOR(5 downto 0);
    rx_wait               : in  STD_LOGIC;
    stage                 : in  std_logic_vector(3 downto 0);
    next_stage            : out std_logic_vector(3 downto 0);
    bus_to_reg            : out STD_LOGIC;
    alu_src               : out STD_LOGIC;
    pc_src                : out std_logic_vector(2 downto 0);
    reg_dst               : out STD_LOGIC;
    alu_control           : out STD_LOGIC_VECTOR(3 downto 0);
    mem_write             : out STD_LOGIC;
    send_enable           : out STD_LOGIC;
    reg_write             : out STD_LOGIC;
    rx_enable             : out STD_LOGIC;
    rx_pop                : out STD_LOGIC
    );

end main_decoder;

architecture behave of main_decoder is

  constant CUR      : std_logic_vector(2 downto 0) := "000";
  constant PC_NEXT  : std_logic_vector(2 downto 0) := "001";
  constant JUMP     : std_logic_vector(2 downto 0) := "010";
  constant RELATIVE : std_logic_vector(2 downto 0) := "011";
  constant REG      : std_logic_vector(2 downto 0) := "100";
  constant LR       : std_logic_vector(2 downto 0) := "101";

  constant RD : STD_LOGIC := '1';
  constant RT : STD_LOGIC := '0';

  signal flags : std_logic_vector(4 downto 0);
  signal op_group, op_id : std_logic_vector(2 downto 0) := "000";

begin  -- behave

  op_group <= op(5 downto 3);
  op_id    <= op(2 downto 0);
  rx_pop   <= '1' when op = "111101" and stage = x"2" and rx_wait = '0' else '0';

  process(op, op_group, op_id, stage, rx_wait)
  begin
    -- reset all output
    flags <= (others => 'X');
    next_stage <= (others => 'X');
    pc_src <=  (others => 'X');
    alu_control <= (others => 'X');
    alu_src <= 'X';
    reg_dst <= 'X';

    pc_src     <= CUR;
    alu_control <= "0000";

    -- HALT
    if op = "111111" then
      next_stage <= stage;
      pc_src     <= CUR;
      flags <= "00000";
    else

    case stage is
      
      when x"0" =>              -- fetch
        next_stage <= x"1";
        flags      <= "00000";

      when x"1" =>              -- decode
        next_stage <= x"2";
        flags      <= "00000";

        case op_group is

          -- Reg x Reg arithmetic
          when "000" =>
            reg_dst     <= '1';
            alu_control <= '0' & op_id;
            alu_src     <= '0';

          -- Reg x Imm arithmetic
          when "001" =>
            alu_src     <= '1';
            reg_dst     <= '0';
            alu_control <= '0' & op_id;

          -- mov
          when "010" =>
            alu_src     <= '1';

          -- conditional branch
          when "100" =>
            next_stage <= x"8"; -- no special operation. goto W stage and update PC

          -- memory operation
          when "101" =>
            if op_id = "100" or op_id = "110" then
              alu_src <= '0';
              reg_dst <= '1';
            else
              alu_src <= '1';
              reg_dst <= '0';
            end if;
            alu_control <= "0000";
            
          -- floating point arithmetic
          when "110" =>
            alu_src     <= '0';
            alu_control <= '0' & op_id;

            -- fmov and fneg takes only one arity
            -- TODO: use frd for fmov and fneg
            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;

          -- jump and special operations
          when "111" =>
            next_stage <= x"8"; -- for jump insts
            case op_id is
              when "101" =>
                next_stage <= x"2";
                reg_dst <= '0';
                flags   <= "00001";
              when "110" =>
                next_stage <= x"2";
                flags <= "00000";
              when others => null;
            end case;

          when others =>
            null;
        end case;

      when x"2" => -- execute

        case op_group is
          when "000" =>
            next_stage  <= x"8";
            alu_src     <= '0';
            reg_dst     <= '1';
            flags       <= "00000";
            alu_control <= '0' & op_id;

          when "001" =>
            next_stage  <= x"8";
            alu_src     <= '1';
            reg_dst     <= '0';
            flags       <= "00000";
            alu_control <= '0' & op_id;

          -- mov
          when "010" =>
            next_stage  <= x"8";
            alu_src     <= '1';
            reg_dst     <= '0';
            flags       <= "00000";
            case op_id is
              when "000" =>
                alu_control <= "1000";
              when "001" =>
                alu_control <= "1001";
              when "010" =>
                alu_control <= "1000";
              when "011" =>
                alu_control <= "1001";
              when "100" => -- sll
                alu_control <= "1010";
              when "101" => -- sra
                alu_control <= "1011";
              -- imovf and fmovi: use add with 0 immediate
              when "110" =>
                alu_control <= "0000";
              when "111" =>
                alu_control <= "0000";
              when others =>
                null;
            end case;

          -- memory operation
          when "101" =>
            if op_id = "100" or op_id = "110" then
              alu_src <= '0';
              reg_dst <= '1';
            else
              alu_src <= '1';
              reg_dst <= '0';
            end if;
            next_stage  <= x"5";
            alu_control <= "0000";
            -- no write yet
            flags       <= "00000";

          -- floating point arithmetic
          when "110" =>
            if op_id = "001" or op_id = "000" then
              next_stage  <= x"8"; -- mov and inv takes 1 clock
            else
              next_stage  <= x"3"; -- execute1
            end if;

            flags <= "00000";
            alu_src     <= '0';

            alu_control <= '0' & op_id;

            -- TODO: fmov, fneg operand will come to frd
            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;

          -- jump and special operations
          when "111" =>
            reg_dst <= '0';
            case op_id is
              when "101" =>
                if rx_wait = '0' then
                  next_stage <= x"8"; -- write result
                  flags      <= "00001";
                else
                  next_stage <= x"2"; -- keep here
                  flags      <= "10001";
                end if;
              when "110" =>
                next_stage <= x"0";
                pc_src     <= PC_NEXT;
                flags      <= "00100";
              when others => null;
            end case;
          when others =>
            null;
        end case;

      when x"3" => -- execute1
        assert op_group = "110" report "only floating point arithmetic is known to use EX1 stage";

        next_stage  <= x"4"; -- execute2
        alu_src     <= '0';
        
        flags       <= "00000";
        alu_control <= '0' & op_id;

        if op(2 downto 1) = "00" then
          reg_dst <= '0';
        else
          reg_dst <= '1';
        end if;

      when x"4" => -- execute2
        assert op_group = "110" report "only floating point arithmetic is known to use EX2 stage";

        next_stage  <= x"8"; -- write back
        alu_src     <= '0';
        
        flags       <= "00000";
        alu_control <= '0' & op_id;

        if op(2 downto 1) = "00" then
          reg_dst <= '0';
        else
          reg_dst <= '1';
        end if;

      when x"5" => -- Memory
        assert op_group = "101" or op_group = "111"
          report "only memory or call / return is known to use memory stage";

        -- memory operation
        -- jump and special operations
        next_stage <= x"6";
        if op_id = "100" or op_id = "110" then
          alu_src <= '0';
          reg_dst <= '1';
        else
          alu_src <= '1';
          reg_dst <= '0';
        end if;

        if op_group = "101" then
          -- lsb of write operation is 1
          flags <= "0" & op(0) & "000";
        else
          flags <= "01000";
        end if;

      when x"6" =>  -- memory 1
        assert op_group = "101" or op_group = "111"
          report "only memory or call / return is known to use memory1 stage";

        case op_group is
          -- memory operation
          when "101" =>
            if op_id = "100" or op_id = "110" then
              alu_src <= '0';
              reg_dst <= '1';
            else
              alu_src <= '1';
              reg_dst <= '0';
            end if;

            -- lsb of write operation is 1
            if op(0) = '0' then
              next_stage <= x"8";
              flags <= "00000";
            else
              -- no write back
              next_stage <= x"0";
              pc_src     <= PC_NEXT;
              flags      <= "01000";
            end if;

          -- jump and special operations
          when "111" =>
            next_stage <= x"8";
            flags      <= "00010";

          when others =>
            null;
        end case;

      when x"8" => -- write
        next_stage <= x"0";
        pc_src     <= PC_NEXT;
        case op_group is

          -- Reg x Reg
          when "000" =>
            reg_dst    <= RD;
            flags      <= "00010";

          -- Reg x Imm
          when "001" =>
            reg_dst    <= RT;
            flags      <= "00010";
            alu_src    <= '1';

          -- move
          when "010" =>
            reg_dst    <= RT;
            flags      <= "00010";
            alu_src    <= '1';

          -- floating point arithmetic
          when "110" =>
            alu_src     <= '0';
            flags       <= "00010";
            reg_dst <= '1';

          -- memory operation
          when "101" =>
            flags      <= "10010";
            if op_id = "100" or op_id = "110" then
              reg_dst    <= '1';
            else
              reg_dst    <= '0';
            end if;

          -- conditional branch
          when "100" =>
            alu_src    <= '1';
            reg_dst    <= '0';
            pc_src     <= RELATIVE;
            flags      <= "00000";

          -- jump and special operations
          when "111" => 
            next_stage <= x"0";
            flags      <= "00000";
            case op_id is
              when "000" =>
                next_stage <= x"0";
                pc_src <= JUMP;
              when "001" =>
                next_stage <= x"0";
                pc_src <= REG;
              when "010" => 
                pc_src <= JUMP;
              when "011" => 
                pc_src <= REG;
              when "100" =>
                next_stage <= x"0";
                flags      <= "00000";
                pc_src     <= LR;
              when "101" => 
                reg_dst    <= '0';
                flags      <= "10011";
              when others => null; 
            end case;
            when others => null;
        end case;

      when others =>
        null;
    end case;
    end if;
  end process;

  bus_to_reg  <= flags(4);
  mem_write   <= '1' when flags(3) = '1' and stage = x"5" else '0';
  send_enable <= flags(2);
  reg_write   <= flags(1);
  rx_enable   <= flags(0);

end behave;
