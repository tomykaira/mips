library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- alu_src: 0: register  1: immediate
-- branch: 1: take branch (+ immediate)
-- reg_dst: 0: 20 to 16  1: 15 to 11
-- reg_write: write enable
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
-- op b   | rx_done | stage | next_stage | bus_to_reg | alu_src |pc_src b| reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable
-- 000000 |       0 |     F | D          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0       # fetch
-- 000000 |       0 |     D | E          |            | REG     | CUR    | -       | 000           | 0         | 0           | 0         | 0       # add
-- 000000 |       0 |     E | W          |            | -       | CUR    | RD      | -             |           |             | 1         |         # add
-- 000000 |       0 |     W | F          |            | -       | NEXT   | -       | -             |           |             | 0         |         # add
-- 000001 |       0 |     D | E          |            | REG     | CUR    | -       | 001           |           |             | 0         |         # sub
-- 000010 |       0 |     D | E          |            |         |        |         | 010           |           |             |           |         # mul
-- 000011 |       0 |     D | E          |            |         |        |         | 011           |           |             |           |         # and
-- 000100 |       0 |     D | E          |            |         |        |         | 100           |           |             |           |         # or
-- 000101 |       0 |     D | E          |            |         |        |         | 101           |           |             |           |         # nor
-- 000110 |       0 |     D | E          |            |         |        |         | 110           |           |             |           |         # xor
-- 010000 |       0 |     D | E          | 0          | IMM     | CUR    | -       | 1000          | 0         | 0           | 0         | 0       # mvlo
-- 010000 |       0 |     E | W          |            | -       | CUR    | RT      | -             | 0         | 0           | 1         | 0       # mvlo
-- 010000 |       0 |     W | F          |            | -       | NEXT   | -       | -             | 0         | 0           | 0         | 0       # mvlo
-- 010001 |       0 |     D | E          |            | IMM     | CUR    | -       | 1001          | 0         | 0           | 0         | 0       # mvhi
-- 001000 |       0 |     D | E          |            |         |        |         | 000           |           |             |           |         # addi
-- 001000 |       0 |     E | W          |            | -       |        | RT      | -             | 0         | 0           | 1         | 0       
-- 001001 |       0 |     D | E          |            | IMM     |        | -       | 001           |           |             | 0         |         # subi
-- 001010 |       0 |     D | E          |            |         |        |         | 010           |           |             |           |         # muli
-- 001011 |       0 |     D | E          |            |         |        |         | 011           |           |             |           |         # andi
-- 001100 |       0 |     D | E          |            |         |        |         | 100           |           |             |           |         # ori
-- 001101 |       0 |     D | E          |            |         |        |         | 101           |           |             |           |         # nori
-- 001110 |       0 |     D | E          |            |         |        |         | 110           |           |             |           |         # xori
-- 000111 |       0 |     D | E          |            |         |        |         | 1010          |           |             |           |         # slli
-- 001111 |       0 |     D | E          |            |         |        |         | 1011          |           |             |           |         # srai
-- # alu control codes are the same as integer, but has different meaning
-- 110000 |       0 |     D | E          | 0          | REG     | CUR    | -       | 000           | 0         | 0           | 0         | 0       # fmov
-- 110000 |         |     E | W          |            |         | CUR    | FRT     | 000           |           |             | 1         |         
-- 110001 |         |     D | E          |            |         | CUR    | FRT     | 001           |           |             | 0         |         # fneg
-- 110001 |         |     E | W          |            |         | CUR    | FRT     | 001           |           |             | 1         |         
-- 110010 |         |     D | E          |            |         | CUR    | FRD     | 010           |           |             | 0         |         # fadd
-- 110010 |         |     E | E1         |            |         | CUR    |         |               |           |             |           |         
-- 110010 |         |    E1 | E2         |            |         | CUR    |         |               |           |             |           |         
-- 110010 |         |    E2 | W          |            |         | CUR    |         |               |           |             | 1         |         
-- 110011 |         |     D | E          |            |         | CUR    | FRD     | 011           |           |             | 0         |         # fsub
-- 110011 |         |     E | E1         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110011 |         |    E1 | E2         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110011 |         |    E2 | W          |            |         | CUR    | FRD     |               |           |             | 1         |         
-- 110100 |         |     D | E          |            |         | CUR    | FRD     | 100           |           |             | 0         |         # fmul
-- 110100 |         |     E | E1         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110100 |         |    E1 | E2         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110100 |         |    E2 | W          |            |         | CUR    | FRD     |               |           |             | 1         |         
-- 110101 |         |     D | E          |            |         | CUR    | FRD     | 101           |           |             | 0         |         # fmuln
-- 110101 |         |     E | E1         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110101 |         |    E1 | E2         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110101 |         |    E2 | W          |            |         | CUR    | FRD     |               |           |             | 1         |         
-- 110110 |         |     D | E          |            |         | CUR    | FRD     | 110           |           |             | 0         |         # fdiv
-- 110110 |         |     E | E1         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110110 |         |    E1 | E2         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110110 |         |    E2 | W          |            |         | CUR    | FRD     |               |           |             | 1         |         
-- 110111 |         |     D | E          |            |         | CUR    | FRD     | 111           |           |             | 0         |         # fsqrt
-- 110111 |         |     E | E1         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110111 |         |    E1 | E2         |            |         | CUR    | FRD     |               |           |             |           |         
-- 110111 |         |    E2 | W          |            |         | CUR    | FRD     |               |           |             | 1         |         
-- 010010 |       0 |     D | E          | 0          | IMM     | CUR    | -       | 1000          | 0         | 0           | 0         | 0       # fmvlo
-- 010010 |       0 |     E | W          |            | -       | CUR    | FRT     | -             | 0         | 0           | 1         | 0       # fmvlo
-- 010011 |       0 |     D | E          | 0          | IMM     | CUR    | -       | 1001          | 0         | 0           | 0         | 0       # fmvhi
-- 010110 |         |     D | W          |            | -       | CUR    | FRT     | 1100          |           |             | 1         |         # imovf
-- 010111 |         |     D | W          |            | -       | CUR    | RT      | 1101          |           |             | 1         |         # fmovi
--#op b   | rx_done | stage | next_stage | bus_to_reg | alu_src | pc_src | reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable
-- 101000 |         |     D | E          | 0          | IMM     | CUR    | RT      | 000           | 0         | 0           | 0         | 0       # ldi
-- 101000 |         |     E | M          |            | IMM     | CUR    | RT      | -             |           |             |           | 
-- 101000 |         |     M | M1         |            | IMM     | CUR    | RT      | -             |           |             |           | 
-- 101000 |         |    M1 | M2         |            | IMM     | CUR    | RT      | -             |           |             |           | 
-- 101000 |         |    M2 | W          | 1          | IMM     | CUR    | RT      | -             |           |             | 1         | 
-- 101000 |         |     W | F          | 0          | IMM     | NEXT   | RT      | -             |           |             | 0         |         
-- 101001 |         |     D | E          | 0          | IMM     | CUR    | -       | 000           | 0         |             | 0         | 0       # sti
-- 101001 |         |     E | M          |            | IMM     | CUR    |         | -             | 1         |             |           | 
-- 101001 |         |     M | M1         |            | IMM     | CUR    |         | -             | 1         |             |           | 
-- 101001 |         |    M1 | M2         |            | IMM     | CUR    |         | -             | 1         |             |           | 
-- 101001 |         |    M2 | F          | 0          | IMM     | NEXT   |         | -             | 1         |             | 0         | 
-- 101100 # ldr
-- 101101 # str
-- 101010 |         |     D | E          | 0          | IMM     | CUR    | FRT     | 000           | 0         | 0           | 0         | 0        # fldi
-- 101010 |         |     E | M          |            | IMM     | CUR    | FRT     | -             |           |             |           |         
-- 101010 |         |     M | M1         |            | IMM     | CUR    | FRT     | -             |           |             |           |         
-- 101010 |         |    M1 | M2         |            | IMM     | CUR    | FRT     | -             |           |             |           |         
-- 101010 |         |    M2 | W          | 1          | IMM     | CUR    | FRT     | -             |           |             | 1         |         
-- 101010 |         |     W | F          | 0          | IMM     | NEXT   | FRT     | -             |           |             | 0         |         
-- 101011 |         |     D | E          | 0          | IMM     | CUR    | -       | 000           | 0         |             | 0         | 0       # fsti
-- 101011 |         |     E | M          |            | IMM     | CUR    |         | -             | 1         |             |           |         
-- 101011 |         |     M | M1         |            | IMM     | CUR    |         | -             | 1         |             |           |         
-- 101011 |         |    M1 | M2         |            | IMM     | CUR    |         | -             | 1         |             |           |         
-- 101011 |         |    M2 | F          | 0          | IMM     | NEXT   |         | -             | 1         |             | 0         |         
-- 101110 # fldr
-- 101111 # fstr
-- 100000 |         |     D | E          | 0          | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # beq
-- 100000 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 100001 |         |     D | E          |            | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # blt
-- 100001 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 100010 |         |     D | E          |            | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # ble
-- 100010 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 100100 |         |     D | E          | 0          | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # fbeq
-- 100100 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 100101 |         |     D | E          |            | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # fblt
-- 100101 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 100110 |         |     D | E          |            | IMM     | CUR    |         | -             | 0         | 0           | 0         | 0       # fble
-- 100110 |         |     E | F          |            |         | REL    |         | -             |           |             |           |         
-- 111000 |         |     D | F          |            |         | J      |         | -             | 0         | 0           | 0         | 0       # j
-- 111001 |         |     D | F          |            |         | BREG   |         | -             | 0         | 0           | 0         | 0       # jr
-- 111010 |         |     D | M          | 0          | -       | CUR    |         |               | 1         | 0           | 0         | 0       # call
-- 111010 |         |     M | M1         |            | -       | CUR    |         |               | 1         |             | 0         |         # RAM[frame pointer] <- link register;
-- 111010 |         |    M1 | M2         |            | -       | CUR    |         |               | 1         |             | 0         |         #
-- 111010 |         |    M2 | W          |            | -       | CUR    |         |               | 0         |             | 1         |         # frame pointer --
-- 111010 |         |     W | W1         |            | -       | CUR    |         |               | 0         |             | 1         |         # link_register <- PC
-- 111010 |         |    W1 | F          |            | -       | J      |         |               | 0         |             | 0         |         # link_register <- PC
-- 111011 |         |     D | M          | 0          | -       | CUR    |         |               | 1         | 0           | 0         | 0       # callr
-- 111011 |         |     M | M1         |            | -       | CUR    |         |               | 1         |             | 0         |         
-- 111011 |         |    M1 | M2         |            | -       | CUR    |         |               | 1         |             | 0         |         
-- 111011 |         |    M2 | W          |            | -       | CUR    |         |               | 0         |             | 1         |         
-- 111011 |         |     W | W1         |            | -       | CUR    |         |               | 0         |             | 1         |         
-- 111011 |         |    W1 | F          |            | -       | BREG   |         |               | 0         |             | 0         |         
-- 111100 |         |     D | M          | 0          | -       | CUR    |         |               | 1         | 0           | 0         | 0        # return; RAM[frame pointer] <- link register
-- 111100 |         |     M | M1         |            | -       | CUR    |         |               | 1         |             | 0         |         
-- 111100 |         |    M1 | M2         |            | -       | CUR    |         |               | 1         |             | 0         |         
-- 111100 |         |    M2 | W          |            | -       | CUR    |         |               | 0         |             | 1         |          # frame pointer++
-- 111100 |         |     W | F          |            | -       | LR     |         |               | 0         |             | 0         |          # goto link register
-- 111101 |       0 |     D | E          | 0          | -       | CUR    | RT      | -             | 0         | 0           | 0         | 1        # inputb
-- 111101 |       0 |     E | E          | 1          |         | CUR    |         |               |           |             | 0         | 1        
-- 111101 |       1 |     E | W          | 1          |         | CUR    |         |               |           |             | 1         | 0        
-- 111101 |       0 |     W | F          | 0          |         | NEXT   |         |               |           |             | 0         | 0        
-- 111110 |       0 |     D | E          | 0          | -       | CUR    | -       | -             | 0         | 1           | 0         | 0        # outputb
-- 111110 |       0 |     E | F          | 0          |         | NEXT   | -       | -             |           | 0           |           |          
-- 111111 |       0 |     F | F          | 0          | -       | CUR    | -       | -             | 0         | 0           | 0         | 0        # halt
-- 111111 |       1 |     F | F          | 0          |         |        |         |               |           | 0           |           |          
-- 111111 |       0 |     D | D          | 0          |         |        |         |               |           | 0           |           |          
-- 111111 |       0 |     F | F          | 0          |         |        |         |               |           | 0           |           |          
-- 111111 |       0 |     M | M          | 0          |         |        |         |               |           | 0           |           |          
-- 111111 |       0 |     W | W          | 0          |         |        |         |               |           | 0           |           |          
-- /TEST
-- op b   | rx_done | stage | next_stage | bus_to_reg | alu_src | pc_src | reg_dst | alu_control b | mem_write | send_enable | reg_write | rx_enable


entity main_decoder is
  
  port (
    op                    : in  STD_LOGIC_VECTOR(5 downto 0);
    rx_done               : in  STD_LOGIC;
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
    rx_enable             : out STD_LOGIC
    );

end main_decoder;

architecture behave of main_decoder is

  signal controls : std_logic_vector(11 downto 0);
  signal flags : std_logic_vector(4 downto 0);

begin  -- behave

  process(op, stage, rx_done)
  begin
    if op = "111111" then
      next_stage <= stage;
      pc_src <= "000";
    else

    case stage is
      
      when "0000" =>              -- fetch
        next_stage <= "0001";
        pc_src <= "000";
        flags <= "00000";

      when "0001" =>              -- decode
        next_stage <= x"2";
        pc_src      <= "000"; -- cur
        flags       <= "00000";

        case op(5 downto 3) is

          -- Reg x Reg arithmetic
          when "000" =>
            -- special: SLLi
            reg_dst     <= '1';
            if op(2 downto 0) = "111" then
              alu_control <= "1010";
              alu_src     <= '1';
            else
              alu_control <= '0' & op(2 downto 0);
              alu_src     <= '0';
            end if; 

          -- Reg x Imm arithmetic
          when "001" =>
            alu_src     <= '1';
            reg_dst     <= '0';
            -- special: SRAi
            if op(2 downto 0) = "111" then
              alu_control <= "1011";
            else
              alu_control <= '0' & op(2 downto 0);
            end if;

          -- mov
          when "010" =>
            alu_src     <= '1';
            case op(2 downto 0) is
              when "000" =>
                alu_control <= "1000";
              when "001" =>
                alu_control <= "1001";
              when "010" =>
                alu_control <= "1000";
              when "011" =>
                alu_control <= "1001";

              -- imovf and fmovi does not need any instruction
              when "110" =>
                
                next_stage <= x"8";
                flags <= "00010";
                alu_control <= "1100";
              when "111" =>
                next_stage <= x"8";
                flags <= "00010";
                alu_control <= "1101";
              when others =>
                null;
            end case;

          -- conditional branch
          when "100" =>
            next_stage <= x"2";

          -- memory operation
          when "101" =>
            alu_src <= '1';
            reg_dst <= '0';
            alu_control <= "0000";
            flags <= "00000";
            
          -- floating point arithmetic
          when "110" =>
            alu_src     <= '0';
            alu_control <= '0' & op(2 downto 0);

            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;

          -- jump and special operations
          when "111" =>
            case op(2 downto 0) is
              when "000" =>
                next_stage <= x"0";
                pc_src <= "010";
              when "001" =>
                next_stage <= x"0";
                pc_src <= "100";
              when "010" =>
                next_stage <= x"5";
                flags <= "01000";
              when "011" =>
                next_stage <= x"5";
                flags <= "01000";
              when "100" =>
                next_stage <= x"5";
                flags <= "01000";
              when "101" =>
                reg_dst <= '0';
                flags   <= "00001";
              when "110" =>
                next_stage <= x"2";
                flags <= "00100";
              when others => null;
            end case;

          when others =>
            null;
        end case;

      when x"2" => -- execute
        pc_src      <= "000"; -- cur

        case op(5 downto 3) is
          when "000" =>
            next_stage  <= x"8";
            alu_src     <= '0';
            reg_dst     <= '1';
            flags       <= "00010";
            alu_control <= '0' & op(2 downto 0);

          when "001" =>
            next_stage  <= x"8";
            alu_src     <= '1';
            reg_dst     <= '0';
            flags       <= "00010";
            alu_control <= '0' & op(2 downto 0);

          -- mov
          when "010" =>
            next_stage  <= x"8";
            alu_src     <= '1';
            reg_dst     <= '0';
            flags       <= "00010";
            case op(2 downto 0) is
              when "000" =>
                alu_control <= "1000";
              when "001" =>
                alu_control <= "1001";
              when "010" =>
                alu_control <= "1000";
              when "011" =>
                alu_control <= "1001";
              when others =>
                null;
            end case;

          -- conditional branch
          when "100" =>
            next_stage <= x"0";
            pc_src <= "011"; -- relative
            
          -- memory operation
          when "101" =>
            next_stage <= x"5";
            alu_src <= '1';
            reg_dst <= '0';
            alu_control <= "0000";
            -- lsb of write operation is 1
            flags <= "0" & op(0) & "000";

          -- floating point arithmetic
          when "110" =>
            if op(2 downto 0) = "001" or op(2 downto 0) = "000" then
              next_stage  <= x"8"; -- mov and inv takes 1 clock
              flags <= "00010";
            else
              next_stage  <= x"3"; -- execute1
              flags       <= "00000";
            end if;

            alu_src     <= '0';
            pc_src      <= "000"; -- cur

            alu_control <= '0' & op(2 downto 0);

            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;

          -- jump and special operations
          when "111" =>
            case op(2 downto 0) is
              when "101" =>
                if rx_done = '1' then
                  next_stage <= x"8"; -- write result
                  reg_dst <= '0';
                  flags   <= "10010";
                else
                  next_stage <= x"2"; -- keep here
                  reg_dst <= '0';
                  flags   <= "10001";
                end if;
              when "110" =>
                next_stage <= x"0";
                pc_src <= "001";
                flags <= "00000";
              when others => null;
            end case;
          when others =>
            null;
        end case;

      when x"3" => -- execute1
        case op(5 downto 3) is
          -- floating point arithmetic
          when "110" =>
            next_stage  <= x"4"; -- execute2
            alu_src     <= '0';
            pc_src      <= "000"; -- cur
            
            flags       <= "00000";
            alu_control <= '0' & op(2 downto 0);

            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;
          when others =>
            null;
        end case;

      when x"4" => -- execute2
        case op(5 downto 3) is
          -- floating point arithmetic
          when "110" =>
            next_stage  <= x"8"; -- write back
            alu_src     <= '0';
            pc_src      <= "000"; -- cur
            
            flags       <= "00010";
            alu_control <= '0' & op(2 downto 0);

            if op(2 downto 1) = "00" then
              reg_dst <= '0';
            else
              reg_dst <= '1';
            end if;
          when others =>
            null;
        end case;

      when x"5" => -- Memory

        case op(5 downto 3) is
          -- memory operation
          when "101" =>
            next_stage <= x"6";
            alu_src <= '1';
            reg_dst <= '0';
            -- lsb of write operation is 1
            flags <= "0" & op(0) & "000";

          -- jump and special operations
          when "111" =>
            next_stage <= x"6";
            flags <= "01000";

          when others =>
            null;
        end case;

      when x"6" =>  -- memory 1
        
        case op(5 downto 3) is
          -- memory operation
          when "101" =>
            next_stage <= x"7";
            alu_src <= '1';
            reg_dst <= '0';
            -- lsb of write operation is 1
            flags <= "0" & op(0) & "000";

          -- jump and special operations
          when "111" =>
            next_stage <= x"7";
            flags <= "01000";

          when others =>
            null;
        end case;

      when x"7" =>  -- memory 2
        
        case op(5 downto 3) is
          -- memory operation
          when "101" =>
            alu_src <= '1';
            reg_dst <= '0';

            -- lsb of write operation is 1
            if op(0) = '0' then
              next_stage <= x"8";
              flags <= "10010";
            else
              -- no write back
              next_stage <= x"0";
              pc_src     <= "001";
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
        pc_src <= "001";
        if op = "111101" then
          next_stage <= x"0"; -- keep here
          reg_dst    <= '0';
          flags      <= "00000";
        elsif op = "111010" or op = "111011" then
          next_stage <= x"9";
          flags      <= "00010";
          pc_src     <= "000";
        elsif op = "111100" then
          next_stage <= x"0";
          flags      <= "00000";
          pc_src     <= "101";
        else
          next_stage <= x"0"; -- write back
          pc_src     <= "001";
          flags      <= "00000";
        end if;

      when x"9" => -- write2
        next_stage <= x"0";
        if op = "111010" then
          pc_src     <= "010";
        else
          pc_src     <= "100";
        end if;
        flags      <= "00000";

      when others =>
        null;
    end case;
    end if;
  end process;

  bus_to_reg  <= flags(4);
  mem_write   <= flags(3);
  send_enable <= flags(2);
  reg_write   <= flags(1);
  rx_enable   <= flags(0);

end behave;
