-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): jmeno <login AT stud.fit.vutbr.cz>
--
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
ENTITY cpu IS
  PORT (
    CLK : IN STD_LOGIC; -- hodinovy signal
    RESET : IN STD_LOGIC; -- asynchronni reset procesoru
    EN : IN STD_LOGIC; -- povoleni cinnosti procesoru

    -- synchronni pamet RAM
    DATA_ADDR : OUT STD_LOGIC_VECTOR(12 DOWNTO 0); -- adresa do pameti
    DATA_WDATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
    DATA_RDATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
    DATA_RDWR : OUT STD_LOGIC; -- cteni (0) / zapis (1)
    DATA_EN : OUT STD_LOGIC; -- povoleni cinnosti

    -- vstupni port
    IN_DATA : IN STD_LOGIC_VECTOR(7 DOWNTO 0); -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
    IN_VLD : IN STD_LOGIC; -- data platna
    IN_REQ : OUT STD_LOGIC; -- pozadavek na vstup data

    -- vystupni port
    OUT_DATA : OUT STD_LOGIC_VECTOR(7 DOWNTO 0); -- zapisovana data
    OUT_BUSY : IN STD_LOGIC; -- LCD je zaneprazdnen (1), nelze zapisovat
    OUT_WE : OUT STD_LOGIC; -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

    -- stavove signaly
    READY : OUT STD_LOGIC; -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
    DONE : OUT STD_LOGIC -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
  );
END cpu;
-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
ARCHITECTURE behavioral OF cpu IS
  --Counter 
  SIGNAL CNT : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL CNT_INC : STD_LOGIC;
  SIGNAL CNT_DEC : STD_LOGIC;
  SIGNAL CNT_ONE : STD_LOGIC;
  SIGNAL CNT_IS_ZERO : STD_LOGIC;

  --Program counter
  SIGNAL PC : STD_LOGIC_VECTOR(12 DOWNTO 0);
  SIGNAL PC_INC : STD_LOGIC;
  SIGNAL PC_DEC : STD_LOGIC;
  --Pointer
  SIGNAL PTR : STD_LOGIC_VECTOR(12 DOWNTO 0);
  SIGNAL PTR_INC : STD_LOGIC;
  SIGNAL PTR_DEC : STD_LOGIC;

  --MULTIPLEXERS
  SIGNAL MX1_SEL : STD_LOGIC;
  SIGNAL MX2_SEL : STD_LOGIC_VECTOR(1 DOWNTO 0);

  TYPE FSM_STATES IS (
    idle,
    init,
    init_cycle,
    fetch,
    decode,

    exec_none,
    exec_inc_ptr, -- >
    exec_dec_ptr, -- <

    exec_inc_val, -- +
    exec_inc_val_write,

    exec_dec_val, -- -
    exec_dec_val_write,

    exec_while, -- [
    exec_while_compare,
    exec_while_read_instr,
    exec_while_update_cnt,
    exec_while_is_cnt_zero,

    exec_while_end, -- ]
    exec_while_end_compare,
    exec_while_end_read_instr,
    exec_while_end_update_cnt,
    exec_while_end_is_cnt_zero,

    exec_while_break, -- ~
    exec_while_break_cycle,
    exec_while_break_compare,
    exec_while_break_is_cnt_zero,
    exec_print, -- .
    exec_print_wait,
    exec_load, -- ,
    exec_load_write,
    exec_done -- @

  );
  SIGNAL P_STATE : FSM_STATES := idle;
  SIGNAL N_STATE : FSM_STATES;

BEGIN

  -- PTR memory pointer
  PTR_PROCESS : PROCESS (CLK, RESET)
  BEGIN
    IF (RESET = '1') THEN
      PTR <= (OTHERS => '0');
    ELSIF (rising_edge(CLK)) THEN
      IF (PTR_INC = '1') THEN
        PTR <= PTR + 1;
      ELSIF (PTR_DEC = '1') THEN
        PTR <= PTR - 1;
      END IF;
    END IF;
  END PROCESS;

  -- PC program counter 
  PC_PROCESS : PROCESS (CLK, RESET)
  BEGIN
    IF (RESET = '1') THEN
      PC <= (OTHERS => '0');
    ELSIF (rising_edge(CLK)) THEN
      IF (PC_INC = '1') THEN
        PC <= PC + 1;
      ELSIF (PC_DEC = '1') THEN
        PC <= PC - 1;
      END IF;
    END IF;
  END PROCESS;

  -- CNT loop counter
  CNT_PROCESS : PROCESS (CLK, RESET)
  BEGIN
    IF (RESET = '1') THEN
      CNT <= (OTHERS => '0');
    ELSIF (rising_edge(CLK)) THEN
      IF (CNT_DEC = '1') THEN
        CNT <= CNT - 1;
      ELSIF (CNT_INC = '1') THEN
        CNT <= CNT + 1;
      ELSIF (CNT_ONE = '1') THEN
        CNT <= X"01";
      END IF;
    END IF;
  END PROCESS;

  -- is cnt zero
  CNT_IS_ZERO_PROCESS : PROCESS (CNT)
  BEGIN
    IF (CNT = X"00") THEN
      CNT_IS_ZERO <= '1';
    ELSE
      CNT_IS_ZERO <= '0';
    END IF;
  END PROCESS;

  -- MX1 
  MX1 : PROCESS (PC, PTR, MX1_SEL)
  BEGIN
    CASE MX1_SEL IS
      WHEN '0' => DATA_ADDR <= PC;
      WHEN '1' => DATA_ADDR <= PTR;
      WHEN OTHERS => NULL;
    END CASE;
  END PROCESS;

  -- MX2 
  MX2 : PROCESS (IN_DATA, DATA_RDATA, MX2_SEL)
  BEGIN
    CASE MX2_SEL IS
      WHEN "00" => DATA_WDATA <= IN_DATA;
      WHEN "01" => DATA_WDATA <= DATA_RDATA - 1;
      WHEN "10" => DATA_WDATA <= DATA_RDATA + 1;
      WHEN OTHERS => NULL;
    END CASE;
  END PROCESS;

  -- P_STATE logic
  FSM_P_STATE : PROCESS (CLK, RESET)
  BEGIN
    IF (RESET = '1') THEN
      P_STATE <= idle;
    ELSIF (rising_edge(CLK)) THEN
      P_STATE <= N_STATE;
    END IF;
  END PROCESS;

  FSM_N_STATE : PROCESS (P_STATE, CNT_IS_ZERO, IN_VLD, OUT_BUSY, DATA_RDATA, EN)
  BEGIN

    DATA_EN <= '0';
    DATA_RDWR <= '0';
    IN_REQ <= '0';
    OUT_WE <= '0';
    OUT_DATA <= X"00";

    PTR_INC <= '0';
    PTR_DEC <= '0';

    CNT_INC <= '0';
    CNT_DEC <= '0';
    CNT_ONE <= '0';

    PC_INC <= '0';
    PC_DEC <= '0';
    MX1_SEL <= '0';
    MX2_SEL <= "00";
    DONE <= '0';

    CASE P_STATE IS
        -- IDLE
      WHEN idle =>
        IF (EN = '0') THEN
          READY <= '0';
          N_STATE <= idle;
        ELSE
          N_STATE <= init;
        END IF;
        -- INIT
        -- set ptr to x+1 (mem[x]=@)
      WHEN init =>
        PTR_INC <= '1'; -- post-icrement pointer
        -- read data from PTR
        MX1_SEL <= '1'; -- data
        DATA_RDWR <= '0'; -- read
        DATA_EN <= '1'; -- enable memory
        N_STATE <= init_cycle;
        -- inc ptr until @ is found
      WHEN init_cycle =>
        -- compare data at PTR equals @
        IF (DATA_RDATA = X"40") THEN
          READY <= '1';
          N_STATE <= fetch;
        ELSE
          N_STATE <= init;
        END IF;
        -- FETCH
      WHEN fetch =>
        N_STATE <= decode;
        DATA_RDWR <= '0'; -- read
        DATA_EN <= '1'; -- enable memory
        MX1_SEL <= '0'; -- program
        -- DECODE
      WHEN decode =>
        CASE DATA_RDATA IS
          WHEN X"3E" =>
            N_STATE <= exec_inc_ptr;
          WHEN X"3C" =>
            N_STATE <= exec_dec_ptr;
          WHEN X"2B" =>
            N_STATE <= exec_inc_val;
          WHEN X"2D" =>
            N_STATE <= exec_dec_val;
          WHEN X"2E" =>
            N_STATE <= exec_print;
          WHEN X"2C" =>
            N_STATE <= exec_load;
          WHEN X"40" =>
            N_STATE <= exec_done;
          WHEN X"5B" =>
            N_STATE <= exec_while;
          WHEN X"5D" =>
            N_STATE <= exec_while_end;
          WHEN X"7E" =>
            N_STATE <= exec_while_break;
          WHEN OTHERS =>
            N_STATE <= exec_none;
        END CASE;

        -- NONE
      WHEN exec_none =>
        PC_INC <= '1';
        N_STATE <= fetch;

        -- DONE
        -- found @ signal done
      WHEN exec_done =>
        DONE <= '1';
        N_STATE <= exec_done; -- TODO:check done or fetch idk

        -- INC PTR
        -- increment pointer to memory
      WHEN exec_inc_ptr =>
        PC_INC <= '1';
        PTR_INC <= '1';
        N_STATE <= fetch;

        -- DEC PTR
        -- decrement pointer to memory
      WHEN exec_dec_ptr =>
        PC_INC <= '1';
        PTR_DEC <= '1';
        N_STATE <= fetch;

        -- INC VAL
        --  Read data from mem[ptr]
      WHEN exec_inc_val =>
        PC_INC <= '1';
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read 
        MX1_SEL <= '1'; --  data
        N_STATE <= exec_inc_val_write;
        -- Write data+1 to mem[ptr] 
      WHEN exec_inc_val_write =>
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '1'; -- write
        MX1_SEL <= '1'; -- data
        MX2_SEL <= "10"; -- increment RDATA
        N_STATE <= fetch;

        --  DEC VAL
        --  Read data from mem[ptr]
      WHEN exec_dec_val =>
        PC_INC <= '1';
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read 
        MX1_SEL <= '1'; --  data
        N_STATE <= exec_dec_val_write;
        -- Write data-1 to mem[ptr] 
      WHEN exec_dec_val_write =>
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '1'; -- write
        MX1_SEL <= '1'; -- data
        MX2_SEL <= "01"; -- decrement RDATA
        N_STATE <= fetch;

        --  WHILE 
        --  Read data from mem[ptr]
      WHEN exec_while =>
        PC_INC <= '1';
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read memory
        MX1_SEL <= '1'; --  data
        N_STATE <= exec_while_compare;
        --  Compare data eq 0
        -- if false just fetch
        -- else set cnt to 1
      WHEN exec_while_compare =>
        IF (DATA_RDATA = X"00") THEN
          CNT_ONE <= '1'; -- set cnt to one
          N_STATE <= exec_while_read_instr;
        ELSE
          N_STATE <= fetch;
        END IF;
        -- read next instruction
      WHEN exec_while_read_instr =>
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read
        MX1_SEL <= '0'; -- program
        N_STATE <= exec_while_update_cnt;

      WHEN exec_while_update_cnt =>
        IF (DATA_RDATA = X"5B") THEN
          CNT_INC <= '1';
        ELSIF (DATA_RDATA = X"5D") THEN
          CNT_DEC <= '1';
        END IF;
        PC_INC <= '1';
        N_STATE <= exec_while_is_cnt_zero;

      WHEN exec_while_is_cnt_zero =>
        IF (CNT_IS_ZERO = '1') THEN
          N_STATE <= fetch;
        ELSE
          N_STATE <= exec_while_read_instr;
        END IF;

        --  WHILE END
        --  Read data from mem[ptr]
      WHEN exec_while_end =>
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read memory
        MX1_SEL <= '1'; --  data
        N_STATE <= exec_while_end_compare;
        --  Compare data eq 0
        -- if true just fetch and inc pc
        -- else set cnt to , decrement pc
      WHEN exec_while_end_compare =>
        IF (DATA_RDATA = X"00") THEN
          PC_INC <= '1';
          N_STATE <= fetch;
        ELSE
          CNT_ONE <= '1';
          PC_DEC <= '1';
          N_STATE <= exec_while_end_read_instr;
        END IF;
        -- read next instruction
      WHEN exec_while_end_read_instr =>
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read
        MX1_SEL <= '0'; -- program
        N_STATE <= exec_while_end_update_cnt;

      WHEN exec_while_end_update_cnt =>
        IF (DATA_RDATA = X"5D") THEN
          CNT_INC <= '1';
        ELSIF (DATA_RDATA = X"5B") THEN
          CNT_DEC <= '1';
        END IF;
        N_STATE <= exec_while_end_is_cnt_zero;

      WHEN exec_while_end_is_cnt_zero =>
        IF (CNT_IS_ZERO = '1') THEN
          PC_INC <= '1';
          N_STATE <= fetch;
        ELSE
          PC_DEC <= '1';
          N_STATE <= exec_while_end_read_instr;
        END IF;

      WHEN exec_print =>
        PC_INC <= '1';
        DATA_EN <= '1'; -- enable memory 
        DATA_RDWR <= '0'; -- read
        MX1_SEL <= '1'; -- data
        N_STATE <= exec_print_wait;
      WHEN exec_print_wait =>
        IF (OUT_BUSY = '0') THEN
          OUT_WE <= '1';
          OUT_DATA <= DATA_RDATA;
          N_STATE <= fetch;
        ELSE
          N_STATE <= exec_print_wait;
        END IF;

      WHEN exec_load =>
        IN_REQ <= '1';
        IF (IN_VLD = '1') THEN
          N_STATE <= exec_load_write;
        ELSE
          N_STATE <= exec_load;
        END IF;
      WHEN exec_load_write =>
        PC_INC <= '1';
        DATA_EN <= '1'; -- enable memory
        DATA_RDWR <= '1'; -- write
        MX1_SEL <= '1'; -- data
        MX2_SEL <= "00"; -- INDATA
        N_STATE <= fetch;

        -- BREAK
        -- set ptr to x+1 (mem[x]=@)
      WHEN exec_while_break =>
        CNT_ONE <= '1';
        N_STATE <= exec_while_break_cycle;
      WHEN exec_while_break_cycle =>
        PC_INC <= '1'; -- post-icrement pointer
        -- read data from PTR
        MX1_SEL <= '0'; -- data
        DATA_RDWR <= '0'; -- read
        DATA_EN <= '1'; -- enable memory
        N_STATE <= exec_while_break_compare;
        -- inc ptr until @ is found
      WHEN exec_while_break_compare =>
        -- compare data at PTR equals @
        IF (DATA_RDATA = X"5B") THEN
          CNT_INC <= '1';
        ELSIF (DATA_RDATA = X"5D") THEN
          CNT_DEC <= '1';
        END IF;
        N_STATE <= exec_while_break_is_cnt_zero;
      WHEN exec_while_break_is_cnt_zero =>
        IF (CNT_IS_ZERO = '1') THEN
          N_STATE <= fetch;
        ELSE
          N_STATE <= exec_while_break_cycle;
        END IF;
    END CASE;
  END PROCESS;
END behavioral;