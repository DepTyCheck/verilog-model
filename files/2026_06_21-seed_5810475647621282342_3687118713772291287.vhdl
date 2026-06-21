-- Seed: 5810475647621282342,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity bad is
  port (jczaksyqvo : out std_logic_vector(3 downto 1); wfkoaqxho : in std_logic_vector(3 downto 2); sxafksko : in time; o : in integer);
end bad;

architecture utdrtthk of bad is
  
begin
  -- Multi-driven assignments
  jczaksyqvo <= ('Z', 'H', 'Z');
  jczaksyqvo <= ('-', '1', 'H');
  jczaksyqvo <= ('1', 'U', '0');
  jczaksyqvo <= "HWW";
end utdrtthk;

library ieee;
use ieee.std_logic_1164.all;

entity hhbalfb is
  port ( auw : linkage severity_level
  ; ionlwjr : linkage std_logic_vector(3 to 4)
  ; ma : buffer string(5 downto 2)
  ; bsesnav : out integer_vector(4 downto 0)
  );
end hhbalfb;

library ieee;
use ieee.std_logic_1164.all;

architecture v of hhbalfb is
  signal etkrcbye : integer;
  signal wtsdhd : time;
  signal qigs : std_logic_vector(3 downto 2);
  signal xkq : std_logic_vector(3 downto 1);
begin
  qofxrubch : entity work.bad
    port map (jczaksyqvo => xkq, wfkoaqxho => qigs, sxafksko => wtsdhd, o => etkrcbye);
  
  -- Single-driven assignments
  bsesnav <= (0_2_1_4_3, 3413, 2, 43214, 16#8_0_3#);
  etkrcbye <= 0_1_1_0_4;
  ma <= "dqdm";
  wtsdhd <= 10120 ms;
  
  -- Multi-driven assignments
  qigs <= ('W', 'Z');
  xkq <= ('1', 'W', 'X');
  xkq <= "L0U";
  qigs <= ('-', 'H');
end v;



-- Seed after: 175772308386002313,3687118713772291287
