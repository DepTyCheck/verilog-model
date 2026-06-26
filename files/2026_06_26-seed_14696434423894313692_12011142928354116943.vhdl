-- Seed: 14696434423894313692,12011142928354116943

entity hmckctnr is
  port (xsjrleyswz : inout time; npdjpulfd : out boolean_vector(1 to 1));
end hmckctnr;

architecture aidulrqwhk of hmckctnr is
  
begin
  -- Single-driven assignments
  xsjrleyswz <= 16#C_F_B_1_3# fs;
end aidulrqwhk;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (igrprgrigu : buffer string(2 downto 1); rtmd : out std_logic_vector(2 downto 3); l : in std_logic);
end a;

architecture jfqqkghxnb of a is
  signal zchcaxmx : boolean_vector(1 to 1);
  signal kajqsvhm : time;
  signal dnowsngl : boolean_vector(1 to 1);
  signal vhqq : time;
  signal qsca : boolean_vector(1 to 1);
  signal afhixv : time;
begin
  zpmihhni : entity work.hmckctnr
    port map (xsjrleyswz => afhixv, npdjpulfd => qsca);
  nyvmyederp : entity work.hmckctnr
    port map (xsjrleyswz => vhqq, npdjpulfd => dnowsngl);
  klthcu : entity work.hmckctnr
    port map (xsjrleyswz => kajqsvhm, npdjpulfd => zchcaxmx);
  
  -- Single-driven assignments
  igrprgrigu <= ('h', 'g');
  
  -- Multi-driven assignments
  rtmd <= (others => '0');
end jfqqkghxnb;



-- Seed after: 4758529043185040405,12011142928354116943
