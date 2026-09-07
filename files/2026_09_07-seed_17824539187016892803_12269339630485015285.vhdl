-- Seed: 17824539187016892803,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (mg : linkage std_logic_vector(3 to 0); sron : inout real);
end g;

architecture unj of g is
  
begin
  -- Single-driven assignments
  sron <= 22.2;
end unj;

library ieee;
use ieee.std_logic_1164.all;

entity pthkaixc is
  port (y : buffer integer; nrc : linkage std_logic; zyulkwrt : linkage std_logic_vector(4 to 4));
end pthkaixc;

library ieee;
use ieee.std_logic_1164.all;

architecture sfmdzzm of pthkaixc is
  signal eugvl : real;
  signal wbgy : std_logic_vector(3 to 0);
begin
  zvfdk : entity work.g
    port map (mg => wbgy, sron => eugvl);
  
  -- Single-driven assignments
  y <= 3_3_0_0;
  
  -- Multi-driven assignments
  wbgy <= wbgy;
  wbgy <= (others => '0');
end sfmdzzm;



-- Seed after: 13639936028822366256,12269339630485015285
