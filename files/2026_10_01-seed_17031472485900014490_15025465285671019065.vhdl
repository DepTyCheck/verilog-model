-- Seed: 17031472485900014490,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity gbmcub is
  port (fpjaa : out std_logic_vector(2 to 3));
end gbmcub;

architecture tpn of gbmcub is
  
begin
  -- Multi-driven assignments
  fpjaa <= fpjaa;
end tpn;

library ieee;
use ieee.std_logic_1164.all;

entity jqgd is
  port (maob : out integer; jikjdruj : inout std_logic; nqsjbj : in bit_vector(2 to 1); hbyhd : linkage std_logic_vector(4 to 0));
end jqgd;

library ieee;
use ieee.std_logic_1164.all;

architecture dwyzuq of jqgd is
  signal zbtzwb : std_logic_vector(2 to 3);
  signal ea : std_logic_vector(2 to 3);
begin
  mvcri : entity work.gbmcub
    port map (fpjaa => ea);
  zurf : entity work.gbmcub
    port map (fpjaa => zbtzwb);
  
  -- Single-driven assignments
  maob <= 2#0110#;
  
  -- Multi-driven assignments
  ea <= ea;
  jikjdruj <= 'W';
  jikjdruj <= 'Z';
end dwyzuq;



-- Seed after: 3832167066135310416,15025465285671019065
