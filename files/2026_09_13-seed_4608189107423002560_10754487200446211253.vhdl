-- Seed: 4608189107423002560,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity toq is
  port (amqlxg : in time; fuixmuez : out std_logic_vector(0 to 4); g : inout real; biwtl : inout integer);
end toq;

architecture hguvvdl of toq is
  
begin
  -- Multi-driven assignments
  fuixmuez <= "Z01H0";
  fuixmuez <= fuixmuez;
  fuixmuez <= fuixmuez;
  fuixmuez <= fuixmuez;
end hguvvdl;

library ieee;
use ieee.std_logic_1164.all;

entity tyoixzkl is
  port (ppvxqhsesp : inout std_logic_vector(1 to 1));
end tyoixzkl;

library ieee;
use ieee.std_logic_1164.all;

architecture vf of tyoixzkl is
  signal qoj : integer;
  signal ks : real;
  signal kacoq : std_logic_vector(0 to 4);
  signal czu : time;
  signal vniwhszf : integer;
  signal amt : real;
  signal d : time;
  signal opcdkyd : integer;
  signal nlxvvnu : real;
  signal iahomyvw : std_logic_vector(0 to 4);
  signal cfebdgbk : time;
  signal valumgl : integer;
  signal gmaejtxorx : real;
  signal mfinbgohwx : std_logic_vector(0 to 4);
  signal xjypyqbee : time;
begin
  tpey : entity work.toq
    port map (amqlxg => xjypyqbee, fuixmuez => mfinbgohwx, g => gmaejtxorx, biwtl => valumgl);
  rh : entity work.toq
    port map (amqlxg => cfebdgbk, fuixmuez => iahomyvw, g => nlxvvnu, biwtl => opcdkyd);
  vaxdcfavje : entity work.toq
    port map (amqlxg => d, fuixmuez => mfinbgohwx, g => amt, biwtl => vniwhszf);
  n : entity work.toq
    port map (amqlxg => czu, fuixmuez => kacoq, g => ks, biwtl => qoj);
  
  -- Single-driven assignments
  xjypyqbee <= 1 sec;
  d <= 3_2_2_1_2.30241 fs;
  cfebdgbk <= 8#6_2_1.63# fs;
  czu <= 3 sec;
  
  -- Multi-driven assignments
  ppvxqhsesp <= "U";
end vf;



-- Seed after: 14882555930521486710,10754487200446211253
