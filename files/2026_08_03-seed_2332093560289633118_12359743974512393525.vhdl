-- Seed: 2332093560289633118,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity amvcbyr is
  port (sluoftg : in time; uoaka : in std_logic_vector(1 downto 2); emrvozdq : linkage std_logic);
end amvcbyr;

architecture tceguxmnej of amvcbyr is
  
begin
  
end tceguxmnej;

library ieee;
use ieee.std_logic_1164.all;

entity tn is
  port (pgtcvvchd : linkage std_logic);
end tn;

library ieee;
use ieee.std_logic_1164.all;

architecture swuydohu of tn is
  signal rlizgoj : std_logic_vector(1 downto 2);
  signal oeijbzsut : std_logic;
  signal acwqfj : std_logic_vector(1 downto 2);
  signal ntg : time;
begin
  gxicmkizy : entity work.amvcbyr
    port map (sluoftg => ntg, uoaka => acwqfj, emrvozdq => oeijbzsut);
  ppid : entity work.amvcbyr
    port map (sluoftg => ntg, uoaka => rlizgoj, emrvozdq => pgtcvvchd);
  
  -- Single-driven assignments
  ntg <= ntg;
  
  -- Multi-driven assignments
  acwqfj <= rlizgoj;
end swuydohu;



-- Seed after: 9796083791187906936,12359743974512393525
