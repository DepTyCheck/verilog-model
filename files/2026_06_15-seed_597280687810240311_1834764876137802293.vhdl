-- Seed: 597280687810240311,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity si is
  port (lnpcjf : inout std_logic_vector(4 to 4); kc : inout bit);
end si;

architecture u of si is
  
begin
  -- Single-driven assignments
  kc <= '0';
  
  -- Multi-driven assignments
  lnpcjf <= (others => 'W');
end u;

entity lccfueldh is
  port (mgxvw : linkage time);
end lccfueldh;

library ieee;
use ieee.std_logic_1164.all;

architecture qaynr of lccfueldh is
  signal fggkwie : bit;
  signal xlxzpduzfj : std_logic_vector(4 to 4);
  signal agbd : bit;
  signal annvb : std_logic_vector(4 to 4);
  signal kcffm : bit;
  signal e : bit;
  signal xydgnr : std_logic_vector(4 to 4);
begin
  dydss : entity work.si
    port map (lnpcjf => xydgnr, kc => e);
  zm : entity work.si
    port map (lnpcjf => xydgnr, kc => kcffm);
  idykmc : entity work.si
    port map (lnpcjf => annvb, kc => agbd);
  j : entity work.si
    port map (lnpcjf => xlxzpduzfj, kc => fggkwie);
  
  -- Multi-driven assignments
  annvb <= "0";
  annvb <= "W";
  xydgnr <= "W";
  xydgnr <= (others => '-');
end qaynr;



-- Seed after: 8560944026441053639,1834764876137802293
