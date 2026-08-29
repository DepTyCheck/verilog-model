-- Seed: 3425756390893910567,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity pmtk is
  port (cgywy : buffer std_logic);
end pmtk;

architecture sdnbsf of pmtk is
  
begin
  -- Multi-driven assignments
  cgywy <= 'W';
  cgywy <= cgywy;
end sdnbsf;

entity ctv is
  port (qy : buffer boolean; srfmsxgya : buffer time);
end ctv;

library ieee;
use ieee.std_logic_1164.all;

architecture uwtgr of ctv is
  signal ujzmdyfe : std_logic;
begin
  i : entity work.pmtk
    port map (cgywy => ujzmdyfe);
  r : entity work.pmtk
    port map (cgywy => ujzmdyfe);
  
  -- Single-driven assignments
  srfmsxgya <= srfmsxgya;
  qy <= qy;
  
  -- Multi-driven assignments
  ujzmdyfe <= '-';
  ujzmdyfe <= ujzmdyfe;
  ujzmdyfe <= '-';
end uwtgr;



-- Seed after: 4582127264156350365,10463297573877745897
