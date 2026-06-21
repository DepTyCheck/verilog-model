-- Seed: 8213903003701270089,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity ztv is
  port (c : out std_logic);
end ztv;

architecture bdj of ztv is
  
begin
  -- Multi-driven assignments
  c <= '1';
end bdj;

entity eqbek is
  port (el : out real; z : in time);
end eqbek;

library ieee;
use ieee.std_logic_1164.all;

architecture icp of eqbek is
  signal bwjz : std_logic;
  signal yqtdmgb : std_logic;
begin
  gihqtlss : entity work.ztv
    port map (c => yqtdmgb);
  zp : entity work.ztv
    port map (c => bwjz);
  pnq : entity work.ztv
    port map (c => yqtdmgb);
  
  -- Single-driven assignments
  el <= 8#3.2_1#;
end icp;

entity lphc is
  port (heifq : out real; h : buffer severity_level; jt : inout integer);
end lphc;

architecture hs of lphc is
  
begin
  -- Single-driven assignments
  jt <= 16#2E6D0#;
  h <= ERROR;
end hs;



-- Seed after: 18156565601267866535,3687118713772291287
