-- Seed: 11025578675826866500,6562834012285671529

entity hduozt is
  port (nrudzun : out character);
end hduozt;

architecture zjfibeye of hduozt is
  
begin
  
end zjfibeye;

entity sbskonf is
  port (gszf : buffer time_vector(0 to 2); vc : buffer time; qycr : inout integer);
end sbskonf;

architecture ziajyxdpb of sbskonf is
  signal cxz : character;
  signal hqa : character;
  signal kgqcozkykr : character;
begin
  e : entity work.hduozt
    port map (nrudzun => kgqcozkykr);
  gcgnnso : entity work.hduozt
    port map (nrudzun => hqa);
  oqqgoipra : entity work.hduozt
    port map (nrudzun => cxz);
  
  -- Single-driven assignments
  qycr <= qycr;
end ziajyxdpb;

library ieee;
use ieee.std_logic_1164.all;

entity ukiptd is
  port (cjc : in time; rsrcctytvf : inout std_logic; kdqakm : inout real_vector(3 to 1));
end ukiptd;

architecture vy of ukiptd is
  signal u : character;
begin
  gz : entity work.hduozt
    port map (nrudzun => u);
  
  -- Single-driven assignments
  kdqakm <= (others => 0.0);
  
  -- Multi-driven assignments
  rsrcctytvf <= 'X';
  rsrcctytvf <= rsrcctytvf;
  rsrcctytvf <= 'H';
  rsrcctytvf <= rsrcctytvf;
end vy;



-- Seed after: 6097596568904649075,6562834012285671529
