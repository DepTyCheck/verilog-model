-- Seed: 16906122137064481460,3687118713772291287

entity turmdgx is
  port (cffmmmgnbm : out boolean_vector(4 to 3); vwrw : linkage time);
end turmdgx;

architecture srybd of turmdgx is
  
begin
  -- Single-driven assignments
  cffmmmgnbm <= (others => TRUE);
end srybd;

library ieee;
use ieee.std_logic_1164.all;

entity obqw is
  port (enwccn : in std_logic; laldmq : buffer boolean);
end obqw;

architecture jhhkldzgwr of obqw is
  signal xrzjxplhmq : time;
  signal mwv : boolean_vector(4 to 3);
  signal dzxicoa : time;
  signal zifet : boolean_vector(4 to 3);
  signal cxzslqosio : time;
  signal lzpsz : boolean_vector(4 to 3);
  signal rbhs : time;
  signal bmi : boolean_vector(4 to 3);
begin
  sgbzcnqc : entity work.turmdgx
    port map (cffmmmgnbm => bmi, vwrw => rbhs);
  a : entity work.turmdgx
    port map (cffmmmgnbm => lzpsz, vwrw => cxzslqosio);
  obe : entity work.turmdgx
    port map (cffmmmgnbm => zifet, vwrw => dzxicoa);
  cjmaqrt : entity work.turmdgx
    port map (cffmmmgnbm => mwv, vwrw => xrzjxplhmq);
  
  -- Single-driven assignments
  laldmq <= FALSE;
end jhhkldzgwr;



-- Seed after: 3495763195753192341,3687118713772291287
