-- Seed: 15314386162441714661,2158184632809654795

use std.reflection.all;

entity cvztqyzaz is
  port (amgylyadn : inout physical_value_mirror);
end cvztqyzaz;

architecture hrzcf of cvztqyzaz is
  
begin
  
end hrzcf;

use std.reflection.all;

entity pqfdlzhd is
  port (rnho : inout access_subtype_mirror; ssth : inout array_value_mirror; ujf : buffer time_vector(4 downto 0));
end pqfdlzhd;

architecture rghcng of pqfdlzhd is
  
begin
  -- Single-driven assignments
  ujf <= (0.0 fs, 4 min, 2#111# ms, 2#0_1_0_1# ms, 0_0.3 us);
end rghcng;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity saf is
  port (uqm : linkage std_logic_vector(4 to 2); ajmiqup : inout integer_value_mirror; xgqhry : in time);
end saf;

use std.reflection.all;

architecture ssh of saf is
  shared variable douw : physical_value_mirror;
  shared variable lahapr : physical_value_mirror;
  signal iwh : time_vector(4 downto 0);
  shared variable l : array_value_mirror;
  shared variable xwwrqmdysp : access_subtype_mirror;
begin
  knpsuurs : entity work.pqfdlzhd
    port map (rnho => xwwrqmdysp, ssth => l, ujf => iwh);
  ixh : entity work.cvztqyzaz
    port map (amgylyadn => lahapr);
  tstgrgg : entity work.cvztqyzaz
    port map (amgylyadn => douw);
end ssh;



-- Seed after: 17887646525890149477,2158184632809654795
