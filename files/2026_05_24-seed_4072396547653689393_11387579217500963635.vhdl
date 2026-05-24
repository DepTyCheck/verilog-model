-- Seed: 4072396547653689393,11387579217500963635



entity edzbpcvebj is
  port (fhemnguo : buffer integer; bjf : linkage time);
end edzbpcvebj;



architecture tvrrspj of edzbpcvebj is
  
begin
  
end tvrrspj;



entity vyza is
  port (aefkjherk : in integer; asfg : in bit);
end vyza;



architecture cdgt of vyza is
  signal f : integer;
  signal xytujlakpg : time;
  signal gp : integer;
  signal dzehoajf : time;
  signal sm : integer;
begin
  oepgc : entity work.edzbpcvebj
    port map (fhemnguo => sm, bjf => dzehoajf);
  glvxqewmgc : entity work.edzbpcvebj
    port map (fhemnguo => gp, bjf => xytujlakpg);
  vixgmamg : entity work.edzbpcvebj
    port map (fhemnguo => f, bjf => xytujlakpg);
end cdgt;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (suxbek : linkage integer_vector(2 downto 0); ttwwmf : inout std_logic);
end s;



architecture vuhfgr of s is
  signal bbeqplzne : integer;
  signal hehip : bit;
  signal nyfaadqvzj : time;
  signal ttrfmi : integer;
begin
  dcwhharq : entity work.edzbpcvebj
    port map (fhemnguo => ttrfmi, bjf => nyfaadqvzj);
  r : entity work.vyza
    port map (aefkjherk => ttrfmi, asfg => hehip);
  qurhbghtm : entity work.edzbpcvebj
    port map (fhemnguo => bbeqplzne, bjf => nyfaadqvzj);
end vuhfgr;



-- Seed after: 1738310917633280564,11387579217500963635
