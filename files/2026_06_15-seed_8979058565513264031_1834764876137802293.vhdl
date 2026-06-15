-- Seed: 8979058565513264031,1834764876137802293

entity lwyrf is
  port (n : in time);
end lwyrf;

architecture sdzo of lwyrf is
  
begin
  
end sdzo;

library ieee;
use ieee.std_logic_1164.all;

entity ibgiykjcu is
  port (twjx : linkage std_logic_vector(1 to 3));
end ibgiykjcu;

architecture jqagw of ibgiykjcu is
  signal jjuyvzdlpe : time;
  signal pj : time;
  signal jzcogux : time;
begin
  gcend : entity work.lwyrf
    port map (n => jzcogux);
  soain : entity work.lwyrf
    port map (n => pj);
  sbu : entity work.lwyrf
    port map (n => jjuyvzdlpe);
  fwvovxqavd : entity work.lwyrf
    port map (n => pj);
  
  -- Single-driven assignments
  jzcogux <= 2 ns;
  jjuyvzdlpe <= 1 hr;
  pj <= 3 sec;
end jqagw;

library ieee;
use ieee.std_logic_1164.all;

entity ydr is
  port (ql : buffer std_logic_vector(1 to 2); qygfvu : out boolean; kygc : linkage severity_level);
end ydr;

library ieee;
use ieee.std_logic_1164.all;

architecture mlogac of ydr is
  signal ocogj : time;
  signal mw : std_logic_vector(1 to 3);
begin
  smmt : entity work.ibgiykjcu
    port map (twjx => mw);
  gi : entity work.lwyrf
    port map (n => ocogj);
  oqsjxiojx : entity work.lwyrf
    port map (n => ocogj);
  xjzmdgve : entity work.lwyrf
    port map (n => ocogj);
  
  -- Single-driven assignments
  qygfvu <= TRUE;
  ocogj <= 16#EF# ps;
  
  -- Multi-driven assignments
  ql <= ('Z', 'U');
  mw <= "Z01";
end mlogac;



-- Seed after: 16360162814191078059,1834764876137802293
