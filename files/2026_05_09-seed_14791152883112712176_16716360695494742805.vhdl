-- Seed: 14791152883112712176,16716360695494742805



entity anm is
  port (ncss : buffer time; e : buffer severity_level);
end anm;



architecture cq of anm is
  
begin
  
end cq;

library ieee;
use ieee.std_logic_1164.all;

entity oztgsfr is
  port (m : inout std_logic; ybfsctu : linkage real);
end oztgsfr;



architecture cchzfcf of oztgsfr is
  signal kpfrvda : severity_level;
  signal akjypakg : time;
begin
  azkzkt : entity work.anm
    port map (ncss => akjypakg, e => kpfrvda);
end cchzfcf;



entity ie is
  port (qzzbppw : out time);
end ie;

library ieee;
use ieee.std_logic_1164.all;

architecture askocj of ie is
  signal gtayxi : severity_level;
  signal xwnav : real;
  signal wbyl : std_logic;
begin
  miqekhmfsm : entity work.oztgsfr
    port map (m => wbyl, ybfsctu => xwnav);
  zfffybnrki : entity work.anm
    port map (ncss => qzzbppw, e => gtayxi);
end askocj;

library ieee;
use ieee.std_logic_1164.all;

entity ljkclnhrl is
  port (jkb : out time; wcvwuhegx : inout std_logic);
end ljkclnhrl;

library ieee;
use ieee.std_logic_1164.all;

architecture lwzvf of ljkclnhrl is
  signal kus : real;
  signal lmvxedi : std_logic;
  signal rjtwpujdzz : severity_level;
  signal iui : time;
  signal ereezfpo : severity_level;
  signal hfk : time;
begin
  ehimov : entity work.anm
    port map (ncss => hfk, e => ereezfpo);
  wbj : entity work.anm
    port map (ncss => iui, e => rjtwpujdzz);
  rbtehmyc : entity work.ie
    port map (qzzbppw => jkb);
  qdbjawaqz : entity work.oztgsfr
    port map (m => lmvxedi, ybfsctu => kus);
end lwzvf;



-- Seed after: 14450922691072107050,16716360695494742805
