-- Seed: 16235308453314514371,8581541265164261811

library ieee;
use ieee.std_logic_1164.all;

entity irrigshcm is
  port (otbg : out std_logic);
end irrigshcm;



architecture bp of irrigshcm is
  
begin
  
end bp;



entity omfysudw is
  port (wkpkoldzr : out time; dagvld : inout time);
end omfysudw;

library ieee;
use ieee.std_logic_1164.all;

architecture r of omfysudw is
  signal qnn : std_logic;
begin
  o : entity work.irrigshcm
    port map (otbg => qnn);
end r;

library ieee;
use ieee.std_logic_1164.all;

entity vsieasmly is
  port (u : in std_logic; emd : buffer bit; vzhzthulr : in severity_level);
end vsieasmly;

library ieee;
use ieee.std_logic_1164.all;

architecture onlik of vsieasmly is
  signal ynkidlil : std_logic;
begin
  rj : entity work.irrigshcm
    port map (otbg => ynkidlil);
end onlik;



entity c is
  port (xh : linkage time; tgxru : linkage time);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture zdcqheg of c is
  signal mrijikyod : time;
  signal inibvww : time;
  signal plqj : severity_level;
  signal q : bit;
  signal qqocdpyfc : std_logic;
  signal pmsbxkigt : time;
  signal rcfsavbfo : time;
begin
  kqvxgyoao : entity work.omfysudw
    port map (wkpkoldzr => rcfsavbfo, dagvld => pmsbxkigt);
  uzvlhujrmr : entity work.vsieasmly
    port map (u => qqocdpyfc, emd => q, vzhzthulr => plqj);
  ywvbwvjbao : entity work.omfysudw
    port map (wkpkoldzr => inibvww, dagvld => mrijikyod);
end zdcqheg;



-- Seed after: 437158502472468016,8581541265164261811
