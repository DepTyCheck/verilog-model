-- Seed: 5484211811996323207,17611625116949931313



entity boiknoxw is
  port (vsoszxj : linkage time; vymhn : linkage time; aarozl : in integer; oykjgfkta : in real);
end boiknoxw;



architecture vrmwlgac of boiknoxw is
  
begin
  
end vrmwlgac;

library ieee;
use ieee.std_logic_1164.all;

entity sf is
  port (hzmsg : buffer real; qhk : in std_logic; lpalw : linkage time; nfsm : linkage character);
end sf;



architecture cewpsnbev of sf is
  signal gezgbx : integer;
  signal vpnoherv : time;
begin
  p : entity work.boiknoxw
    port map (vsoszxj => lpalw, vymhn => vpnoherv, aarozl => gezgbx, oykjgfkta => hzmsg);
end cewpsnbev;



entity pytb is
  port (swvhokoe : buffer severity_level);
end pytb;



architecture zkon of pytb is
  signal t : real;
  signal ycjrbtfj : time;
  signal ivgzo : real;
  signal paqm : integer;
  signal w : time;
  signal ejfqtbxavq : time;
begin
  nr : entity work.boiknoxw
    port map (vsoszxj => ejfqtbxavq, vymhn => w, aarozl => paqm, oykjgfkta => ivgzo);
  zovia : entity work.boiknoxw
    port map (vsoszxj => ejfqtbxavq, vymhn => ejfqtbxavq, aarozl => paqm, oykjgfkta => ivgzo);
  zipvf : entity work.boiknoxw
    port map (vsoszxj => ejfqtbxavq, vymhn => ycjrbtfj, aarozl => paqm, oykjgfkta => t);
end zkon;

library ieee;
use ieee.std_logic_1164.all;

entity rzjgco is
  port (pxow : in integer; lngiauedzw : buffer std_logic);
end rzjgco;



architecture twmwixuns of rzjgco is
  signal k : integer;
  signal dghkt : severity_level;
  signal fwadesleht : real;
  signal jvhmnld : integer;
  signal eiwyjpsbr : time;
  signal a : time;
begin
  paiyfq : entity work.boiknoxw
    port map (vsoszxj => a, vymhn => eiwyjpsbr, aarozl => jvhmnld, oykjgfkta => fwadesleht);
  gwyotzlzgh : entity work.pytb
    port map (swvhokoe => dghkt);
  gbmzukaf : entity work.boiknoxw
    port map (vsoszxj => a, vymhn => a, aarozl => k, oykjgfkta => fwadesleht);
end twmwixuns;



-- Seed after: 17790663708151808917,17611625116949931313
