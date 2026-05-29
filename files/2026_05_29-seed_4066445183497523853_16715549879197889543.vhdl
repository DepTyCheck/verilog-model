-- Seed: 4066445183497523853,16715549879197889543



entity u is
  port (fyf : out real; evghaycum : buffer integer_vector(0 to 3));
end u;



architecture s of u is
  
begin
  
end s;



entity lgqwf is
  port (xyorojd : out time; hpvin : inout integer; gbwwgl : linkage integer_vector(1 to 2); vrn : out real);
end lgqwf;



architecture wfr of lgqwf is
  signal nowthasxj : integer_vector(0 to 3);
begin
  cta : entity work.u
    port map (fyf => vrn, evghaycum => nowthasxj);
end wfr;

library ieee;
use ieee.std_logic_1164.all;

entity xti is
  port (pr : linkage std_logic; bayfyy : linkage severity_level);
end xti;



architecture qaapwndg of xti is
  signal gdeontxp : integer_vector(0 to 3);
  signal svluy : real;
  signal th : integer_vector(0 to 3);
  signal noqgvfd : real;
begin
  l : entity work.u
    port map (fyf => noqgvfd, evghaycum => th);
  yyoxq : entity work.u
    port map (fyf => svluy, evghaycum => gdeontxp);
end qaapwndg;



entity aofvxci is
  port (qcqvz : linkage real; lgfd : buffer character; zhdabej : buffer integer);
end aofvxci;

library ieee;
use ieee.std_logic_1164.all;

architecture f of aofvxci is
  signal vq : integer_vector(0 to 3);
  signal lejlnfy : real;
  signal jiqafvpm : real;
  signal pvwpqnxo : integer_vector(1 to 2);
  signal h : time;
  signal asjxqy : severity_level;
  signal ofvmbahyh : std_logic;
  signal yqck : integer_vector(0 to 3);
  signal xn : real;
begin
  dfeycysop : entity work.u
    port map (fyf => xn, evghaycum => yqck);
  zb : entity work.xti
    port map (pr => ofvmbahyh, bayfyy => asjxqy);
  rzfccd : entity work.lgqwf
    port map (xyorojd => h, hpvin => zhdabej, gbwwgl => pvwpqnxo, vrn => jiqafvpm);
  dmofgmw : entity work.u
    port map (fyf => lejlnfy, evghaycum => vq);
end f;



-- Seed after: 7976318704127939461,16715549879197889543
