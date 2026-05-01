-- Seed: 14161628062530949169,779721673283024895



entity zhp is
  port (bthqx : buffer integer; xyces : buffer time);
end zhp;



architecture xqscv of zhp is
  
begin
  
end xqscv;



entity snol is
  port (z : out time);
end snol;



architecture laptymvrgz of snol is
  signal vdgxqsb : time;
  signal befm : integer;
  signal qwlxcl : integer;
  signal xjestg : time;
  signal cknhtqfm : integer;
begin
  ngcodt : entity work.zhp
    port map (bthqx => cknhtqfm, xyces => xjestg);
  nne : entity work.zhp
    port map (bthqx => qwlxcl, xyces => z);
  dawpettfz : entity work.zhp
    port map (bthqx => befm, xyces => vdgxqsb);
end laptymvrgz;

library ieee;
use ieee.std_logic_1164.all;

entity gtqzkmmbp is
  port (bftsbqxaql : out std_logic; llfw : out character);
end gtqzkmmbp;



architecture xq of gtqzkmmbp is
  
begin
  
end xq;



entity npejfeml is
  port (b : in boolean);
end npejfeml;

library ieee;
use ieee.std_logic_1164.all;

architecture qgez of npejfeml is
  signal ssl : time;
  signal qhqm : integer;
  signal zn : time;
  signal sqkr : character;
  signal piclosh : character;
  signal ecwaygjy : std_logic;
begin
  awfucun : entity work.gtqzkmmbp
    port map (bftsbqxaql => ecwaygjy, llfw => piclosh);
  mzri : entity work.gtqzkmmbp
    port map (bftsbqxaql => ecwaygjy, llfw => sqkr);
  qyqqynff : entity work.snol
    port map (z => zn);
  sxujw : entity work.zhp
    port map (bthqx => qhqm, xyces => ssl);
end qgez;



-- Seed after: 261798567861819999,779721673283024895
