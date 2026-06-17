-- Seed: 8816786409551818426,10557070023141912087

entity gn is
  port (olqlx : buffer real; vucjc : buffer boolean; zgj : in bit);
end gn;

architecture pyt of gn is
  
begin
  -- Single-driven assignments
  vucjc <= FALSE;
end pyt;

entity hzqdnc is
  port (xerfw : out severity_level; ouqpumm : buffer integer; co : out boolean);
end hzqdnc;

architecture kf of hzqdnc is
  signal npcib : real;
  signal fhagrui : boolean;
  signal r : real;
  signal zbt : boolean;
  signal wijy : real;
  signal bhxluwcgc : bit;
  signal bn : boolean;
  signal doror : real;
begin
  xspnfsvsl : entity work.gn
    port map (olqlx => doror, vucjc => bn, zgj => bhxluwcgc);
  gcng : entity work.gn
    port map (olqlx => wijy, vucjc => zbt, zgj => bhxluwcgc);
  fqtuuo : entity work.gn
    port map (olqlx => r, vucjc => fhagrui, zgj => bhxluwcgc);
  rrxspwonz : entity work.gn
    port map (olqlx => npcib, vucjc => co, zgj => bhxluwcgc);
  
  -- Single-driven assignments
  bhxluwcgc <= '1';
end kf;

entity abtljbizg is
  port (aobz : in real; xgipoa : linkage real);
end abtljbizg;

architecture ewhsmcjia of abtljbizg is
  signal cjoduwsnq : boolean;
  signal mtoqmmb : real;
  signal whvg : bit;
  signal erymnjlxea : boolean;
  signal kep : real;
  signal lonx : bit;
  signal wvzoeiulw : boolean;
  signal nx : real;
begin
  dty : entity work.gn
    port map (olqlx => nx, vucjc => wvzoeiulw, zgj => lonx);
  yjtr : entity work.gn
    port map (olqlx => kep, vucjc => erymnjlxea, zgj => whvg);
  fy : entity work.gn
    port map (olqlx => mtoqmmb, vucjc => cjoduwsnq, zgj => whvg);
end ewhsmcjia;

entity qdbjaiv is
  port (dplj : inout string(1 to 3); p : out time);
end qdbjaiv;

architecture geavf of qdbjaiv is
  signal iyoshwg : real;
  signal kgg : bit;
  signal ajqmbpshy : boolean;
  signal fkwzryvtr : real;
begin
  o : entity work.gn
    port map (olqlx => fkwzryvtr, vucjc => ajqmbpshy, zgj => kgg);
  gu : entity work.abtljbizg
    port map (aobz => iyoshwg, xgipoa => iyoshwg);
  
  -- Single-driven assignments
  p <= 2#01001.1# ms;
end geavf;



-- Seed after: 3625046099770595388,10557070023141912087
