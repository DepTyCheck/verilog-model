-- Seed: 2707819558966054105,2158184632809654795

use std.reflection.all;

entity ghg is
  port (cl : inout enumeration_value_mirror);
end ghg;

architecture s of ghg is
  
begin
  
end s;

entity afpbvh is
  port (cyleq : buffer time);
end afpbvh;

use std.reflection.all;

architecture vhfqgb of afpbvh is
  shared variable rjhklcqhsl : enumeration_value_mirror;
  shared variable aneny : enumeration_value_mirror;
  shared variable mnlhbkpxvc : enumeration_value_mirror;
begin
  fqtoalmp : entity work.ghg
    port map (cl => mnlhbkpxvc);
  pktxsa : entity work.ghg
    port map (cl => aneny);
  jqpa : entity work.ghg
    port map (cl => rjhklcqhsl);
end vhfqgb;

entity s is
  port (uowje : buffer boolean);
end s;

use std.reflection.all;

architecture uquknh of s is
  shared variable lhdewslldh : enumeration_value_mirror;
  shared variable pgrsbp : enumeration_value_mirror;
  shared variable binrfgvykb : enumeration_value_mirror;
begin
  agyappxrcr : entity work.ghg
    port map (cl => binrfgvykb);
  xin : entity work.ghg
    port map (cl => pgrsbp);
  dt : entity work.ghg
    port map (cl => lhdewslldh);
  
  -- Single-driven assignments
  uowje <= FALSE;
end uquknh;

use std.reflection.all;

entity jyrbtlaqs is
  port (fnrqc : inout enumeration_value_mirror);
end jyrbtlaqs;

use std.reflection.all;

architecture ler of jyrbtlaqs is
  shared variable gi : enumeration_value_mirror;
  signal w : boolean;
  signal cyvgkxj : time;
begin
  o : entity work.ghg
    port map (cl => fnrqc);
  yda : entity work.afpbvh
    port map (cyleq => cyvgkxj);
  qgwdjymp : entity work.s
    port map (uowje => w);
  jwpeexveva : entity work.ghg
    port map (cl => gi);
end ler;



-- Seed after: 1078735003209013204,2158184632809654795
