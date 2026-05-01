-- Seed: 16141333642839959919,779721673283024895



entity fnt is
  port (jjybumxj : in integer; ojhrhkup : out time; dyugzwfa : buffer integer; xs : in integer);
end fnt;



architecture mvh of fnt is
  
begin
  
end mvh;



entity rspjw is
  port (kdi : linkage integer; ezihe : inout time; bzrzloa : in real);
end rspjw;



architecture koeexkdjn of rspjw is
  signal fhcnpfv : integer;
  signal pi : integer;
  signal edtnafmch : time;
  signal fcojggsgz : integer;
  signal ulm : integer;
  signal mkxbsvcgvf : time;
  signal qoahw : integer;
begin
  hhdh : entity work.fnt
    port map (jjybumxj => qoahw, ojhrhkup => mkxbsvcgvf, dyugzwfa => qoahw, xs => ulm);
  wt : entity work.fnt
    port map (jjybumxj => fcojggsgz, ojhrhkup => edtnafmch, dyugzwfa => ulm, xs => qoahw);
  r : entity work.fnt
    port map (jjybumxj => ulm, ojhrhkup => ezihe, dyugzwfa => pi, xs => fhcnpfv);
end koeexkdjn;

library ieee;
use ieee.std_logic_1164.all;

entity zpoujyb is
  port (oemndeabfg : buffer std_logic; xg : linkage std_logic; ydehwyneyv : out severity_level);
end zpoujyb;



architecture ihr of zpoujyb is
  signal fuqxzani : real;
  signal fpksqjey : time;
  signal z : integer;
begin
  tmbty : entity work.rspjw
    port map (kdi => z, ezihe => fpksqjey, bzrzloa => fuqxzani);
end ihr;



-- Seed after: 6519167525151355891,779721673283024895
