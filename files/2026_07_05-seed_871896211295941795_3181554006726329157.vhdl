-- Seed: 871896211295941795,3181554006726329157

use std.reflection.all;

entity gk is
  port (tgktnttou : inout protected_value_mirror; zxy : buffer integer_vector(2 to 2));
end gk;

architecture ja of gk is
  
begin
  -- Single-driven assignments
  zxy <= zxy;
end ja;

use std.reflection.all;

entity tojzk is
  port (xbiyuja : inout integer_value_mirror; ctlm : inout array_subtype_mirror);
end tojzk;

use std.reflection.all;

architecture heu of tojzk is
  signal xjjqlqfr : integer_vector(2 to 2);
  shared variable lmczkmr : protected_value_mirror;
  signal xsosyxyt : integer_vector(2 to 2);
  shared variable ducwlpk : protected_value_mirror;
begin
  rocwadufc : entity work.gk
    port map (tgktnttou => ducwlpk, zxy => xsosyxyt);
  felsux : entity work.gk
    port map (tgktnttou => lmczkmr, zxy => xjjqlqfr);
end heu;

library ieee;
use ieee.std_logic_1164.all;

entity pm is
  port (mvzgpjiyl : out character; bghc : in std_logic);
end pm;

use std.reflection.all;

architecture b of pm is
  signal bjlsr : integer_vector(2 to 2);
  shared variable qycaz : protected_value_mirror;
  signal llsdejp : integer_vector(2 to 2);
  shared variable xnuryu : protected_value_mirror;
  signal yt : integer_vector(2 to 2);
  shared variable bcga : protected_value_mirror;
begin
  jrnxzoydzs : entity work.gk
    port map (tgktnttou => bcga, zxy => yt);
  kf : entity work.gk
    port map (tgktnttou => xnuryu, zxy => llsdejp);
  ewvoaklrmu : entity work.gk
    port map (tgktnttou => qycaz, zxy => bjlsr);
  
  -- Single-driven assignments
  mvzgpjiyl <= mvzgpjiyl;
end b;

use std.reflection.all;

entity qbgw is
  port (rdl : buffer time; hy : inout protected_subtype_mirror; rc : inout value_mirror);
end qbgw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mhwp of qbgw is
  signal dmnchc : std_logic;
  signal fz : character;
  signal fkboawf : integer_vector(2 to 2);
  shared variable broao : protected_value_mirror;
  signal nidordu : integer_vector(2 to 2);
  shared variable ziki : protected_value_mirror;
  signal njzodcb : integer_vector(2 to 2);
  shared variable vwvyexpg : protected_value_mirror;
begin
  wewewpmx : entity work.gk
    port map (tgktnttou => vwvyexpg, zxy => njzodcb);
  lygixp : entity work.gk
    port map (tgktnttou => ziki, zxy => nidordu);
  zg : entity work.gk
    port map (tgktnttou => broao, zxy => fkboawf);
  gepuyjmdy : entity work.pm
    port map (mvzgpjiyl => fz, bghc => dmnchc);
  
  -- Single-driven assignments
  rdl <= 0.4_1_2_4 fs;
  
  -- Multi-driven assignments
  dmnchc <= 'W';
  dmnchc <= dmnchc;
  dmnchc <= dmnchc;
end mhwp;



-- Seed after: 15259573720491072679,3181554006726329157
