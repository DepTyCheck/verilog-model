-- Seed: 8945026098157158188,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity gngv is
  port (eqtbwidw : linkage boolean_vector(3 downto 0); toer : out std_logic; mmgdwy : inout array_value_mirror);
end gngv;

architecture foi of gngv is
  
begin
  
end foi;

library ieee;
use ieee.std_logic_1164.all;

entity htgnfem is
  port (w : inout std_logic_vector(0 to 1));
end htgnfem;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tqnbbah of htgnfem is
  shared variable v : array_value_mirror;
  signal xuxcbn : boolean_vector(3 downto 0);
  shared variable ejqvsewaeq : array_value_mirror;
  signal t : boolean_vector(3 downto 0);
  shared variable udmrplyt : array_value_mirror;
  signal artay : std_logic;
  signal tv : boolean_vector(3 downto 0);
begin
  rfpjovpru : entity work.gngv
    port map (eqtbwidw => tv, toer => artay, mmgdwy => udmrplyt);
  dkduatuizo : entity work.gngv
    port map (eqtbwidw => t, toer => artay, mmgdwy => ejqvsewaeq);
  rgsndwsth : entity work.gngv
    port map (eqtbwidw => xuxcbn, toer => artay, mmgdwy => v);
  
  -- Multi-driven assignments
  w <= w;
  w <= w;
  w <= ('Z', 'W');
  w <= ('L', 'X');
end tqnbbah;

entity yeucvinlrk is
  port (z : buffer integer; iwdti : out time);
end yeucvinlrk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture jyd of yeucvinlrk is
  shared variable bx : array_value_mirror;
  signal skndahpr : boolean_vector(3 downto 0);
  shared variable fc : array_value_mirror;
  signal sfc : std_logic;
  signal axmpxbudy : boolean_vector(3 downto 0);
  signal xmb : std_logic_vector(0 to 1);
begin
  nbd : entity work.htgnfem
    port map (w => xmb);
  uvdgqii : entity work.gngv
    port map (eqtbwidw => axmpxbudy, toer => sfc, mmgdwy => fc);
  fatkiwry : entity work.gngv
    port map (eqtbwidw => skndahpr, toer => sfc, mmgdwy => bx);
  
  -- Multi-driven assignments
  xmb <= ('Z', '-');
  xmb <= xmb;
  xmb <= xmb;
  xmb <= "ZZ";
end jyd;

use std.reflection.all;

entity xfvu is
  port (fkfgcn : inout enumeration_value_mirror);
end xfvu;

architecture pphdnsh of xfvu is
  
begin
  
end pphdnsh;



-- Seed after: 14941561969193681634,14426950258250697445
