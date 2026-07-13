-- Seed: 497786312580983127,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity rwjvi is
  port (jlrlottw : buffer std_logic; gpkx : inout access_value_mirror; hbz : in time; ryjablv : linkage character);
end rwjvi;

architecture t of rwjvi is
  
begin
  -- Multi-driven assignments
  jlrlottw <= jlrlottw;
  jlrlottw <= 'L';
  jlrlottw <= jlrlottw;
end t;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pkbfavwaf is
  port (janaiky : out real_vector(0 downto 3); ymyj : inout access_value_mirror; opmvysyr : in std_logic; symffmxfyg : inout time);
end pkbfavwaf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture bu of pkbfavwaf is
  signal wkw : character;
  shared variable pmnsfjaus : access_value_mirror;
  signal sk : std_logic;
  signal ajpsaiufz : character;
  signal yegmngpf : std_logic;
  signal wpd : character;
  signal nmozuleuw : time;
  shared variable bm : access_value_mirror;
  signal bdl : std_logic;
  signal qohtddjyx : character;
  signal rqm : time;
  shared variable dxt : access_value_mirror;
  signal qdufzkqxx : std_logic;
begin
  fwqtriz : entity work.rwjvi
    port map (jlrlottw => qdufzkqxx, gpkx => dxt, hbz => rqm, ryjablv => qohtddjyx);
  gvhct : entity work.rwjvi
    port map (jlrlottw => bdl, gpkx => bm, hbz => nmozuleuw, ryjablv => wpd);
  gycyafx : entity work.rwjvi
    port map (jlrlottw => yegmngpf, gpkx => ymyj, hbz => nmozuleuw, ryjablv => ajpsaiufz);
  ffjw : entity work.rwjvi
    port map (jlrlottw => sk, gpkx => pmnsfjaus, hbz => rqm, ryjablv => wkw);
  
  -- Single-driven assignments
  janaiky <= (others => 0.0);
  symffmxfyg <= 4.0 ps;
  rqm <= symffmxfyg;
  
  -- Multi-driven assignments
  yegmngpf <= opmvysyr;
end bu;



-- Seed after: 12647825445201770605,3566912872917928779
