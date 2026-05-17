-- Seed: 3629322369714521705,3820899062418988741

library ieee;
use ieee.std_logic_1164.all;

entity tgxwptp is
  port (j : inout std_logic);
end tgxwptp;



architecture wmhzpndzj of tgxwptp is
  
begin
  
end wmhzpndzj;

library ieee;
use ieee.std_logic_1164.all;

entity faoojzqqvh is
  port (lfctkcpxe : out bit; pg : out time; qcgksyvry : inout std_logic);
end faoojzqqvh;

library ieee;
use ieee.std_logic_1164.all;

architecture k of faoojzqqvh is
  signal lrfnurjqd : std_logic;
  signal whuny : std_logic;
begin
  v : entity work.tgxwptp
    port map (j => whuny);
  jpxlzhxoum : entity work.tgxwptp
    port map (j => qcgksyvry);
  alhmqzimdc : entity work.tgxwptp
    port map (j => lrfnurjqd);
end k;



entity ogi is
  port (kilni : in integer);
end ogi;

library ieee;
use ieee.std_logic_1164.all;

architecture bjrbguftvh of ogi is
  signal lwi : std_logic;
  signal jbadz : std_logic;
begin
  tjfa : entity work.tgxwptp
    port map (j => jbadz);
  zofmzrln : entity work.tgxwptp
    port map (j => lwi);
end bjrbguftvh;



entity lnvsa is
  port (eaexif : buffer time; zamp : out severity_level);
end lnvsa;

library ieee;
use ieee.std_logic_1164.all;

architecture rm of lnvsa is
  signal rwhie : integer;
  signal mrkhtt : std_logic;
  signal kflyvdvqz : bit;
  signal iaagmnx : integer;
begin
  ktp : entity work.ogi
    port map (kilni => iaagmnx);
  qi : entity work.faoojzqqvh
    port map (lfctkcpxe => kflyvdvqz, pg => eaexif, qcgksyvry => mrkhtt);
  nvws : entity work.ogi
    port map (kilni => rwhie);
end rm;



-- Seed after: 1879094143560995325,3820899062418988741
