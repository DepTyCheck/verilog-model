-- Seed: 2864527967824890718,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity pmaz is
  port (tpoiyj : buffer std_logic_vector(1 downto 3); rwqlehfiwv : inout integer; hlspgro : in time; dscxd : in integer);
end pmaz;

architecture olnaskgrv of pmaz is
  
begin
  -- Single-driven assignments
  rwqlehfiwv <= dscxd;
  
  -- Multi-driven assignments
  tpoiyj <= tpoiyj;
end olnaskgrv;

entity xtzgncg is
  port (tdqvrr : in integer; qdvmp : linkage integer);
end xtzgncg;

library ieee;
use ieee.std_logic_1164.all;

architecture wq of xtzgncg is
  signal fsznfhpq : integer;
  signal yfqeqyah : integer;
  signal jmwyeaefdh : time;
  signal syj : integer;
  signal qay : std_logic_vector(1 downto 3);
begin
  dzefi : entity work.pmaz
    port map (tpoiyj => qay, rwqlehfiwv => syj, hlspgro => jmwyeaefdh, dscxd => yfqeqyah);
  li : entity work.pmaz
    port map (tpoiyj => qay, rwqlehfiwv => yfqeqyah, hlspgro => jmwyeaefdh, dscxd => fsznfhpq);
  
  -- Single-driven assignments
  jmwyeaefdh <= 8#011.522# ns;
  
  -- Multi-driven assignments
  qay <= (others => '0');
  qay <= (others => '0');
  qay <= qay;
end wq;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (flyrg : linkage time; w : in std_logic);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture fntw of h is
  signal zzazbsene : integer;
  signal ijdvb : integer;
  signal xlkfanjh : integer;
  signal goswetggqm : time;
  signal tehd : integer;
  signal mnac : time;
  signal aro : integer;
  signal boplgdqul : std_logic_vector(1 downto 3);
begin
  acdbmikvmh : entity work.pmaz
    port map (tpoiyj => boplgdqul, rwqlehfiwv => aro, hlspgro => mnac, dscxd => tehd);
  klitbc : entity work.pmaz
    port map (tpoiyj => boplgdqul, rwqlehfiwv => tehd, hlspgro => goswetggqm, dscxd => xlkfanjh);
  aeihvlwqpg : entity work.xtzgncg
    port map (tdqvrr => ijdvb, qdvmp => zzazbsene);
  
  -- Single-driven assignments
  goswetggqm <= 8#3_3.14556# fs;
  ijdvb <= 2_3_3;
  
  -- Multi-driven assignments
  boplgdqul <= (others => '0');
  boplgdqul <= boplgdqul;
end fntw;

entity q is
  port (mlcg : out integer);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture xvxjlwfz of q is
  signal w : integer;
  signal nplohm : time;
  signal bcfwfyrrvq : integer;
  signal mofgrc : std_logic_vector(1 downto 3);
  signal epnt : std_logic;
  signal rywqhkif : time;
begin
  gig : entity work.h
    port map (flyrg => rywqhkif, w => epnt);
  bowxvloofe : entity work.pmaz
    port map (tpoiyj => mofgrc, rwqlehfiwv => bcfwfyrrvq, hlspgro => nplohm, dscxd => bcfwfyrrvq);
  gkiswqerlj : entity work.xtzgncg
    port map (tdqvrr => mlcg, qdvmp => w);
  cufoglu : entity work.xtzgncg
    port map (tdqvrr => w, qdvmp => mlcg);
  
  -- Multi-driven assignments
  epnt <= epnt;
end xvxjlwfz;



-- Seed after: 16411002851598146993,10940991575366938685
