-- Seed: 14792093523451608898,499459191852795575

entity tjmvk is
  port (pgfg : buffer integer; qsdyn : inout time; dsqesk : linkage time; wwpfmawui : in real);
end tjmvk;

architecture p of tjmvk is
  
begin
  -- Single-driven assignments
  pgfg <= pgfg;
  qsdyn <= qsdyn;
end p;

entity jvag is
  port (tiqulpwyh : buffer time_vector(2 to 2); lxohc : linkage boolean; rvqodzhxm : inout bit_vector(3 to 2); yfadgygz : linkage boolean);
end jvag;

architecture orrlzjautx of jvag is
  signal evkbdb : time;
  signal wcsdrv : time;
  signal fcssmc : integer;
  signal wiiyhmovq : real;
  signal zeg : time;
  signal eosb : time;
  signal gtfmo : integer;
begin
  egwzskli : entity work.tjmvk
    port map (pgfg => gtfmo, qsdyn => eosb, dsqesk => zeg, wwpfmawui => wiiyhmovq);
  cnzgfzecrl : entity work.tjmvk
    port map (pgfg => fcssmc, qsdyn => wcsdrv, dsqesk => evkbdb, wwpfmawui => wiiyhmovq);
  
  -- Single-driven assignments
  wiiyhmovq <= 2#11.1#;
  rvqodzhxm <= rvqodzhxm;
  tiqulpwyh <= (others => 1332.2 ms);
end orrlzjautx;

library ieee;
use ieee.std_logic_1164.all;

entity gandk is
  port (yqgquy : out real; qkqfl : buffer time; gfn : linkage std_logic_vector(2 downto 0));
end gandk;

architecture azlsrkw of gandk is
  signal bin : boolean;
  signal mskeivs : bit_vector(3 to 2);
  signal syu : boolean;
  signal fcrelkin : time_vector(2 to 2);
  signal rpatt : boolean;
  signal rpvrvvqk : bit_vector(3 to 2);
  signal igmzh : boolean;
  signal dg : time_vector(2 to 2);
  signal jovsymcqys : real;
  signal oexkmgzst : time;
  signal hvy : integer;
begin
  vuqqbf : entity work.tjmvk
    port map (pgfg => hvy, qsdyn => oexkmgzst, dsqesk => qkqfl, wwpfmawui => jovsymcqys);
  g : entity work.jvag
    port map (tiqulpwyh => dg, lxohc => igmzh, rvqodzhxm => rpvrvvqk, yfadgygz => rpatt);
  jmnjfry : entity work.jvag
    port map (tiqulpwyh => fcrelkin, lxohc => syu, rvqodzhxm => mskeivs, yfadgygz => bin);
  
  -- Single-driven assignments
  yqgquy <= yqgquy;
  jovsymcqys <= 22.0;
end azlsrkw;

entity zmmbicb is
  port (eedeu : buffer boolean_vector(2 to 1); nzpuu : in integer; fvmdtnozcd : buffer time);
end zmmbicb;

library ieee;
use ieee.std_logic_1164.all;

architecture yxrgo of zmmbicb is
  signal qclncwpxcx : std_logic_vector(2 downto 0);
  signal jj : time;
  signal g : time;
  signal ajypflr : integer;
  signal cpvopsg : real;
  signal dqc : time;
  signal j : time;
  signal kblvuqktne : integer;
begin
  yabkcjpwj : entity work.tjmvk
    port map (pgfg => kblvuqktne, qsdyn => j, dsqesk => dqc, wwpfmawui => cpvopsg);
  hmksabsd : entity work.tjmvk
    port map (pgfg => ajypflr, qsdyn => g, dsqesk => jj, wwpfmawui => cpvopsg);
  exdz : entity work.gandk
    port map (yqgquy => cpvopsg, qkqfl => fvmdtnozcd, gfn => qclncwpxcx);
  
  -- Single-driven assignments
  eedeu <= eedeu;
  
  -- Multi-driven assignments
  qclncwpxcx <= ('U', 'U', '-');
end yxrgo;



-- Seed after: 9041486844133368570,499459191852795575
