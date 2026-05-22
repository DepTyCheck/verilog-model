-- Seed: 949317937599008969,8881251984038598933



entity lj is
  port (itnmn : out severity_level; ocsf : linkage character; wrsf : linkage time; c : inout integer);
end lj;



architecture hgrousbpc of lj is
  
begin
  
end hgrousbpc;



entity t is
  port (yctveyrwq : in time; vfxwzuj : out integer);
end t;



architecture zbia of t is
  signal wpuhayj : character;
  signal ympbu : severity_level;
  signal ljizs : integer;
  signal c : character;
  signal zckrfzrlr : severity_level;
  signal rdnzbhgv : integer;
  signal xykfprru : time;
  signal kalmxabq : character;
  signal p : severity_level;
begin
  kjlhnizoe : entity work.lj
    port map (itnmn => p, ocsf => kalmxabq, wrsf => xykfprru, c => rdnzbhgv);
  slhtetveuw : entity work.lj
    port map (itnmn => zckrfzrlr, ocsf => c, wrsf => yctveyrwq, c => ljizs);
  xaiwkf : entity work.lj
    port map (itnmn => ympbu, ocsf => wpuhayj, wrsf => yctveyrwq, c => vfxwzuj);
end zbia;

library ieee;
use ieee.std_logic_1164.all;

entity wydsjw is
  port (cutjlem : inout std_logic);
end wydsjw;



architecture ne of wydsjw is
  signal ysdoux : integer;
  signal jarfniuo : integer;
  signal h : integer;
  signal vxdoufbqep : time;
  signal pcvumc : character;
  signal wqddcz : severity_level;
begin
  jvwwut : entity work.lj
    port map (itnmn => wqddcz, ocsf => pcvumc, wrsf => vxdoufbqep, c => h);
  onivwb : entity work.t
    port map (yctveyrwq => vxdoufbqep, vfxwzuj => jarfniuo);
  jivjmbt : entity work.t
    port map (yctveyrwq => vxdoufbqep, vfxwzuj => ysdoux);
end ne;



entity wecb is
  port (wouavanl : out boolean; pjcgk : inout integer; vvqbwgruvf : in time; ethcmnx : out time);
end wecb;



architecture xca of wecb is
  signal ojndvpp : character;
  signal loyrkkg : severity_level;
begin
  zihdebphny : entity work.lj
    port map (itnmn => loyrkkg, ocsf => ojndvpp, wrsf => ethcmnx, c => pjcgk);
end xca;



-- Seed after: 10528780852374138137,8881251984038598933
