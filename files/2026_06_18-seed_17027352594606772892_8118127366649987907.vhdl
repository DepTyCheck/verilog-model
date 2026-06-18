-- Seed: 17027352594606772892,8118127366649987907

entity kwlwsj is
  port (wxwj : inout bit; qhryaxxom : in severity_level; ipabvebvy : buffer time);
end kwlwsj;

architecture cgnxyi of kwlwsj is
  
begin
  -- Single-driven assignments
  wxwj <= '1';
  ipabvebvy <= 1 min;
end cgnxyi;

library ieee;
use ieee.std_logic_1164.all;

entity akvvil is
  port (savhsahyqz : inout std_logic; ij : linkage real; adkvglnrnh : out std_logic);
end akvvil;

architecture flmgg of akvvil is
  signal ela : time;
  signal gnfipku : severity_level;
  signal suwzo : bit;
  signal qfzouxde : time;
  signal jorceeh : bit;
  signal rk : time;
  signal ouz : severity_level;
  signal sdxywgdt : bit;
  signal peno : time;
  signal fqt : severity_level;
  signal mcmzqwxud : bit;
begin
  veni : entity work.kwlwsj
    port map (wxwj => mcmzqwxud, qhryaxxom => fqt, ipabvebvy => peno);
  xswmttk : entity work.kwlwsj
    port map (wxwj => sdxywgdt, qhryaxxom => ouz, ipabvebvy => rk);
  upggxvnuix : entity work.kwlwsj
    port map (wxwj => jorceeh, qhryaxxom => fqt, ipabvebvy => qfzouxde);
  qmyp : entity work.kwlwsj
    port map (wxwj => suwzo, qhryaxxom => gnfipku, ipabvebvy => ela);
  
  -- Single-driven assignments
  fqt <= ERROR;
  
  -- Multi-driven assignments
  adkvglnrnh <= '1';
  adkvglnrnh <= 'W';
  adkvglnrnh <= '0';
end flmgg;



-- Seed after: 4770560763901081963,8118127366649987907
