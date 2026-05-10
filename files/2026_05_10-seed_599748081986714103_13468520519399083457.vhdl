-- Seed: 599748081986714103,13468520519399083457



entity erpcxmfy is
  port (hbeht : in time; bfmxbqyep : inout boolean; dlut : out integer);
end erpcxmfy;



architecture yhrdrqug of erpcxmfy is
  
begin
  
end yhrdrqug;



entity dxu is
  port (oipuaoibf : linkage real; qilvs : out time);
end dxu;



architecture quwr of dxu is
  signal qlk : integer;
  signal bjmhglbrys : boolean;
  signal phab : time;
  signal pqk : integer;
  signal jcug : boolean;
begin
  qrzsrywk : entity work.erpcxmfy
    port map (hbeht => qilvs, bfmxbqyep => jcug, dlut => pqk);
  ukwapcun : entity work.erpcxmfy
    port map (hbeht => phab, bfmxbqyep => bjmhglbrys, dlut => qlk);
end quwr;

library ieee;
use ieee.std_logic_1164.all;

entity dlszr is
  port (rtc : inout severity_level; gv : linkage std_logic; lidbmfw : out bit);
end dlszr;



architecture q of dlszr is
  signal nhvdaefx : integer;
  signal eo : boolean;
  signal aokzs : time;
  signal olobffq : integer;
  signal qpqzvq : boolean;
  signal x : time;
  signal wgz : real;
  signal c : integer;
  signal pakubipp : boolean;
  signal qwtvci : time;
begin
  hzjywjbapq : entity work.erpcxmfy
    port map (hbeht => qwtvci, bfmxbqyep => pakubipp, dlut => c);
  xvxhv : entity work.dxu
    port map (oipuaoibf => wgz, qilvs => x);
  ee : entity work.erpcxmfy
    port map (hbeht => x, bfmxbqyep => qpqzvq, dlut => olobffq);
  k : entity work.erpcxmfy
    port map (hbeht => aokzs, bfmxbqyep => eo, dlut => nhvdaefx);
end q;



-- Seed after: 6750334861567626208,13468520519399083457
