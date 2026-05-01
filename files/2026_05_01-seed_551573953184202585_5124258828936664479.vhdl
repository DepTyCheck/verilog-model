-- Seed: 551573953184202585,5124258828936664479



entity clfdwtxzi is
  port (lh : linkage real; ujcyh : in integer; caiqxc : inout real; wspmo : in time);
end clfdwtxzi;



architecture d of clfdwtxzi is
  
begin
  
end d;



entity gq is
  port (vstx : linkage integer; cbdobvpez : linkage severity_level);
end gq;



architecture gbjz of gq is
  signal phtyx : time;
  signal ml : integer;
  signal afz : real;
  signal dgesg : time;
  signal tdyvczvayt : real;
  signal zlqau : integer;
  signal xtutqwkpa : real;
begin
  copbqyxn : entity work.clfdwtxzi
    port map (lh => xtutqwkpa, ujcyh => zlqau, caiqxc => tdyvczvayt, wspmo => dgesg);
  ihnrmxxiky : entity work.clfdwtxzi
    port map (lh => afz, ujcyh => ml, caiqxc => xtutqwkpa, wspmo => phtyx);
  xxbjdk : entity work.clfdwtxzi
    port map (lh => xtutqwkpa, ujcyh => zlqau, caiqxc => afz, wspmo => phtyx);
end gbjz;

library ieee;
use ieee.std_logic_1164.all;

entity mxpmevfrh is
  port (axx : buffer std_logic);
end mxpmevfrh;



architecture wh of mxpmevfrh is
  signal vsbllnjd : real;
  signal hb : real;
  signal rj : time;
  signal mvspkl : integer;
  signal cjiv : real;
  signal oxyynuowpr : severity_level;
  signal l : integer;
begin
  yl : entity work.gq
    port map (vstx => l, cbdobvpez => oxyynuowpr);
  ixdwm : entity work.clfdwtxzi
    port map (lh => cjiv, ujcyh => mvspkl, caiqxc => cjiv, wspmo => rj);
  menzmxjwde : entity work.gq
    port map (vstx => mvspkl, cbdobvpez => oxyynuowpr);
  fpuizunws : entity work.clfdwtxzi
    port map (lh => hb, ujcyh => l, caiqxc => vsbllnjd, wspmo => rj);
end wh;



entity g is
  port (eysjz : inout time; dvzspfqheg : buffer integer);
end g;



architecture tzxs of g is
  signal r : time;
  signal uv : time;
  signal k : real;
  signal sle : real;
begin
  mqxkq : entity work.clfdwtxzi
    port map (lh => sle, ujcyh => dvzspfqheg, caiqxc => k, wspmo => uv);
  ltyv : entity work.clfdwtxzi
    port map (lh => sle, ujcyh => dvzspfqheg, caiqxc => sle, wspmo => r);
end tzxs;



-- Seed after: 13143177944098924780,5124258828936664479
