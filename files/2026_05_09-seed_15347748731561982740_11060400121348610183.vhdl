-- Seed: 15347748731561982740,11060400121348610183



entity xwyeffyll is
  port (uiw : in integer; vtt : in time; hujdbji : out time);
end xwyeffyll;



architecture otfcgufgt of xwyeffyll is
  
begin
  
end otfcgufgt;



entity cftwbvolby is
  port (wy : inout integer; vuqz : buffer integer);
end cftwbvolby;



architecture vpor of cftwbvolby is
  signal inresmnm : time;
  signal l : time;
begin
  jyt : entity work.xwyeffyll
    port map (uiw => wy, vtt => l, hujdbji => inresmnm);
end vpor;

library ieee;
use ieee.std_logic_1164.all;

entity ko is
  port (bb : linkage boolean; ksx : buffer real; jktpmkgcxo : in std_logic; kjyierjel : inout time);
end ko;



architecture u of ko is
  signal sdowuvthc : time;
  signal sjonmdr : integer;
  signal wmouoabvyj : integer;
  signal g : integer;
  signal pyaivw : integer;
begin
  ndxhn : entity work.xwyeffyll
    port map (uiw => pyaivw, vtt => kjyierjel, hujdbji => kjyierjel);
  egsxong : entity work.cftwbvolby
    port map (wy => g, vuqz => wmouoabvyj);
  uyj : entity work.cftwbvolby
    port map (wy => pyaivw, vuqz => sjonmdr);
  lkbp : entity work.xwyeffyll
    port map (uiw => g, vtt => sdowuvthc, hujdbji => sdowuvthc);
end u;



entity czpv is
  port (bqkudlhvu : in time);
end czpv;

library ieee;
use ieee.std_logic_1164.all;

architecture ieocrq of czpv is
  signal aswyaz : time;
  signal nerquolre : std_logic;
  signal znjfa : real;
  signal sdoxz : boolean;
begin
  bofdborf : entity work.ko
    port map (bb => sdoxz, ksx => znjfa, jktpmkgcxo => nerquolre, kjyierjel => aswyaz);
end ieocrq;



-- Seed after: 17500143231687492308,11060400121348610183
