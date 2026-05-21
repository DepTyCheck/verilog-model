-- Seed: 12676038898405890397,11460338385561364835

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (k : linkage time; xtgrmve : out time; yqp : linkage std_logic; owgbm : buffer integer);
end g;



architecture fja of g is
  
begin
  
end fja;



entity ves is
  port (vvylrq : inout real; f : in real; d : in bit);
end ves;

library ieee;
use ieee.std_logic_1164.all;

architecture iyqawro of ves is
  signal eypoyrrg : integer;
  signal wtsllflojr : std_logic;
  signal xremayc : time;
  signal eg : integer;
  signal rhtidpl : std_logic;
  signal lcgojqi : time;
begin
  flo : entity work.g
    port map (k => lcgojqi, xtgrmve => lcgojqi, yqp => rhtidpl, owgbm => eg);
  atzzjdru : entity work.g
    port map (k => lcgojqi, xtgrmve => xremayc, yqp => wtsllflojr, owgbm => eypoyrrg);
end iyqawro;



entity sdb is
  port (k : out real; gqiidck : out real);
end sdb;

library ieee;
use ieee.std_logic_1164.all;

architecture sagtdha of sdb is
  signal fgbxbk : bit;
  signal ejsxcbxpow : real;
  signal mmgzmyzx : real;
  signal lhmr : integer;
  signal oyyojzzeax : std_logic;
  signal cbtvmpkna : time;
  signal cdebgt : bit;
begin
  yg : entity work.ves
    port map (vvylrq => k, f => gqiidck, d => cdebgt);
  teigxakyc : entity work.g
    port map (k => cbtvmpkna, xtgrmve => cbtvmpkna, yqp => oyyojzzeax, owgbm => lhmr);
  ftshyxol : entity work.ves
    port map (vvylrq => mmgzmyzx, f => ejsxcbxpow, d => fgbxbk);
end sagtdha;



entity gtr is
  port (z : in boolean);
end gtr;

library ieee;
use ieee.std_logic_1164.all;

architecture xjjcwdekfo of gtr is
  signal ngyb : integer;
  signal xevkrd : std_logic;
  signal xdltf : time;
  signal u : integer;
  signal tssduj : std_logic;
  signal pxx : real;
  signal kacr : real;
  signal zlxwvgn : integer;
  signal jxhbxi : std_logic;
  signal qx : time;
  signal jcqzfuvpxh : time;
begin
  xbmridxf : entity work.g
    port map (k => jcqzfuvpxh, xtgrmve => qx, yqp => jxhbxi, owgbm => zlxwvgn);
  wrh : entity work.sdb
    port map (k => kacr, gqiidck => pxx);
  tfb : entity work.g
    port map (k => jcqzfuvpxh, xtgrmve => jcqzfuvpxh, yqp => tssduj, owgbm => u);
  owr : entity work.g
    port map (k => jcqzfuvpxh, xtgrmve => xdltf, yqp => xevkrd, owgbm => ngyb);
end xjjcwdekfo;



-- Seed after: 3180263204705109518,11460338385561364835
