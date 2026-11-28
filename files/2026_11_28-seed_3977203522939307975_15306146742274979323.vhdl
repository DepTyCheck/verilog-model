-- Seed: 3977203522939307975,15306146742274979323

entity qwiz is
  port (cztyyjxe : inout integer);
end qwiz;

architecture ihozrjydbg of qwiz is
  
begin
  -- Single-driven assignments
  cztyyjxe <= 1_0_4_4;
end ihozrjydbg;

entity abni is
  port (x : inout integer);
end abni;

architecture xkkvgrau of abni is
  signal xivlb : integer;
  signal hzjlx : integer;
  signal aokeatz : integer;
begin
  yegt : entity work.qwiz
    port map (cztyyjxe => aokeatz);
  pkqwmk : entity work.qwiz
    port map (cztyyjxe => hzjlx);
  xjlchb : entity work.qwiz
    port map (cztyyjxe => xivlb);
  al : entity work.qwiz
    port map (cztyyjxe => x);
end xkkvgrau;

library ieee;
use ieee.std_logic_1164.all;

entity ggezwyct is
  port (mzcvfyjtuy : in integer; i : in time; ugxcdeib : in time; qrjezloqw : buffer std_logic);
end ggezwyct;

architecture agcq of ggezwyct is
  signal nfjejtoqye : integer;
  signal motuscd : integer;
begin
  nxf : entity work.qwiz
    port map (cztyyjxe => motuscd);
  rgn : entity work.qwiz
    port map (cztyyjxe => nfjejtoqye);
  
  -- Multi-driven assignments
  qrjezloqw <= 'Z';
  qrjezloqw <= qrjezloqw;
  qrjezloqw <= 'X';
  qrjezloqw <= qrjezloqw;
end agcq;



-- Seed after: 13640598348630352229,15306146742274979323
