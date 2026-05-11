-- Seed: 17718226955520970749,9372850727389630639



entity zypgl is
  port (oynxwwqkw : in integer; mdmybqoh : out integer; asmbzsh : in real; fstx : linkage severity_level);
end zypgl;



architecture v of zypgl is
  
begin
  
end v;

library ieee;
use ieee.std_logic_1164.all;

entity fdtltct is
  port (gfhzxoo : linkage std_logic; seiqvdkiac : linkage real; ijjutjg : linkage real);
end fdtltct;



architecture mqc of fdtltct is
  signal vmxxnd : real;
  signal infnbiwg : integer;
  signal rsvgcv : integer;
  signal nnhdwhg : severity_level;
  signal fcotfhy : real;
  signal xxwu : integer;
  signal fg : integer;
begin
  zw : entity work.zypgl
    port map (oynxwwqkw => fg, mdmybqoh => xxwu, asmbzsh => fcotfhy, fstx => nnhdwhg);
  aek : entity work.zypgl
    port map (oynxwwqkw => xxwu, mdmybqoh => rsvgcv, asmbzsh => fcotfhy, fstx => nnhdwhg);
  u : entity work.zypgl
    port map (oynxwwqkw => infnbiwg, mdmybqoh => fg, asmbzsh => vmxxnd, fstx => nnhdwhg);
end mqc;



entity o is
  port (lcrqvooa : inout integer; b : out integer; bbtiyiw : out time; xoecogzfu : linkage integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture qfrsixdpdg of o is
  signal yyjll : integer;
  signal cpgzier : real;
  signal nwyzrg : std_logic;
  signal vh : severity_level;
  signal glkwtpgz : real;
  signal eqicpewmq : integer;
begin
  gauk : entity work.zypgl
    port map (oynxwwqkw => eqicpewmq, mdmybqoh => b, asmbzsh => glkwtpgz, fstx => vh);
  zfdxdyw : entity work.fdtltct
    port map (gfhzxoo => nwyzrg, seiqvdkiac => cpgzier, ijjutjg => glkwtpgz);
  s : entity work.zypgl
    port map (oynxwwqkw => b, mdmybqoh => yyjll, asmbzsh => cpgzier, fstx => vh);
end qfrsixdpdg;



entity goewx is
  port (andelas : in time; jqvwafimo : buffer real; p : inout time);
end goewx;

library ieee;
use ieee.std_logic_1164.all;

architecture sz of goewx is
  signal byqi : real;
  signal j : real;
  signal imhxgdg : std_logic;
  signal jwlslpbiu : integer;
  signal xvvvncyowz : time;
  signal qtk : integer;
  signal bma : integer;
  signal kbuvj : severity_level;
  signal zckfm : real;
  signal aearvqjras : integer;
  signal v : integer;
begin
  kxsruzald : entity work.zypgl
    port map (oynxwwqkw => v, mdmybqoh => aearvqjras, asmbzsh => zckfm, fstx => kbuvj);
  kol : entity work.o
    port map (lcrqvooa => bma, b => qtk, bbtiyiw => xvvvncyowz, xoecogzfu => jwlslpbiu);
  ddb : entity work.o
    port map (lcrqvooa => v, b => jwlslpbiu, bbtiyiw => p, xoecogzfu => aearvqjras);
  e : entity work.fdtltct
    port map (gfhzxoo => imhxgdg, seiqvdkiac => j, ijjutjg => byqi);
end sz;



-- Seed after: 17881616315064891696,9372850727389630639
