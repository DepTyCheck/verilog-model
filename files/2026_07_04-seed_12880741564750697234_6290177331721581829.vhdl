-- Seed: 12880741564750697234,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;

entity xzjex is
  port (daa : buffer std_logic);
end xzjex;

architecture hoiqx of xzjex is
  
begin
  -- Multi-driven assignments
  daa <= 'L';
  daa <= daa;
  daa <= 'U';
  daa <= '1';
end hoiqx;

use std.reflection.all;

entity uunbxwsvre is
  port (xqmulafi : inout access_subtype_mirror);
end uunbxwsvre;

architecture l of uunbxwsvre is
  
begin
  
end l;

library ieee;
use ieee.std_logic_1164.all;

entity iobjoaqq is
  port (ufznf : inout std_logic; eiurwtan : linkage real);
end iobjoaqq;

library ieee;
use ieee.std_logic_1164.all;

architecture zbaa of iobjoaqq is
  signal nyfzgz : std_logic;
  signal tsz : std_logic;
begin
  dyzllgckft : entity work.xzjex
    port map (daa => tsz);
  fydrhok : entity work.xzjex
    port map (daa => nyfzgz);
end zbaa;

entity bbkwv is
  port (jcmwshlxbj : out integer);
end bbkwv;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mbevgi of bbkwv is
  signal tsm : real;
  shared variable tbhrc : access_subtype_mirror;
  signal oaycy : std_logic;
begin
  brxskt : entity work.xzjex
    port map (daa => oaycy);
  upqkfunmqp : entity work.uunbxwsvre
    port map (xqmulafi => tbhrc);
  kjvsvziivb : entity work.iobjoaqq
    port map (ufznf => oaycy, eiurwtan => tsm);
  
  -- Single-driven assignments
  jcmwshlxbj <= jcmwshlxbj;
  
  -- Multi-driven assignments
  oaycy <= oaycy;
  oaycy <= 'Z';
  oaycy <= 'X';
end mbevgi;



-- Seed after: 18068426888353377384,6290177331721581829
