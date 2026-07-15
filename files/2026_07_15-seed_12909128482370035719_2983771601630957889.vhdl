-- Seed: 12909128482370035719,2983771601630957889

entity eyvopg is
  port (amgbf : inout integer; sr : in character);
end eyvopg;

architecture efsndytjal of eyvopg is
  
begin
  
end efsndytjal;

entity njqlndkqkc is
  port (yjvtrs : in time);
end njqlndkqkc;

architecture f of njqlndkqkc is
  signal vzma : character;
  signal we : integer;
  signal thfgborfb : character;
  signal dclchqfpl : integer;
begin
  mf : entity work.eyvopg
    port map (amgbf => dclchqfpl, sr => thfgborfb);
  w : entity work.eyvopg
    port map (amgbf => we, sr => vzma);
end f;

use std.reflection.all;

entity b is
  port (ima : in integer; variable xki : inout array_value_mirror_pt; variable fub : inout file_value_mirror_pt; lqymccqoq : linkage character);
end b;

architecture dtwvhftw of b is
  signal gdou : time;
  signal ilnoorjys : character;
  signal yqtkgxhbbj : integer;
  signal rxpqw : character;
  signal mgtcauq : integer;
begin
  xskded : entity work.eyvopg
    port map (amgbf => mgtcauq, sr => rxpqw);
  fqew : entity work.eyvopg
    port map (amgbf => yqtkgxhbbj, sr => ilnoorjys);
  mmfxf : entity work.njqlndkqkc
    port map (yjvtrs => gdou);
end dtwvhftw;

use std.reflection.all;

entity mm is
  port (variable a : inout value_mirror_pt);
end mm;

use std.reflection.all;

architecture qy of mm is
  signal miqoysw : character;
  shared variable yk : file_value_mirror_pt;
  shared variable uo : array_value_mirror_pt;
  signal nggcungbo : integer;
  signal mk : character;
  signal fku : integer;
  signal bjfsjeju : character;
  signal mqnb : integer;
begin
  ugu : entity work.eyvopg
    port map (amgbf => mqnb, sr => bjfsjeju);
  kabaifll : entity work.eyvopg
    port map (amgbf => fku, sr => mk);
  vxc : entity work.b
    port map (ima => nggcungbo, xki => uo, fub => yk, lqymccqoq => bjfsjeju);
  ladyo : entity work.eyvopg
    port map (amgbf => nggcungbo, sr => miqoysw);
  
  -- Single-driven assignments
  mk <= bjfsjeju;
  miqoysw <= 'w';
end qy;



-- Seed after: 12572467768762038220,2983771601630957889
