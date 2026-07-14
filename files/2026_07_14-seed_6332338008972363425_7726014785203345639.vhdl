-- Seed: 6332338008972363425,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity niitfhwums is
  port (xohgxanm : in std_logic; btt : linkage time);
end niitfhwums;

architecture ttqjl of niitfhwums is
  
begin
  
end ttqjl;

use std.reflection.all;

entity rqxnktny is
  port (rkqfjmpbmp : inout access_subtype_mirror; qobto : in boolean_vector(2 to 4); r : inout integer_value_mirror; xeaj : inout integer_value_mirror);
end rqxnktny;

library ieee;
use ieee.std_logic_1164.all;

architecture vb of rqxnktny is
  signal pe : time;
  signal zyvkmgmp : time;
  signal qdkwohpm : std_logic;
begin
  ccgepgrpz : entity work.niitfhwums
    port map (xohgxanm => qdkwohpm, btt => zyvkmgmp);
  tgfdu : entity work.niitfhwums
    port map (xohgxanm => qdkwohpm, btt => pe);
  
  -- Multi-driven assignments
  qdkwohpm <= 'U';
end vb;

use std.reflection.all;

entity mwtskx is
  port (a : inout protected_value_mirror);
end mwtskx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zwyyw of mwtskx is
  shared variable elodxi : integer_value_mirror;
  shared variable ndvion : integer_value_mirror;
  signal bt : boolean_vector(2 to 4);
  shared variable evff : access_subtype_mirror;
  signal unosgoy : time;
  signal gczsvppwq : std_logic;
  signal t : time;
  signal ll : std_logic;
  signal wqthuypyy : time;
  signal mhhob : std_logic;
begin
  vknlfuy : entity work.niitfhwums
    port map (xohgxanm => mhhob, btt => wqthuypyy);
  nlv : entity work.niitfhwums
    port map (xohgxanm => ll, btt => t);
  icikrtml : entity work.niitfhwums
    port map (xohgxanm => gczsvppwq, btt => unosgoy);
  vtrbay : entity work.rqxnktny
    port map (rkqfjmpbmp => evff, qobto => bt, r => ndvion, xeaj => elodxi);
  
  -- Multi-driven assignments
  mhhob <= '0';
  mhhob <= mhhob;
  mhhob <= 'W';
  mhhob <= '1';
end zwyyw;



-- Seed after: 10095144073096201636,7726014785203345639
