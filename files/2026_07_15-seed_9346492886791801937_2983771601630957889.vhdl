-- Seed: 9346492886791801937,2983771601630957889

use std.reflection.all;

entity kdx is
  port (xs : out real; variable qzzmtiufnp : inout subtype_mirror_pt; drzkbovkda : buffer integer);
end kdx;

architecture eiiachvgsz of kdx is
  
begin
  -- Single-driven assignments
  xs <= xs;
  drzkbovkda <= 3;
end eiiachvgsz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity x is
  port (jtmrtbqu : buffer integer; variable z : inout integer_subtype_mirror_pt; dhr : buffer std_logic);
end x;

architecture skcv of x is
  
begin
  -- Single-driven assignments
  jtmrtbqu <= jtmrtbqu;
  
  -- Multi-driven assignments
  dhr <= 'W';
end skcv;

library ieee;
use ieee.std_logic_1164.all;

entity ioh is
  port (fm : inout std_logic);
end ioh;

use std.reflection.all;

architecture ttnuyaiqll of ioh is
  signal u : integer;
  shared variable awdbyb : subtype_mirror_pt;
  signal aywz : real;
  signal fpaa : integer;
  shared variable twvrk : subtype_mirror_pt;
  signal rlbkn : real;
  signal dxs : integer;
  shared variable btpa : subtype_mirror_pt;
  signal xfqou : real;
  shared variable rrgshx : integer_subtype_mirror_pt;
  signal e : integer;
begin
  vm : entity work.x
    port map (jtmrtbqu => e, z => rrgshx, dhr => fm);
  unxbklgpb : entity work.kdx
    port map (xs => xfqou, qzzmtiufnp => btpa, drzkbovkda => dxs);
  sb : entity work.kdx
    port map (xs => rlbkn, qzzmtiufnp => twvrk, drzkbovkda => fpaa);
  kldmzde : entity work.kdx
    port map (xs => aywz, qzzmtiufnp => awdbyb, drzkbovkda => u);
  
  -- Multi-driven assignments
  fm <= fm;
  fm <= fm;
  fm <= 'W';
  fm <= 'U';
end ttnuyaiqll;



-- Seed after: 8665671041133311482,2983771601630957889
