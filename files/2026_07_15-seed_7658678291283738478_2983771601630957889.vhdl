-- Seed: 7658678291283738478,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;

entity hod is
  port (vfzsqp : in time; dpjq : in integer; btytwqrl : linkage std_logic);
end hod;

architecture nfgrj of hod is
  
begin
  
end nfgrj;

use std.reflection.all;

entity xkcondf is
  port (variable as : inout floating_value_mirror_pt; pyl : inout bit);
end xkcondf;

library ieee;
use ieee.std_logic_1164.all;

architecture sctws of xkcondf is
  signal rjiidkjz : std_logic;
  signal x : std_logic;
  signal jljnsbfn : integer;
  signal cm : integer;
  signal ri : time;
  signal zodbfzx : std_logic;
  signal utcn : integer;
  signal k : time;
begin
  vcoqhkj : entity work.hod
    port map (vfzsqp => k, dpjq => utcn, btytwqrl => zodbfzx);
  kkwluih : entity work.hod
    port map (vfzsqp => ri, dpjq => cm, btytwqrl => zodbfzx);
  js : entity work.hod
    port map (vfzsqp => k, dpjq => jljnsbfn, btytwqrl => x);
  kpafewuv : entity work.hod
    port map (vfzsqp => k, dpjq => utcn, btytwqrl => rjiidkjz);
  
  -- Single-driven assignments
  pyl <= '1';
  k <= k;
  
  -- Multi-driven assignments
  x <= zodbfzx;
end sctws;



-- Seed after: 16827603712440552452,2983771601630957889
