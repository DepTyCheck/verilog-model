-- Seed: 11699421685229195972,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity y is
  port (hyigdicono : inout std_logic; xg : inout record_subtype_mirror);
end y;

architecture iaoprvo of y is
  
begin
  -- Multi-driven assignments
  hyigdicono <= 'L';
  hyigdicono <= '-';
end iaoprvo;

library ieee;
use ieee.std_logic_1164.all;

entity wjiarwznx is
  port (g : out integer; ovoinujh : buffer std_logic; cejtzpuoq : in time; o : out bit);
end wjiarwznx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture js of wjiarwznx is
  shared variable w : record_subtype_mirror;
  shared variable uusugfgn : record_subtype_mirror;
  signal zcgoliensl : std_logic;
  shared variable plndpqndu : record_subtype_mirror;
begin
  zqqzx : entity work.y
    port map (hyigdicono => ovoinujh, xg => plndpqndu);
  ejnbdijfjv : entity work.y
    port map (hyigdicono => zcgoliensl, xg => uusugfgn);
  cvm : entity work.y
    port map (hyigdicono => ovoinujh, xg => w);
  
  -- Multi-driven assignments
  zcgoliensl <= ovoinujh;
end js;



-- Seed after: 13982345812458042002,14426950258250697445
