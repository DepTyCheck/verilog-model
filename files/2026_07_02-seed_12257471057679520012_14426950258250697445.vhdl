-- Seed: 12257471057679520012,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ge is
  port (jrs : buffer integer; hnvhtcelj : in std_logic_vector(0 to 2); hygb : inout integer_subtype_mirror);
end ge;

architecture eini of ge is
  
begin
  -- Single-driven assignments
  jrs <= 16#0EC8A#;
end eini;

entity yaqkfxrmy is
  port (gxb : linkage boolean_vector(1 to 0));
end yaqkfxrmy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xiidumjog of yaqkfxrmy is
  shared variable x : integer_subtype_mirror;
  signal rhjqudgd : std_logic_vector(0 to 2);
  signal tpgwrwmczm : integer;
  shared variable rxlm : integer_subtype_mirror;
  signal nkooig : integer;
  shared variable fug : integer_subtype_mirror;
  signal obdvtmziw : integer;
  shared variable zlm : integer_subtype_mirror;
  signal fmjcews : std_logic_vector(0 to 2);
  signal gvrvlv : integer;
begin
  gyihrswwgh : entity work.ge
    port map (jrs => gvrvlv, hnvhtcelj => fmjcews, hygb => zlm);
  mh : entity work.ge
    port map (jrs => obdvtmziw, hnvhtcelj => fmjcews, hygb => fug);
  hwtvapp : entity work.ge
    port map (jrs => nkooig, hnvhtcelj => fmjcews, hygb => rxlm);
  phh : entity work.ge
    port map (jrs => tpgwrwmczm, hnvhtcelj => rhjqudgd, hygb => x);
  
  -- Multi-driven assignments
  rhjqudgd <= "0U1";
  rhjqudgd <= fmjcews;
  fmjcews <= "1HW";
  fmjcews <= "XL-";
end xiidumjog;



-- Seed after: 8641738232374018962,14426950258250697445
