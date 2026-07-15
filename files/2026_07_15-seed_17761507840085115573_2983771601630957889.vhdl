-- Seed: 17761507840085115573,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jpnuunzm is
  port (variable pkoxbh : inout floating_subtype_mirror_pt; dtpyyv : buffer std_logic_vector(4 to 2));
end jpnuunzm;

architecture gykpjkiv of jpnuunzm is
  
begin
  
end gykpjkiv;

entity lvzfre is
  port (xtiit : out real; cdqjpmlgai : in bit_vector(0 downto 3));
end lvzfre;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lmma of lvzfre is
  shared variable zauttz : floating_subtype_mirror_pt;
  shared variable mizzrk : floating_subtype_mirror_pt;
  shared variable botjgef : floating_subtype_mirror_pt;
  signal dmmvkg : std_logic_vector(4 to 2);
  shared variable nv : floating_subtype_mirror_pt;
begin
  cb : entity work.jpnuunzm
    port map (pkoxbh => nv, dtpyyv => dmmvkg);
  mmklizafj : entity work.jpnuunzm
    port map (pkoxbh => botjgef, dtpyyv => dmmvkg);
  diswzkjif : entity work.jpnuunzm
    port map (pkoxbh => mizzrk, dtpyyv => dmmvkg);
  ujy : entity work.jpnuunzm
    port map (pkoxbh => zauttz, dtpyyv => dmmvkg);
  
  -- Single-driven assignments
  xtiit <= xtiit;
end lmma;



-- Seed after: 5730224284600527254,2983771601630957889
