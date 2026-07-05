-- Seed: 9476486697608340932,3181554006726329157

use std.reflection.all;

entity xumfg is
  port (bxqpcjiaw : inout array_subtype_mirror; xfjqwefa : inout array_value_mirror; b : inout protected_value_mirror; tfddlx : linkage bit);
end xumfg;

architecture e of xumfg is
  
begin
  
end e;

use std.reflection.all;

entity tuyzmtj is
  port (w : inout subtype_mirror; yhx : inout enumeration_subtype_mirror; wwlrdrtmmm : inout record_value_mirror);
end tuyzmtj;

architecture sz of tuyzmtj is
  
begin
  
end sz;

use std.reflection.all;

entity iqmg is
  port (agzbxh : inout enumeration_subtype_mirror);
end iqmg;

use std.reflection.all;

architecture npvdghksv of iqmg is
  signal kkizwyhx : bit;
  shared variable mungsqml : protected_value_mirror;
  shared variable vnyvxrkj : array_value_mirror;
  shared variable tgohkkn : array_subtype_mirror;
begin
  n : entity work.xumfg
    port map (bxqpcjiaw => tgohkkn, xfjqwefa => vnyvxrkj, b => mungsqml, tfddlx => kkizwyhx);
end npvdghksv;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (tmz : in std_logic; pvlnggyro : buffer integer);
end z;

use std.reflection.all;

architecture gcpelyd of z is
  signal maksccmcy : bit;
  shared variable cjehz : protected_value_mirror;
  shared variable lq : array_value_mirror;
  shared variable behly : array_subtype_mirror;
  signal o : bit;
  shared variable sttvcy : protected_value_mirror;
  shared variable grikkxfpwv : array_value_mirror;
  shared variable ap : array_subtype_mirror;
  shared variable zxdg : enumeration_subtype_mirror;
  shared variable ucrivfkh : record_value_mirror;
  shared variable inzlcajl : enumeration_subtype_mirror;
  shared variable ak : subtype_mirror;
begin
  zsohq : entity work.tuyzmtj
    port map (w => ak, yhx => inzlcajl, wwlrdrtmmm => ucrivfkh);
  pmsecv : entity work.iqmg
    port map (agzbxh => zxdg);
  waezjh : entity work.xumfg
    port map (bxqpcjiaw => ap, xfjqwefa => grikkxfpwv, b => sttvcy, tfddlx => o);
  cpeq : entity work.xumfg
    port map (bxqpcjiaw => behly, xfjqwefa => lq, b => cjehz, tfddlx => maksccmcy);
end gcpelyd;



-- Seed after: 5370372962544579379,3181554006726329157
