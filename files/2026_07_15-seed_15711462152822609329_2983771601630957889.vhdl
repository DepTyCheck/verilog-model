-- Seed: 15711462152822609329,2983771601630957889

use std.reflection.all;

entity yuftk is
  port (variable hd : inout physical_subtype_mirror_pt; variable o : inout value_mirror_pt; i : linkage time);
end yuftk;

architecture hsktg of yuftk is
  
begin
  
end hsktg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity adurzkyua is
  port (ffvplv : linkage std_logic_vector(0 to 1); variable tikzggje : inout value_mirror_pt);
end adurzkyua;

use std.reflection.all;

architecture pnxjp of adurzkyua is
  signal qemockspq : time;
  shared variable oqsm : physical_subtype_mirror_pt;
  signal utmnpzuhls : time;
  shared variable vmlkd : value_mirror_pt;
  shared variable i : physical_subtype_mirror_pt;
  signal mmg : time;
  shared variable wtsjhknhe : value_mirror_pt;
  shared variable locwbk : physical_subtype_mirror_pt;
begin
  obqf : entity work.yuftk
    port map (hd => locwbk, o => wtsjhknhe, i => mmg);
  myhhe : entity work.yuftk
    port map (hd => i, o => vmlkd, i => utmnpzuhls);
  rbf : entity work.yuftk
    port map (hd => oqsm, o => tikzggje, i => qemockspq);
end pnxjp;

use std.reflection.all;

entity qapllfx is
  port (variable jdmeuttze : inout record_value_mirror_pt);
end qapllfx;

use std.reflection.all;

architecture sysmu of qapllfx is
  signal t : time;
  shared variable za : value_mirror_pt;
  shared variable mqaxw : physical_subtype_mirror_pt;
begin
  dtc : entity work.yuftk
    port map (hd => mqaxw, o => za, i => t);
end sysmu;

use std.reflection.all;

entity faclqabyi is
  port (variable ewjqktom : inout enumeration_value_mirror_pt; variable foroe : inout floating_value_mirror_pt);
end faclqabyi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pxnpcze of faclqabyi is
  shared variable v : value_mirror_pt;
  signal b : std_logic_vector(0 to 1);
  shared variable xig : record_value_mirror_pt;
begin
  ghsjxarkwu : entity work.qapllfx
    port map (jdmeuttze => xig);
  kmfuorpy : entity work.adurzkyua
    port map (ffvplv => b, tikzggje => v);
  
  -- Multi-driven assignments
  b <= ('-', '-');
  b <= "01";
end pxnpcze;



-- Seed after: 11113004443810867575,2983771601630957889
