-- Seed: 6310425317874804963,7726014785203345639

use std.reflection.all;

entity fshcozf is
  port (apzzojr : inout enumeration_subtype_mirror; fvhejwrzv : inout access_value_mirror; bc : inout access_value_mirror; dpcfg : buffer time);
end fshcozf;

architecture r of fshcozf is
  
begin
  -- Single-driven assignments
  dpcfg <= 8#054# us;
end r;

use std.reflection.all;

entity rx is
  port (xjgabu : out time; yqzl : in integer; u : inout subtype_mirror);
end rx;

use std.reflection.all;

architecture t of rx is
  signal zmkrkb : time;
  shared variable kspyyneif : access_value_mirror;
  shared variable vejfqkuv : access_value_mirror;
  shared variable meuradpdh : enumeration_subtype_mirror;
  signal ufnelqoq : time;
  shared variable tczosim : access_value_mirror;
  shared variable omlnegh : access_value_mirror;
  shared variable tpma : enumeration_subtype_mirror;
begin
  cponon : entity work.fshcozf
    port map (apzzojr => tpma, fvhejwrzv => omlnegh, bc => tczosim, dpcfg => ufnelqoq);
  iqn : entity work.fshcozf
    port map (apzzojr => meuradpdh, fvhejwrzv => vejfqkuv, bc => kspyyneif, dpcfg => zmkrkb);
end t;

entity bxmu is
  port (oukk : inout time);
end bxmu;

use std.reflection.all;

architecture vw of bxmu is
  signal fdals : time;
  shared variable yulyamimo : access_value_mirror;
  shared variable jwu : access_value_mirror;
  shared variable oujghvj : enumeration_subtype_mirror;
begin
  nxl : entity work.fshcozf
    port map (apzzojr => oujghvj, fvhejwrzv => jwu, bc => yulyamimo, dpcfg => fdals);
  
  -- Single-driven assignments
  oukk <= oukk;
end vw;

entity k is
  port (qizpejpcnk : in real; tv : out real);
end k;

use std.reflection.all;

architecture ta of k is
  signal qvvnlbf : time;
  shared variable dkfiognrbv : access_value_mirror;
  shared variable viop : access_value_mirror;
  shared variable ckncctoxi : enumeration_subtype_mirror;
  signal c : time;
  shared variable datzd : access_value_mirror;
  shared variable dvyk : access_value_mirror;
  shared variable xdpmcobo : enumeration_subtype_mirror;
  signal pdgv : time;
  shared variable jzwwycbao : access_value_mirror;
  shared variable fnoj : access_value_mirror;
  shared variable qv : enumeration_subtype_mirror;
  signal qhngfdbcwl : time;
  shared variable tbwfbwk : access_value_mirror;
  shared variable bvetmciz : access_value_mirror;
  shared variable ajrutyzvxh : enumeration_subtype_mirror;
begin
  icbpygj : entity work.fshcozf
    port map (apzzojr => ajrutyzvxh, fvhejwrzv => bvetmciz, bc => tbwfbwk, dpcfg => qhngfdbcwl);
  ehbyo : entity work.fshcozf
    port map (apzzojr => qv, fvhejwrzv => fnoj, bc => jzwwycbao, dpcfg => pdgv);
  xgddvir : entity work.fshcozf
    port map (apzzojr => xdpmcobo, fvhejwrzv => dvyk, bc => datzd, dpcfg => c);
  lenauwsmr : entity work.fshcozf
    port map (apzzojr => ckncctoxi, fvhejwrzv => viop, bc => dkfiognrbv, dpcfg => qvvnlbf);
  
  -- Single-driven assignments
  tv <= 2#11.1_0_0_0_1#;
end ta;



-- Seed after: 4589443688370069968,7726014785203345639
