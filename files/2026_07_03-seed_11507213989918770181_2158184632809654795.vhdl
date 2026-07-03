-- Seed: 11507213989918770181,2158184632809654795

use std.reflection.all;

entity f is
  port (xwbubsvnye : inout protected_subtype_mirror);
end f;

architecture pbatlwu of f is
  
begin
  
end pbatlwu;

use std.reflection.all;

entity ytzjdbern is
  port (vwkvp : linkage real_vector(0 to 0); fn : inout enumeration_value_mirror; xzhsbtwxw : inout access_subtype_mirror; guvlxtppje : out real);
end ytzjdbern;

use std.reflection.all;

architecture wqpjw of ytzjdbern is
  shared variable zxeng : protected_subtype_mirror;
  shared variable dxn : protected_subtype_mirror;
  shared variable ra : protected_subtype_mirror;
begin
  vci : entity work.f
    port map (xwbubsvnye => ra);
  ulhc : entity work.f
    port map (xwbubsvnye => dxn);
  byluvh : entity work.f
    port map (xwbubsvnye => zxeng);
  
  -- Single-driven assignments
  guvlxtppje <= 8#0_6_1.513#;
end wqpjw;

use std.reflection.all;

entity aboznbecq is
  port (qurn : inout enumeration_subtype_mirror);
end aboznbecq;

use std.reflection.all;

architecture seepsai of aboznbecq is
  shared variable fzabfpsa : protected_subtype_mirror;
  signal iggw : real;
  shared variable rjq : access_subtype_mirror;
  shared variable qxjdjhu : enumeration_value_mirror;
  signal hffsizwys : real_vector(0 to 0);
begin
  mmsfjebvy : entity work.ytzjdbern
    port map (vwkvp => hffsizwys, fn => qxjdjhu, xzhsbtwxw => rjq, guvlxtppje => iggw);
  zkal : entity work.f
    port map (xwbubsvnye => fzabfpsa);
end seepsai;

use std.reflection.all;

entity siacwz is
  port (kahlgg : inout record_value_mirror; nxx : inout floating_subtype_mirror);
end siacwz;

use std.reflection.all;

architecture vlu of siacwz is
  shared variable b : protected_subtype_mirror;
  shared variable opmksk : protected_subtype_mirror;
  shared variable kshgvczk : protected_subtype_mirror;
  signal uctia : real;
  shared variable vhevxfo : access_subtype_mirror;
  shared variable ptsimjoz : enumeration_value_mirror;
  signal xc : real_vector(0 to 0);
begin
  h : entity work.ytzjdbern
    port map (vwkvp => xc, fn => ptsimjoz, xzhsbtwxw => vhevxfo, guvlxtppje => uctia);
  wgustbdwdv : entity work.f
    port map (xwbubsvnye => kshgvczk);
  p : entity work.f
    port map (xwbubsvnye => opmksk);
  a : entity work.f
    port map (xwbubsvnye => b);
end vlu;



-- Seed after: 12925381537296341841,2158184632809654795
