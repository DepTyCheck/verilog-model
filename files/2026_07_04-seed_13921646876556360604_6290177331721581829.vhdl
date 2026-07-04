-- Seed: 13921646876556360604,6290177331721581829

use std.reflection.all;

entity l is
  port (iguefzth : inout record_subtype_mirror; ikk : buffer real; ktwytg : inout time; k : inout physical_subtype_mirror);
end l;

architecture qyjisgy of l is
  
begin
  
end qyjisgy;

use std.reflection.all;

entity sskfbxfzr is
  port (wha : inout file_value_mirror);
end sskfbxfzr;

use std.reflection.all;

architecture urqoqxui of sskfbxfzr is
  shared variable dsbqmljvdu : physical_subtype_mirror;
  signal gyh : time;
  signal bilmno : real;
  shared variable fplaujtv : record_subtype_mirror;
  shared variable pfcbj : physical_subtype_mirror;
  signal ahyaqqsa : time;
  signal sofsriulel : real;
  shared variable fe : record_subtype_mirror;
  shared variable yafvvk : physical_subtype_mirror;
  signal pcfdov : time;
  signal ammwfsog : real;
  shared variable vhg : record_subtype_mirror;
begin
  fuxykl : entity work.l
    port map (iguefzth => vhg, ikk => ammwfsog, ktwytg => pcfdov, k => yafvvk);
  i : entity work.l
    port map (iguefzth => fe, ikk => sofsriulel, ktwytg => ahyaqqsa, k => pfcbj);
  yijy : entity work.l
    port map (iguefzth => fplaujtv, ikk => bilmno, ktwytg => gyh, k => dsbqmljvdu);
end urqoqxui;

entity rvzbfywt is
  port (ozz : in integer; e : inout integer);
end rvzbfywt;

architecture ghtyhl of rvzbfywt is
  
begin
  -- Single-driven assignments
  e <= 1_4_2;
end ghtyhl;

use std.reflection.all;

entity letgmynx is
  port (utb : inout array_value_mirror);
end letgmynx;

use std.reflection.all;

architecture c of letgmynx is
  shared variable r : file_value_mirror;
  shared variable rs : physical_subtype_mirror;
  signal zpjpizx : time;
  signal slpzawvjn : real;
  shared variable bz : record_subtype_mirror;
begin
  u : entity work.l
    port map (iguefzth => bz, ikk => slpzawvjn, ktwytg => zpjpizx, k => rs);
  tctju : entity work.sskfbxfzr
    port map (wha => r);
end c;



-- Seed after: 4794360513041715770,6290177331721581829
