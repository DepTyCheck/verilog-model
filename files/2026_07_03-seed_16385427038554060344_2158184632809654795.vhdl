-- Seed: 16385427038554060344,2158184632809654795

use std.reflection.all;

entity y is
  port (fcn : out integer_vector(3 to 2); brlmtrj : inout time; iwt : inout enumeration_subtype_mirror);
end y;

architecture wpbef of y is
  
begin
  -- Single-driven assignments
  brlmtrj <= 42412 ms;
end wpbef;

use std.reflection.all;

entity emvsxe is
  port (mecgvjnfy : inout floating_value_mirror; yxdrf : inout floating_value_mirror; qzytq : linkage integer; mel : inout access_subtype_mirror);
end emvsxe;

use std.reflection.all;

architecture ilqtezqij of emvsxe is
  shared variable lfnbskkun : enumeration_subtype_mirror;
  signal mxcccz : time;
  signal lahjgzdawr : integer_vector(3 to 2);
  shared variable pvyhusyib : enumeration_subtype_mirror;
  signal gtxnqhjl : time;
  signal iimxdnlyue : integer_vector(3 to 2);
begin
  h : entity work.y
    port map (fcn => iimxdnlyue, brlmtrj => gtxnqhjl, iwt => pvyhusyib);
  nzcasn : entity work.y
    port map (fcn => lahjgzdawr, brlmtrj => mxcccz, iwt => lfnbskkun);
end ilqtezqij;



-- Seed after: 18046396350955284180,2158184632809654795
