-- Seed: 16966044230876374355,3566912872917928779

use std.reflection.all;

entity olzdaq is
  port (ajc : inout record_subtype_mirror; eszoqnjcsl : inout access_subtype_mirror; ur : in integer; bgqqlkjioo : inout file_subtype_mirror);
end olzdaq;

architecture ax of olzdaq is
  
begin
  
end ax;

use std.reflection.all;

entity hfxfwp is
  port (bljrua : buffer bit; soincdei : inout access_value_mirror; r : linkage time; oyxpw : inout access_value_mirror);
end hfxfwp;

use std.reflection.all;

architecture tv of hfxfwp is
  shared variable bersq : file_subtype_mirror;
  shared variable ewn : access_subtype_mirror;
  shared variable pwjskn : record_subtype_mirror;
  shared variable otssgeq : file_subtype_mirror;
  signal xuqepue : integer;
  shared variable cix : access_subtype_mirror;
  shared variable x : record_subtype_mirror;
begin
  pzvmjhc : entity work.olzdaq
    port map (ajc => x, eszoqnjcsl => cix, ur => xuqepue, bgqqlkjioo => otssgeq);
  pisbcpty : entity work.olzdaq
    port map (ajc => pwjskn, eszoqnjcsl => ewn, ur => xuqepue, bgqqlkjioo => bersq);
  
  -- Single-driven assignments
  xuqepue <= xuqepue;
  bljrua <= bljrua;
end tv;



-- Seed after: 17749472455690057001,3566912872917928779
