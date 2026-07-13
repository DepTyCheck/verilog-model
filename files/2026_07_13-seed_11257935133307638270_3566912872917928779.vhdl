-- Seed: 11257935133307638270,3566912872917928779

use std.reflection.all;

entity kca is
  port (ok : inout physical_value_mirror; g : inout file_subtype_mirror);
end kca;

architecture odmclet of kca is
  
begin
  
end odmclet;

use std.reflection.all;

entity blctuwzbt is
  port (yuixl : inout protected_value_mirror; xtn : out time);
end blctuwzbt;

use std.reflection.all;

architecture tngvjkbfuf of blctuwzbt is
  shared variable fkzcz : file_subtype_mirror;
  shared variable rpvxhllcgw : physical_value_mirror;
  shared variable iy : file_subtype_mirror;
  shared variable kybly : physical_value_mirror;
  shared variable uai : file_subtype_mirror;
  shared variable l : physical_value_mirror;
begin
  lgfkw : entity work.kca
    port map (ok => l, g => uai);
  ymc : entity work.kca
    port map (ok => kybly, g => iy);
  juwakn : entity work.kca
    port map (ok => rpvxhllcgw, g => fkzcz);
  
  -- Single-driven assignments
  xtn <= 3 sec;
end tngvjkbfuf;



-- Seed after: 8900331790474766326,3566912872917928779
