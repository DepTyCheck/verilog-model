-- Seed: 8088165459672319978,7726014785203345639

use std.reflection.all;

entity vikgxtryx is
  port (zozqsdgh : inout floating_subtype_mirror; azxzlfte : inout enumeration_value_mirror; bewxbyqxhl : inout time; cc : inout value_mirror);
end vikgxtryx;

architecture indjfcoqo of vikgxtryx is
  
begin
  -- Single-driven assignments
  bewxbyqxhl <= 16#A_8.7_0# us;
end indjfcoqo;

use std.reflection.all;

entity bdcsxgfb is
  port (wbsmivgh : inout enumeration_subtype_mirror; gst : buffer time_vector(2 to 3); yjivzp : inout floating_subtype_mirror);
end bdcsxgfb;

use std.reflection.all;

architecture wkt of bdcsxgfb is
  shared variable gkpgxs : value_mirror;
  signal yzl : time;
  shared variable eemxih : enumeration_value_mirror;
  shared variable rldcieguci : floating_subtype_mirror;
  shared variable jvdjwmmra : value_mirror;
  signal bnbsb : time;
  shared variable xzrfw : enumeration_value_mirror;
  shared variable bqqens : value_mirror;
  signal vsokgnl : time;
  shared variable fgfswgp : enumeration_value_mirror;
  shared variable ar : floating_subtype_mirror;
begin
  inmcyayp : entity work.vikgxtryx
    port map (zozqsdgh => ar, azxzlfte => fgfswgp, bewxbyqxhl => vsokgnl, cc => bqqens);
  lr : entity work.vikgxtryx
    port map (zozqsdgh => yjivzp, azxzlfte => xzrfw, bewxbyqxhl => bnbsb, cc => jvdjwmmra);
  gultrqnfb : entity work.vikgxtryx
    port map (zozqsdgh => rldcieguci, azxzlfte => eemxih, bewxbyqxhl => yzl, cc => gkpgxs);
  
  -- Single-driven assignments
  gst <= (2#11# fs, 1 min);
end wkt;



-- Seed after: 4779680970138829904,7726014785203345639
