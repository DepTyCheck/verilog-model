-- Seed: 1625075593436685083,2983771601630957889

use std.reflection.all;

entity iuif is
  port (variable i : inout access_value_mirror_pt; utbhbhldll : in time);
end iuif;

architecture r of iuif is
  
begin
  
end r;

use std.reflection.all;

entity k is
  port (ilvv : buffer time; variable shzqp : inout value_mirror_pt; variable d : inout protected_value_mirror_pt; jm : inout real);
end k;

use std.reflection.all;

architecture zm of k is
  signal yfdzikzm : time;
  shared variable wsmzag : access_value_mirror_pt;
  shared variable cttb : access_value_mirror_pt;
  signal kkcc : time;
  shared variable biogsxjc : access_value_mirror_pt;
  shared variable mvqwedeh : access_value_mirror_pt;
begin
  now : entity work.iuif
    port map (i => mvqwedeh, utbhbhldll => ilvv);
  vji : entity work.iuif
    port map (i => biogsxjc, utbhbhldll => kkcc);
  skeat : entity work.iuif
    port map (i => cttb, utbhbhldll => ilvv);
  rybzw : entity work.iuif
    port map (i => wsmzag, utbhbhldll => yfdzikzm);
  
  -- Single-driven assignments
  jm <= 2#0.1_0#;
end zm;

use std.reflection.all;

entity ja is
  port (variable ejj : inout enumeration_subtype_mirror_pt; xdkfedig : buffer real);
end ja;

use std.reflection.all;

architecture pr of ja is
  shared variable as : access_value_mirror_pt;
  shared variable nl : access_value_mirror_pt;
  signal mtjz : time;
  shared variable i : access_value_mirror_pt;
begin
  tur : entity work.iuif
    port map (i => i, utbhbhldll => mtjz);
  qqgi : entity work.iuif
    port map (i => nl, utbhbhldll => mtjz);
  aeablhks : entity work.iuif
    port map (i => as, utbhbhldll => mtjz);
end pr;



-- Seed after: 1187197913403166402,2983771601630957889
