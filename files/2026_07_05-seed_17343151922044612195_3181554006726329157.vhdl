-- Seed: 17343151922044612195,3181554006726329157

use std.reflection.all;

entity x is
  port (niklvtv : buffer time_vector(2 to 1); kyrvwd : inout record_value_mirror);
end x;

architecture friuaqew of x is
  
begin
  -- Single-driven assignments
  niklvtv <= (others => 0 ns);
end friuaqew;

use std.reflection.all;

entity itvpnxlnwv is
  port (pogamojrmb : out real; vvxy : inout record_subtype_mirror);
end itvpnxlnwv;

use std.reflection.all;

architecture bom of itvpnxlnwv is
  shared variable vz : record_value_mirror;
  signal ibpku : time_vector(2 to 1);
begin
  barmwxw : entity work.x
    port map (niklvtv => ibpku, kyrvwd => vz);
  
  -- Single-driven assignments
  pogamojrmb <= pogamojrmb;
end bom;



-- Seed after: 5776750430073871809,3181554006726329157
