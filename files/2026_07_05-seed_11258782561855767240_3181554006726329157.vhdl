-- Seed: 11258782561855767240,3181554006726329157

use std.reflection.all;

entity byvqe is
  port (mwl : inout record_value_mirror);
end byvqe;

architecture d of byvqe is
  
begin
  
end d;

entity dmtk is
  port (ujhahlorq : buffer integer);
end dmtk;

use std.reflection.all;

architecture yrniobkus of dmtk is
  shared variable gzurnetx : record_value_mirror;
  shared variable veex : record_value_mirror;
begin
  tuysaky : entity work.byvqe
    port map (mwl => veex);
  xmcafirf : entity work.byvqe
    port map (mwl => gzurnetx);
  
  -- Single-driven assignments
  ujhahlorq <= ujhahlorq;
end yrniobkus;

use std.reflection.all;

entity v is
  port (wwww : in time; vtyvyzsc : inout physical_value_mirror; cnuktr : inout access_value_mirror);
end v;

use std.reflection.all;

architecture nacugex of v is
  shared variable juxmckht : record_value_mirror;
  shared variable uepqafjbxu : record_value_mirror;
begin
  djei : entity work.byvqe
    port map (mwl => uepqafjbxu);
  mmfob : entity work.byvqe
    port map (mwl => juxmckht);
end nacugex;



-- Seed after: 2858366409530880850,3181554006726329157
