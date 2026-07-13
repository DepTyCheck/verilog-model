-- Seed: 4180604915774833677,3566912872917928779

use std.reflection.all;

entity ubux is
  port (djdfxle : inout record_subtype_mirror; vbiihcwfhu : linkage real; bamozsb : inout enumeration_subtype_mirror);
end ubux;

architecture qy of ubux is
  
begin
  
end qy;

entity fzcfgdwwat is
  port (buehe : out time);
end fzcfgdwwat;

use std.reflection.all;

architecture rqklhs of fzcfgdwwat is
  shared variable ljsnxlm : enumeration_subtype_mirror;
  signal wj : real;
  shared variable wwytamulbl : record_subtype_mirror;
  shared variable ccimx : enumeration_subtype_mirror;
  signal tjiovcj : real;
  shared variable dncwp : record_subtype_mirror;
begin
  gyfy : entity work.ubux
    port map (djdfxle => dncwp, vbiihcwfhu => tjiovcj, bamozsb => ccimx);
  w : entity work.ubux
    port map (djdfxle => wwytamulbl, vbiihcwfhu => wj, bamozsb => ljsnxlm);
  
  -- Single-driven assignments
  buehe <= 4_3 fs;
end rqklhs;



-- Seed after: 16964788700715014786,3566912872917928779
