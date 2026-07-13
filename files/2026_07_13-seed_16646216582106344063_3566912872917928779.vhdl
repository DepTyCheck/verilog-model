-- Seed: 16646216582106344063,3566912872917928779

use std.reflection.all;

entity hei is
  port (rzrnty : inout access_value_mirror; oldjnjkw : out bit_vector(0 to 3); gecqvbpda : inout array_subtype_mirror; te : inout file_subtype_mirror);
end hei;

architecture vwnbq of hei is
  
begin
  -- Single-driven assignments
  oldjnjkw <= ('0', '1', '1', '0');
end vwnbq;

library ieee;
use ieee.std_logic_1164.all;

entity zef is
  port (hpetcfx : linkage std_logic_vector(4 to 3); vynnku : inout character);
end zef;

use std.reflection.all;

architecture ei of zef is
  shared variable itmvfdda : file_subtype_mirror;
  shared variable um : array_subtype_mirror;
  signal km : bit_vector(0 to 3);
  shared variable bkj : access_value_mirror;
  shared variable b : file_subtype_mirror;
  shared variable vmev : array_subtype_mirror;
  signal sgpeajvu : bit_vector(0 to 3);
  shared variable kvn : access_value_mirror;
  shared variable plbbolay : file_subtype_mirror;
  shared variable pvf : array_subtype_mirror;
  signal y : bit_vector(0 to 3);
  shared variable hhrbu : access_value_mirror;
  shared variable gnjzrhsttb : file_subtype_mirror;
  shared variable hmjrbqh : array_subtype_mirror;
  signal gemjxufr : bit_vector(0 to 3);
  shared variable fz : access_value_mirror;
begin
  qr : entity work.hei
    port map (rzrnty => fz, oldjnjkw => gemjxufr, gecqvbpda => hmjrbqh, te => gnjzrhsttb);
  xfnf : entity work.hei
    port map (rzrnty => hhrbu, oldjnjkw => y, gecqvbpda => pvf, te => plbbolay);
  avbcqt : entity work.hei
    port map (rzrnty => kvn, oldjnjkw => sgpeajvu, gecqvbpda => vmev, te => b);
  hxpmgzmx : entity work.hei
    port map (rzrnty => bkj, oldjnjkw => km, gecqvbpda => um, te => itmvfdda);
  
  -- Single-driven assignments
  vynnku <= vynnku;
end ei;



-- Seed after: 1737913465429611662,3566912872917928779
