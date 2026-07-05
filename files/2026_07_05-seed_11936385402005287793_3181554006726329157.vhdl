-- Seed: 11936385402005287793,3181554006726329157

entity wgepmipr is
  port (jjinybjq : inout boolean);
end wgepmipr;

architecture yqrzttdxd of wgepmipr is
  
begin
  -- Single-driven assignments
  jjinybjq <= TRUE;
end yqrzttdxd;

use std.reflection.all;

entity vxhapca is
  port (xsnjpqaobi : inout file_subtype_mirror; i : inout value_mirror; vudwusxo : inout time);
end vxhapca;

architecture dxn of vxhapca is
  signal jsnozf : boolean;
begin
  zzrffdo : entity work.wgepmipr
    port map (jjinybjq => jsnozf);
  
  -- Single-driven assignments
  vudwusxo <= 1.0 ps;
end dxn;

use std.reflection.all;

entity ca is
  port (sq : inout floating_value_mirror; pmqcgskuyo : inout access_value_mirror; etjxg : inout physical_subtype_mirror);
end ca;

architecture w of ca is
  signal rznvxnjw : boolean;
begin
  dov : entity work.wgepmipr
    port map (jjinybjq => rznvxnjw);
end w;

entity icrqowx is
  port (bslohkpz : out time_vector(1 downto 1); dvxnydz : inout integer);
end icrqowx;

use std.reflection.all;

architecture erenooynwm of icrqowx is
  signal kecgywq : time;
  shared variable urccgfflx : value_mirror;
  shared variable zbfxjwi : file_subtype_mirror;
  shared variable jcidgs : physical_subtype_mirror;
  shared variable assqgglugh : access_value_mirror;
  shared variable xfrpdezzyg : floating_value_mirror;
begin
  aukemnlvy : entity work.ca
    port map (sq => xfrpdezzyg, pmqcgskuyo => assqgglugh, etjxg => jcidgs);
  zvrxff : entity work.vxhapca
    port map (xsnjpqaobi => zbfxjwi, i => urccgfflx, vudwusxo => kecgywq);
  
  -- Single-driven assignments
  bslohkpz <= bslohkpz;
  dvxnydz <= dvxnydz;
end erenooynwm;



-- Seed after: 3802361390438495571,3181554006726329157
