-- Seed: 6208671544447319727,3566912872917928779

use std.reflection.all;

entity khhmg is
  port (qjw : inout enumeration_subtype_mirror; foqy : inout record_value_mirror; gxgmyh : linkage severity_level);
end khhmg;

architecture v of khhmg is
  
begin
  
end v;

library ieee;
use ieee.std_logic_1164.all;

entity vgwxb is
  port (eeawvbe : buffer std_logic; difeuc : buffer integer; mcgfsgfolo : buffer bit);
end vgwxb;

architecture hknvgkiyx of vgwxb is
  
begin
  -- Single-driven assignments
  mcgfsgfolo <= mcgfsgfolo;
  difeuc <= 16#A_4_7#;
  
  -- Multi-driven assignments
  eeawvbe <= eeawvbe;
end hknvgkiyx;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity fahy is
  port (obmgfl : out std_logic; kjgcnne : inout file_subtype_mirror; qbozi : inout array_value_mirror);
end fahy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tnisy of fahy is
  signal jsuddhsr : bit;
  signal hfztywc : integer;
  signal bwlwmv : std_logic;
  signal ivqcg : severity_level;
  shared variable pmmloibhex : record_value_mirror;
  shared variable ems : enumeration_subtype_mirror;
begin
  syzaw : entity work.khhmg
    port map (qjw => ems, foqy => pmmloibhex, gxgmyh => ivqcg);
  c : entity work.vgwxb
    port map (eeawvbe => bwlwmv, difeuc => hfztywc, mcgfsgfolo => jsuddhsr);
end tnisy;

entity ecgtpqz is
  port (yemrnob : inout bit);
end ecgtpqz;

use std.reflection.all;

architecture cza of ecgtpqz is
  signal ujkwfwwwtn : severity_level;
  shared variable uwrqrvhvpo : record_value_mirror;
  shared variable vtnpkzhl : enumeration_subtype_mirror;
  signal ibxlecy : severity_level;
  shared variable qzsuill : record_value_mirror;
  shared variable tx : enumeration_subtype_mirror;
  signal rdvi : severity_level;
  shared variable qszdo : record_value_mirror;
  shared variable cwguhsag : enumeration_subtype_mirror;
begin
  yoxktlq : entity work.khhmg
    port map (qjw => cwguhsag, foqy => qszdo, gxgmyh => rdvi);
  gbcuicyrhd : entity work.khhmg
    port map (qjw => tx, foqy => qzsuill, gxgmyh => ibxlecy);
  vcxt : entity work.khhmg
    port map (qjw => vtnpkzhl, foqy => uwrqrvhvpo, gxgmyh => ujkwfwwwtn);
  
  -- Single-driven assignments
  yemrnob <= yemrnob;
end cza;



-- Seed after: 14005930023205887778,3566912872917928779
