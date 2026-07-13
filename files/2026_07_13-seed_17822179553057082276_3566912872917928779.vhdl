-- Seed: 17822179553057082276,3566912872917928779

use std.reflection.all;

entity qa is
  port (gat : inout file_value_mirror; q : inout floating_value_mirror);
end qa;

architecture yqnvlmcbry of qa is
  
begin
  
end yqnvlmcbry;

entity fimebu is
  port (yswlkv : inout boolean_vector(3 to 4));
end fimebu;

use std.reflection.all;

architecture swwiczuiov of fimebu is
  shared variable kbmnwfgn : floating_value_mirror;
  shared variable o : file_value_mirror;
begin
  l : entity work.qa
    port map (gat => o, q => kbmnwfgn);
end swwiczuiov;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hcnybgnvo is
  port (skxfafdc : inout enumeration_value_mirror; hbeiburhd : in std_logic; ct : buffer boolean; sqv : inout protected_value_mirror);
end hcnybgnvo;

architecture iryscokzl of hcnybgnvo is
  signal ufqjdawjwz : boolean_vector(3 to 4);
begin
  akql : entity work.fimebu
    port map (yswlkv => ufqjdawjwz);
end iryscokzl;

use std.reflection.all;

entity jzogxowrwl is
  port (xogahu : buffer real; tnsbk : inout floating_subtype_mirror);
end jzogxowrwl;

use std.reflection.all;

architecture dzqnsuxjo of jzogxowrwl is
  shared variable avkedlqz : floating_value_mirror;
  shared variable aikk : file_value_mirror;
  shared variable iwuiy : floating_value_mirror;
  shared variable qxyk : file_value_mirror;
begin
  j : entity work.qa
    port map (gat => qxyk, q => iwuiy);
  bqhdsxkd : entity work.qa
    port map (gat => aikk, q => avkedlqz);
  
  -- Single-driven assignments
  xogahu <= 16#5_4_3.8_3#;
end dzqnsuxjo;



-- Seed after: 11943221084129661023,3566912872917928779
