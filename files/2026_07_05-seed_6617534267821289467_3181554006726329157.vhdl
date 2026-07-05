-- Seed: 6617534267821289467,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity kynaefuu is
  port (kdmaao : buffer std_logic_vector(0 downto 4); l : inout enumeration_subtype_mirror; zfo : buffer time; y : out real);
end kynaefuu;

architecture eveepv of kynaefuu is
  
begin
  -- Multi-driven assignments
  kdmaao <= kdmaao;
  kdmaao <= "";
end eveepv;

use std.reflection.all;

entity flhs is
  port (iqjkdor : inout boolean; lth : inout access_value_mirror; pvffjyf : in severity_level);
end flhs;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ba of flhs is
  signal z : real;
  signal qbjobgnu : time;
  shared variable uvquekj : enumeration_subtype_mirror;
  signal efjohlt : real;
  signal godqhrfxdq : time;
  shared variable f : enumeration_subtype_mirror;
  signal zjczpsmuwd : real;
  signal imhwl : time;
  shared variable y : enumeration_subtype_mirror;
  signal toh : std_logic_vector(0 downto 4);
begin
  qe : entity work.kynaefuu
    port map (kdmaao => toh, l => y, zfo => imhwl, y => zjczpsmuwd);
  x : entity work.kynaefuu
    port map (kdmaao => toh, l => f, zfo => godqhrfxdq, y => efjohlt);
  sfxkj : entity work.kynaefuu
    port map (kdmaao => toh, l => uvquekj, zfo => qbjobgnu, y => z);
  
  -- Single-driven assignments
  iqjkdor <= TRUE;
  
  -- Multi-driven assignments
  toh <= (others => '0');
  toh <= toh;
end ba;



-- Seed after: 6257142555216712362,3181554006726329157
