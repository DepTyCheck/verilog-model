-- Seed: 15746225090529146047,6290177331721581829

use std.reflection.all;

entity enxwng is
  port (liuwltz : inout file_subtype_mirror; tri : inout severity_level);
end enxwng;

architecture ldqmdtj of enxwng is
  
begin
  
end ldqmdtj;

use std.reflection.all;

entity ltppueb is
  port (swwiqyliz : inout file_subtype_mirror);
end ltppueb;

use std.reflection.all;

architecture cqvcug of ltppueb is
  signal oemwdx : severity_level;
  signal eezd : severity_level;
  shared variable radkv : file_subtype_mirror;
begin
  uf : entity work.enxwng
    port map (liuwltz => radkv, tri => eezd);
  pixzke : entity work.enxwng
    port map (liuwltz => swwiqyliz, tri => oemwdx);
end cqvcug;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity eww is
  port (gdh : buffer boolean; tnosarnr : inout real; whg : inout physical_subtype_mirror; j : in std_logic_vector(0 to 3));
end eww;

architecture caueddg of eww is
  
begin
  
end caueddg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hqydxvjhl is
  port (xdk : linkage std_logic_vector(3 downto 0); dhopm : inout file_value_mirror);
end hqydxvjhl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mi of hqydxvjhl is
  shared variable ztkpfk : file_subtype_mirror;
  signal kwwpob : std_logic_vector(0 to 3);
  shared variable oshqv : physical_subtype_mirror;
  signal xeyggjihov : real;
  signal someaf : boolean;
begin
  wuzw : entity work.eww
    port map (gdh => someaf, tnosarnr => xeyggjihov, whg => oshqv, j => kwwpob);
  wxcj : entity work.ltppueb
    port map (swwiqyliz => ztkpfk);
  
  -- Multi-driven assignments
  kwwpob <= kwwpob;
  kwwpob <= "01XH";
end mi;



-- Seed after: 9371941630755409405,6290177331721581829
