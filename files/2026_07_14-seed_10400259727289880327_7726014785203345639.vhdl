-- Seed: 10400259727289880327,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ipmhxb is
  port (jydvvnw : inout array_value_mirror; esmejlj : inout integer_subtype_mirror; wjtwbk : inout std_logic_vector(2 to 2));
end ipmhxb;

architecture fnibdylob of ipmhxb is
  
begin
  -- Multi-driven assignments
  wjtwbk <= wjtwbk;
  wjtwbk <= (others => 'U');
  wjtwbk <= wjtwbk;
end fnibdylob;

entity lheakupzy is
  port (e : buffer severity_level);
end lheakupzy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dmqlznnqr of lheakupzy is
  shared variable wszpx : integer_subtype_mirror;
  shared variable ezk : array_value_mirror;
  signal ctasfblsfs : std_logic_vector(2 to 2);
  shared variable xmzfc : integer_subtype_mirror;
  shared variable bvfyxhdd : array_value_mirror;
  signal utmcgbt : std_logic_vector(2 to 2);
  shared variable zfchbuqblb : integer_subtype_mirror;
  shared variable r : array_value_mirror;
begin
  qxloom : entity work.ipmhxb
    port map (jydvvnw => r, esmejlj => zfchbuqblb, wjtwbk => utmcgbt);
  tmh : entity work.ipmhxb
    port map (jydvvnw => bvfyxhdd, esmejlj => xmzfc, wjtwbk => ctasfblsfs);
  tfgqglvuw : entity work.ipmhxb
    port map (jydvvnw => ezk, esmejlj => wszpx, wjtwbk => utmcgbt);
  
  -- Single-driven assignments
  e <= NOTE;
  
  -- Multi-driven assignments
  utmcgbt <= "X";
  utmcgbt <= "L";
end dmqlznnqr;



-- Seed after: 161888529949286430,7726014785203345639
