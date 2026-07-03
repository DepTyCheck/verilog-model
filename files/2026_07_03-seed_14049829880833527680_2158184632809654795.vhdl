-- Seed: 14049829880833527680,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity unonvykm is
  port (eqvvvle : inout subtype_mirror; vvyrsvelte : inout std_logic_vector(2 to 1); buzi : inout physical_subtype_mirror);
end unonvykm;

architecture vtijilj of unonvykm is
  
begin
  
end vtijilj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity rspacsxw is
  port (pybvbi : in std_logic_vector(2 to 1); fm : inout floating_subtype_mirror);
end rspacsxw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture couygref of rspacsxw is
  shared variable yfgskkedb : physical_subtype_mirror;
  shared variable bd : subtype_mirror;
  shared variable fflpw : physical_subtype_mirror;
  signal nbkhuhbtp : std_logic_vector(2 to 1);
  shared variable hdszxlt : subtype_mirror;
  shared variable wfkgbzhk : physical_subtype_mirror;
  signal wpf : std_logic_vector(2 to 1);
  shared variable ns : subtype_mirror;
begin
  esouokbj : entity work.unonvykm
    port map (eqvvvle => ns, vvyrsvelte => wpf, buzi => wfkgbzhk);
  cdwgpfblo : entity work.unonvykm
    port map (eqvvvle => hdszxlt, vvyrsvelte => nbkhuhbtp, buzi => fflpw);
  kccidu : entity work.unonvykm
    port map (eqvvvle => bd, vvyrsvelte => nbkhuhbtp, buzi => yfgskkedb);
  
  -- Multi-driven assignments
  wpf <= pybvbi;
  wpf <= pybvbi;
  wpf <= "";
end couygref;



-- Seed after: 17779325172649372219,2158184632809654795
