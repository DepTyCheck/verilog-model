-- Seed: 12844482231637196521,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ppt is
  port (yfg : inout access_value_mirror; mncf : inout floating_value_mirror; xj : inout std_logic_vector(1 downto 3); dinrmu : inout subtype_mirror);
end ppt;

architecture ckgbuf of ppt is
  
begin
  -- Multi-driven assignments
  xj <= "";
  xj <= (others => '0');
  xj <= "";
end ckgbuf;

use std.reflection.all;

entity jyxqnk is
  port (sgcszgp : inout physical_subtype_mirror);
end jyxqnk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dbkelpzg of jyxqnk is
  shared variable iwp : subtype_mirror;
  signal p : std_logic_vector(1 downto 3);
  shared variable mbsp : floating_value_mirror;
  shared variable dzyg : access_value_mirror;
begin
  dlplnkhiw : entity work.ppt
    port map (yfg => dzyg, mncf => mbsp, xj => p, dinrmu => iwp);
  
  -- Multi-driven assignments
  p <= p;
  p <= p;
  p <= p;
  p <= "";
end dbkelpzg;

use std.reflection.all;

entity ebfvadgh is
  port ( myc : buffer integer_vector(1 downto 1)
  ; ltwfnbmkzj : linkage integer
  ; kngqaodd : inout floating_subtype_mirror
  ; d : inout physical_subtype_mirror
  );
end ebfvadgh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture v of ebfvadgh is
  shared variable wn : physical_subtype_mirror;
  shared variable gogoysoafe : subtype_mirror;
  shared variable mxn : floating_value_mirror;
  shared variable xvg : access_value_mirror;
  shared variable uknnbyllis : subtype_mirror;
  signal wtlo : std_logic_vector(1 downto 3);
  shared variable xh : floating_value_mirror;
  shared variable fqvastckw : access_value_mirror;
begin
  hoprnqoya : entity work.ppt
    port map (yfg => fqvastckw, mncf => xh, xj => wtlo, dinrmu => uknnbyllis);
  jlo : entity work.ppt
    port map (yfg => xvg, mncf => mxn, xj => wtlo, dinrmu => gogoysoafe);
  xe : entity work.jyxqnk
    port map (sgcszgp => wn);
  
  -- Multi-driven assignments
  wtlo <= wtlo;
  wtlo <= "";
end v;



-- Seed after: 2344616496199731469,2158184632809654795
