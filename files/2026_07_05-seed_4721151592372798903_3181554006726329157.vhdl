-- Seed: 4721151592372798903,3181554006726329157

use std.reflection.all;

entity hemvh is
  port (jkosiruj : in integer; bexdmgxur : buffer integer; ncdcyehl : inout real_vector(2 downto 2); hpqsqvsaay : inout protected_subtype_mirror);
end hemvh;

architecture vrrptnk of hemvh is
  
begin
  -- Single-driven assignments
  ncdcyehl <= (others => 2#0_1_1.011#);
  bexdmgxur <= bexdmgxur;
end vrrptnk;

use std.reflection.all;

entity g is
  port (lkouxwviw : inout array_subtype_mirror; yvghv : inout protected_subtype_mirror);
end g;

architecture cgijysu of g is
  
begin
  
end cgijysu;

use std.reflection.all;

entity y is
  port (dyf : inout file_value_mirror; blfcauqqtk : out character);
end y;

use std.reflection.all;

architecture qa of y is
  shared variable vej : protected_subtype_mirror;
  signal tufsuoqxd : real_vector(2 downto 2);
  signal ymghf : integer;
  signal hj : integer;
begin
  r : entity work.hemvh
    port map (jkosiruj => hj, bexdmgxur => ymghf, ncdcyehl => tufsuoqxd, hpqsqvsaay => vej);
  
  -- Single-driven assignments
  hj <= 3001;
end qa;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ofjzhbszrf is
  port (arhrdy : inout record_value_mirror; n : in integer; qkc : in real; cigddhyo : buffer std_logic_vector(1 to 2));
end ofjzhbszrf;

use std.reflection.all;

architecture fad of ofjzhbszrf is
  shared variable x : protected_subtype_mirror;
  signal pgohegejoh : real_vector(2 downto 2);
  signal vcwbwhr : integer;
  signal zknvpy : character;
  shared variable yldcw : file_value_mirror;
  shared variable xcx : protected_subtype_mirror;
  shared variable gtzfdzqsx : array_subtype_mirror;
begin
  fdbvsp : entity work.g
    port map (lkouxwviw => gtzfdzqsx, yvghv => xcx);
  jucvlcmq : entity work.y
    port map (dyf => yldcw, blfcauqqtk => zknvpy);
  qri : entity work.hemvh
    port map (jkosiruj => vcwbwhr, bexdmgxur => vcwbwhr, ncdcyehl => pgohegejoh, hpqsqvsaay => x);
  
  -- Multi-driven assignments
  cigddhyo <= cigddhyo;
  cigddhyo <= ('1', 'H');
  cigddhyo <= cigddhyo;
  cigddhyo <= cigddhyo;
end fad;



-- Seed after: 3686871994304890481,3181554006726329157
