-- Seed: 13225230044539143364,14426950258250697445

use std.reflection.all;

entity commpbwpmr is
  port (uqbsxnivdj : inout value_mirror);
end commpbwpmr;

architecture smrzmbem of commpbwpmr is
  
begin
  
end smrzmbem;

use std.reflection.all;

entity wofh is
  port (uqzuoebpoj : inout array_value_mirror; j : inout record_subtype_mirror; dvytpcgmb : inout access_value_mirror; v : inout integer);
end wofh;

architecture p of wofh is
  
begin
  -- Single-driven assignments
  v <= 4;
end p;

use std.reflection.all;

entity mg is
  port (uvkpzkncei : inout value_mirror; ikck : inout access_value_mirror);
end mg;

use std.reflection.all;

architecture ga of mg is
  shared variable kjf : value_mirror;
begin
  pumaufx : entity work.commpbwpmr
    port map (uqbsxnivdj => kjf);
end ga;

entity zorjrbrv is
  port (kgyhbba : buffer real);
end zorjrbrv;

use std.reflection.all;

architecture qlruetf of zorjrbrv is
  shared variable ti : access_value_mirror;
  shared variable ouqwurh : value_mirror;
  shared variable igeqe : access_value_mirror;
  shared variable cjur : value_mirror;
  shared variable y : value_mirror;
  shared variable wmpuukagbr : access_value_mirror;
  shared variable gtjfqz : value_mirror;
begin
  fthfjf : entity work.mg
    port map (uvkpzkncei => gtjfqz, ikck => wmpuukagbr);
  k : entity work.commpbwpmr
    port map (uqbsxnivdj => y);
  mjphkwhm : entity work.mg
    port map (uvkpzkncei => cjur, ikck => igeqe);
  mvmar : entity work.mg
    port map (uvkpzkncei => ouqwurh, ikck => ti);
end qlruetf;



-- Seed after: 10915009996865204298,14426950258250697445
