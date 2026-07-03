-- Seed: 3068459018316758894,2158184632809654795

use std.reflection.all;

entity xdreoaaew is
  port (xlyzytj : inout array_value_mirror; iamjwicsp : inout floating_value_mirror; ashxtjfbac : inout access_subtype_mirror);
end xdreoaaew;

architecture nzngcnfvof of xdreoaaew is
  
begin
  
end nzngcnfvof;

use std.reflection.all;

entity u is
  port (larhsrz : inout file_subtype_mirror);
end u;

use std.reflection.all;

architecture jkdt of u is
  shared variable euil : access_subtype_mirror;
  shared variable ba : floating_value_mirror;
  shared variable fcjgl : array_value_mirror;
begin
  vknujrza : entity work.xdreoaaew
    port map (xlyzytj => fcjgl, iamjwicsp => ba, ashxtjfbac => euil);
end jkdt;

use std.reflection.all;

entity h is
  port (cjd : inout access_value_mirror);
end h;

use std.reflection.all;

architecture muvbwwxp of h is
  shared variable acm : access_subtype_mirror;
  shared variable cf : floating_value_mirror;
  shared variable ntyiuhlha : array_value_mirror;
begin
  tsayhzt : entity work.xdreoaaew
    port map (xlyzytj => ntyiuhlha, iamjwicsp => cf, ashxtjfbac => acm);
end muvbwwxp;

use std.reflection.all;

entity pjyakkazz is
  port (uzro : buffer bit; vhkhmdwskc : out time_vector(0 downto 0); k : inout integer_value_mirror);
end pjyakkazz;

use std.reflection.all;

architecture ljqno of pjyakkazz is
  shared variable gcyermdfpd : access_subtype_mirror;
  shared variable i : floating_value_mirror;
  shared variable xro : array_value_mirror;
  shared variable sqzcwkemy : access_value_mirror;
begin
  dvanbazh : entity work.h
    port map (cjd => sqzcwkemy);
  ssfisklglf : entity work.xdreoaaew
    port map (xlyzytj => xro, iamjwicsp => i, ashxtjfbac => gcyermdfpd);
  
  -- Single-driven assignments
  vhkhmdwskc <= (others => 4_3_0_0 ms);
  uzro <= '0';
end ljqno;



-- Seed after: 10847631840159903881,2158184632809654795
