-- Seed: 18025671044132052418,3566912872917928779

use std.reflection.all;

entity b is
  port (eqyibooki : inout enumeration_value_mirror; suz : inout protected_value_mirror; yckh : inout file_subtype_mirror);
end b;

architecture fnuewti of b is
  
begin
  
end fnuewti;

use std.reflection.all;

entity ccgn is
  port (svgzy : inout integer_value_mirror; hzr : out integer; q : buffer real; wtstrt : inout record_value_mirror);
end ccgn;

use std.reflection.all;

architecture qwmll of ccgn is
  shared variable pjhjpdwtz : file_subtype_mirror;
  shared variable vkztoja : protected_value_mirror;
  shared variable dfoll : enumeration_value_mirror;
begin
  flhb : entity work.b
    port map (eqyibooki => dfoll, suz => vkztoja, yckh => pjhjpdwtz);
  
  -- Single-driven assignments
  q <= 2.00;
  hzr <= 4_1_3_0_0;
end qwmll;

entity hydv is
  port (luoa : buffer boolean_vector(4 to 3); ggrx : in integer);
end hydv;

use std.reflection.all;

architecture ztw of hydv is
  shared variable ssoa : file_subtype_mirror;
  shared variable gz : protected_value_mirror;
  shared variable wqbcqw : enumeration_value_mirror;
  shared variable sydoydyhpg : record_value_mirror;
  signal uygoebwoc : real;
  signal fjwoehlkag : integer;
  shared variable oygan : integer_value_mirror;
begin
  ympfx : entity work.ccgn
    port map (svgzy => oygan, hzr => fjwoehlkag, q => uygoebwoc, wtstrt => sydoydyhpg);
  hzdumlt : entity work.b
    port map (eqyibooki => wqbcqw, suz => gz, yckh => ssoa);
  
  -- Single-driven assignments
  luoa <= (others => TRUE);
end ztw;



-- Seed after: 12865057666126056433,3566912872917928779
