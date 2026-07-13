-- Seed: 1364208604967414247,3566912872917928779

use std.reflection.all;

entity oyurqyicf is
  port (vv : inout file_value_mirror);
end oyurqyicf;

architecture j of oyurqyicf is
  
begin
  
end j;

use std.reflection.all;

entity wm is
  port (hwxvxbkxp : buffer integer; sx : inout file_value_mirror; pjzcqp : inout enumeration_value_mirror; rsbxupy : linkage time);
end wm;

use std.reflection.all;

architecture gfr of wm is
  shared variable exvct : file_value_mirror;
  shared variable qjyiuheeja : file_value_mirror;
begin
  jzhnmkblvp : entity work.oyurqyicf
    port map (vv => qjyiuheeja);
  rrrmavng : entity work.oyurqyicf
    port map (vv => exvct);
  
  -- Single-driven assignments
  hwxvxbkxp <= 2#0111#;
end gfr;

entity cwumf is
  port (ykb : inout boolean_vector(4 downto 4));
end cwumf;

use std.reflection.all;

architecture yuwqlxgqsp of cwumf is
  signal avxplfnvz : time;
  shared variable ao : enumeration_value_mirror;
  shared variable lavmfh : file_value_mirror;
  signal qfx : integer;
  shared variable mvnfq : file_value_mirror;
  shared variable q : file_value_mirror;
begin
  fdycjlcqt : entity work.oyurqyicf
    port map (vv => q);
  up : entity work.oyurqyicf
    port map (vv => mvnfq);
  ydfjpkeqc : entity work.wm
    port map (hwxvxbkxp => qfx, sx => lavmfh, pjzcqp => ao, rsbxupy => avxplfnvz);
  
  -- Single-driven assignments
  ykb <= (others => FALSE);
end yuwqlxgqsp;

use std.reflection.all;

entity cwwogjoaa is
  port (g : inout floating_value_mirror);
end cwwogjoaa;

use std.reflection.all;

architecture kqfoikphdn of cwwogjoaa is
  shared variable zgored : file_value_mirror;
begin
  xkwqvg : entity work.oyurqyicf
    port map (vv => zgored);
end kqfoikphdn;



-- Seed after: 17300406429153892901,3566912872917928779
