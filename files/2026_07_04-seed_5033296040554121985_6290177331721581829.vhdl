-- Seed: 5033296040554121985,6290177331721581829

use std.reflection.all;

entity megeninoky is
  port (npajx : inout access_subtype_mirror; cgr : inout file_subtype_mirror; gjuqjp : linkage real);
end megeninoky;

architecture gps of megeninoky is
  
begin
  
end gps;

use std.reflection.all;

entity rhh is
  port (eboprqysr : out integer_vector(0 downto 3); rgqzxbiv : inout file_value_mirror);
end rhh;

use std.reflection.all;

architecture y of rhh is
  signal uodni : real;
  shared variable cojhysxdce : file_subtype_mirror;
  shared variable isiz : access_subtype_mirror;
  signal geotmtr : real;
  shared variable g : file_subtype_mirror;
  shared variable neu : access_subtype_mirror;
  signal yddg : real;
  shared variable rizcqft : file_subtype_mirror;
  shared variable elubw : access_subtype_mirror;
  signal wg : real;
  shared variable rwmjcp : file_subtype_mirror;
  shared variable kt : access_subtype_mirror;
begin
  xgtleevku : entity work.megeninoky
    port map (npajx => kt, cgr => rwmjcp, gjuqjp => wg);
  paofmupz : entity work.megeninoky
    port map (npajx => elubw, cgr => rizcqft, gjuqjp => yddg);
  gr : entity work.megeninoky
    port map (npajx => neu, cgr => g, gjuqjp => geotmtr);
  dxtbvdr : entity work.megeninoky
    port map (npajx => isiz, cgr => cojhysxdce, gjuqjp => uodni);
  
  -- Single-driven assignments
  eboprqysr <= eboprqysr;
end y;



-- Seed after: 17492017519001424178,6290177331721581829
