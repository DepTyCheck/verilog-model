-- Seed: 6399980822843617233,10871023049702252113

entity r is
  port (vgpsdjftsy : inout boolean_vector(3 downto 2); y : linkage time);
end r;

architecture ivrbyvt of r is
  
begin
  -- Single-driven assignments
  vgpsdjftsy <= (TRUE, FALSE);
end ivrbyvt;

entity hnjzpyl is
  port (bviivvk : out real);
end hnjzpyl;

architecture gj of hnjzpyl is
  signal wtsdqoql : time;
  signal wccaswwwp : boolean_vector(3 downto 2);
  signal pniobu : time;
  signal bvqfq : boolean_vector(3 downto 2);
  signal fcf : time;
  signal htmfejm : boolean_vector(3 downto 2);
  signal rrkevz : time;
  signal ln : boolean_vector(3 downto 2);
begin
  v : entity work.r
    port map (vgpsdjftsy => ln, y => rrkevz);
  fcajwglqbr : entity work.r
    port map (vgpsdjftsy => htmfejm, y => fcf);
  b : entity work.r
    port map (vgpsdjftsy => bvqfq, y => pniobu);
  eatvvfxh : entity work.r
    port map (vgpsdjftsy => wccaswwwp, y => wtsdqoql);
  
  -- Single-driven assignments
  bviivvk <= 41040.03;
end gj;

library ieee;
use ieee.std_logic_1164.all;

entity qwdyjisf is
  port (brn : buffer real; smeooene : inout std_logic; vdkpnd : buffer integer);
end qwdyjisf;

architecture t of qwdyjisf is
  
begin
  -- Single-driven assignments
  vdkpnd <= 1_2_2_1;
  
  -- Multi-driven assignments
  smeooene <= 'L';
  smeooene <= '0';
end t;



-- Seed after: 15027530430865733170,10871023049702252113
