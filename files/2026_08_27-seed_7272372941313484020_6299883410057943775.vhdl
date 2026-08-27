-- Seed: 7272372941313484020,6299883410057943775

entity bgigywa is
  port (fhgvgdzob : buffer bit; expcmuw : linkage real);
end bgigywa;

architecture fdjpvbna of bgigywa is
  
begin
  -- Single-driven assignments
  fhgvgdzob <= '0';
end fdjpvbna;

entity tqiwxjgg is
  port (lobjswflb : linkage real_vector(3 downto 3));
end tqiwxjgg;

architecture mngfys of tqiwxjgg is
  signal bsgcdlz : real;
  signal mxz : bit;
  signal cyjjnpmbe : real;
  signal qfhx : bit;
begin
  mnvvipwvzq : entity work.bgigywa
    port map (fhgvgdzob => qfhx, expcmuw => cyjjnpmbe);
  l : entity work.bgigywa
    port map (fhgvgdzob => mxz, expcmuw => bsgcdlz);
end mngfys;

library ieee;
use ieee.std_logic_1164.all;

entity lr is
  port (kkuiwj : in std_logic; gejt : in severity_level);
end lr;

architecture rpzd of lr is
  signal xqz : real_vector(3 downto 3);
begin
  py : entity work.tqiwxjgg
    port map (lobjswflb => xqz);
end rpzd;

library ieee;
use ieee.std_logic_1164.all;

entity snyam is
  port (z : inout integer; tenvftw : buffer boolean_vector(2 downto 2); qkkpbs : buffer std_logic; pzoey : buffer real);
end snyam;

architecture vfrzhfhcss of snyam is
  signal wzho : real;
  signal xlilva : bit;
begin
  qhfibg : entity work.bgigywa
    port map (fhgvgdzob => xlilva, expcmuw => wzho);
  
  -- Single-driven assignments
  tenvftw <= (others => FALSE);
  z <= 16#CD9#;
  pzoey <= wzho;
  
  -- Multi-driven assignments
  qkkpbs <= qkkpbs;
end vfrzhfhcss;



-- Seed after: 3885836293462018829,6299883410057943775
