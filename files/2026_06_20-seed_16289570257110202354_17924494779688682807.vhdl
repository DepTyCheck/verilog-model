-- Seed: 16289570257110202354,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity jcqjwiviz is
  port (ebpcovtf : linkage std_logic_vector(3 to 1));
end jcqjwiviz;

architecture dmhfxb of jcqjwiviz is
  
begin
  
end dmhfxb;

entity s is
  port (bhtsleuji : inout string(2 to 5); nqjxht : inout bit; besnawmg : in real; chx : in integer);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture lzhonur of s is
  signal vq : std_logic_vector(3 to 1);
  signal hnfsuzsw : std_logic_vector(3 to 1);
begin
  dvaju : entity work.jcqjwiviz
    port map (ebpcovtf => hnfsuzsw);
  umeoz : entity work.jcqjwiviz
    port map (ebpcovtf => vq);
  fifdjrto : entity work.jcqjwiviz
    port map (ebpcovtf => hnfsuzsw);
  
  -- Single-driven assignments
  bhtsleuji <= ('f', 'd', 'f', 'w');
  nqjxht <= '0';
  
  -- Multi-driven assignments
  vq <= "";
  vq <= (others => '0');
  hnfsuzsw <= "";
  hnfsuzsw <= "";
end lzhonur;

entity g is
  port (cslstg : inout severity_level; jqrdr : buffer severity_level; aqido : in time);
end g;

architecture flrprvlsql of g is
  signal wymjolrnj : integer;
  signal higgicicy : real;
  signal wu : bit;
  signal pjuyty : string(2 to 5);
begin
  l : entity work.s
    port map (bhtsleuji => pjuyty, nqjxht => wu, besnawmg => higgicicy, chx => wymjolrnj);
  
  -- Single-driven assignments
  higgicicy <= 11.34212;
  wymjolrnj <= 16#B_8_D#;
  cslstg <= NOTE;
  jqrdr <= ERROR;
end flrprvlsql;



-- Seed after: 16403037137384556812,17924494779688682807
