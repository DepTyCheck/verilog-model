-- Seed: 1175156756537238936,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity xph is
  port (jpeivjptvn : out std_logic; y : inout boolean_vector(0 to 3));
end xph;

architecture blu of xph is
  
begin
  -- Single-driven assignments
  y <= (FALSE, TRUE, TRUE, FALSE);
  
  -- Multi-driven assignments
  jpeivjptvn <= 'H';
end blu;

entity jpoatc is
  port (gxvoypb : inout integer);
end jpoatc;

library ieee;
use ieee.std_logic_1164.all;

architecture aykwxt of jpoatc is
  signal gkorp : boolean_vector(0 to 3);
  signal arsoes : std_logic;
begin
  xwigfajdd : entity work.xph
    port map (jpeivjptvn => arsoes, y => gkorp);
  
  -- Multi-driven assignments
  arsoes <= 'Z';
end aykwxt;

library ieee;
use ieee.std_logic_1164.all;

entity kejafd is
  port (jzsunyykua : in integer_vector(2 to 1); itgqaoyhz : inout real; vl : out boolean; zybnkzx : inout std_logic_vector(0 to 2));
end kejafd;

library ieee;
use ieee.std_logic_1164.all;

architecture uuijhpkrax of kejafd is
  signal bx : boolean_vector(0 to 3);
  signal yjsaj : std_logic;
  signal ubufoxltq : boolean_vector(0 to 3);
  signal edgrw : std_logic;
  signal eyr : boolean_vector(0 to 3);
  signal ewbliv : std_logic;
begin
  echalxx : entity work.xph
    port map (jpeivjptvn => ewbliv, y => eyr);
  uo : entity work.xph
    port map (jpeivjptvn => edgrw, y => ubufoxltq);
  r : entity work.xph
    port map (jpeivjptvn => yjsaj, y => bx);
  
  -- Single-driven assignments
  itgqaoyhz <= 2_4_1_2_2.40311;
  vl <= TRUE;
  
  -- Multi-driven assignments
  ewbliv <= 'L';
end uuijhpkrax;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (jyul : out time; awiexy : in std_logic; tfjjaw : in real);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture fdhwdmah of u is
  signal qmncwzn : integer;
  signal ruvrmcj : boolean_vector(0 to 3);
  signal avxwya : std_logic;
begin
  citl : entity work.xph
    port map (jpeivjptvn => avxwya, y => ruvrmcj);
  jcnic : entity work.jpoatc
    port map (gxvoypb => qmncwzn);
  
  -- Single-driven assignments
  jyul <= 2#0.1_1_1# ns;
  
  -- Multi-driven assignments
  avxwya <= 'L';
  avxwya <= '-';
  avxwya <= '1';
end fdhwdmah;



-- Seed after: 4847336781699566422,6882842853887419669
