-- Seed: 212240522829599614,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity defeytyrl is
  port (yqu : linkage std_logic; umsycyvy : inout real);
end defeytyrl;

architecture t of defeytyrl is
  
begin
  -- Single-driven assignments
  umsycyvy <= 16#E.7#;
end t;

entity jaf is
  port (r : inout real; x : out string(2 to 2));
end jaf;

architecture ylw of jaf is
  
begin
  
end ylw;

library ieee;
use ieee.std_logic_1164.all;

entity xqijnyoaul is
  port (dn : buffer std_logic_vector(4 to 1); sbjsmhkxr : buffer real; zfccjxajvp : in real_vector(3 to 1));
end xqijnyoaul;

library ieee;
use ieee.std_logic_1164.all;

architecture zfi of xqijnyoaul is
  signal tjxdxblijs : std_logic;
  signal bfqeof : string(2 to 2);
  signal jdegir : real;
  signal ewdgy : real;
  signal zuwdzelzwz : std_logic;
begin
  yxlyq : entity work.defeytyrl
    port map (yqu => zuwdzelzwz, umsycyvy => ewdgy);
  agapiga : entity work.jaf
    port map (r => jdegir, x => bfqeof);
  ilykfym : entity work.defeytyrl
    port map (yqu => tjxdxblijs, umsycyvy => sbjsmhkxr);
  
  -- Multi-driven assignments
  zuwdzelzwz <= 'Z';
  dn <= (others => '0');
end zfi;

library ieee;
use ieee.std_logic_1164.all;

entity ofudkrap is
  port (lrf : inout integer; zc : in integer; vdhwxcc : inout std_logic_vector(1 downto 1); uvqaxzal : buffer bit_vector(0 to 0));
end ofudkrap;

library ieee;
use ieee.std_logic_1164.all;

architecture egelssvuzp of ofudkrap is
  signal uu : string(2 to 2);
  signal lfuctlyyn : real;
  signal owsywulnxa : real;
  signal cbhdlso : std_logic_vector(4 to 1);
  signal fokfpd : real;
  signal whfrymoaw : std_logic;
  signal vrjosm : real_vector(3 to 1);
  signal i : real;
  signal mmuuq : std_logic_vector(4 to 1);
begin
  rmrjl : entity work.xqijnyoaul
    port map (dn => mmuuq, sbjsmhkxr => i, zfccjxajvp => vrjosm);
  qqjpkckx : entity work.defeytyrl
    port map (yqu => whfrymoaw, umsycyvy => fokfpd);
  fmajgwa : entity work.xqijnyoaul
    port map (dn => cbhdlso, sbjsmhkxr => owsywulnxa, zfccjxajvp => vrjosm);
  gmcgkm : entity work.jaf
    port map (r => lfuctlyyn, x => uu);
  
  -- Single-driven assignments
  uvqaxzal <= (others => '1');
  
  -- Multi-driven assignments
  vdhwxcc <= "U";
end egelssvuzp;



-- Seed after: 16544253062367643391,3687118713772291287
