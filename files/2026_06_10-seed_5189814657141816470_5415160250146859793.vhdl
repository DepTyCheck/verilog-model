-- Seed: 5189814657141816470,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity etzwvjwofo is
  port (efnlkrjur : in real_vector(4 to 2); hihroxsds : inout std_logic_vector(2 downto 4); xkuufrskla : in std_logic; ewytcn : out real);
end etzwvjwofo;



architecture y of etzwvjwofo is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity fvjiep is
  port (ebqt : in time_vector(4 downto 1); caxcnhnnp : linkage time; nzaepgv : out std_logic; rsfgddjf : in bit_vector(4 to 1));
end fvjiep;

library ieee;
use ieee.std_logic_1164.all;

architecture pmnpnl of fvjiep is
  signal hvcofjwedx : real;
  signal fycqyyvwf : std_logic;
  signal mzitdq : real_vector(4 to 2);
  signal tds : real;
  signal yvjqpyhavi : real_vector(4 to 2);
  signal liwonzjtje : real;
  signal nhw : std_logic_vector(2 downto 4);
  signal jtg : real_vector(4 to 2);
begin
  ukn : entity work.etzwvjwofo
    port map (efnlkrjur => jtg, hihroxsds => nhw, xkuufrskla => nzaepgv, ewytcn => liwonzjtje);
  b : entity work.etzwvjwofo
    port map (efnlkrjur => yvjqpyhavi, hihroxsds => nhw, xkuufrskla => nzaepgv, ewytcn => tds);
  fqjbc : entity work.etzwvjwofo
    port map (efnlkrjur => mzitdq, hihroxsds => nhw, xkuufrskla => fycqyyvwf, ewytcn => hvcofjwedx);
end pmnpnl;



entity ejf is
  port (humftsiyvy : inout bit);
end ejf;

library ieee;
use ieee.std_logic_1164.all;

architecture zi of ejf is
  signal oza : bit_vector(4 to 1);
  signal rmhkzb : time;
  signal jmaqt : time_vector(4 downto 1);
  signal qiihboijom : real;
  signal dg : std_logic;
  signal rn : std_logic_vector(2 downto 4);
  signal uyusrbf : real_vector(4 to 2);
begin
  cu : entity work.etzwvjwofo
    port map (efnlkrjur => uyusrbf, hihroxsds => rn, xkuufrskla => dg, ewytcn => qiihboijom);
  sthgunnyu : entity work.fvjiep
    port map (ebqt => jmaqt, caxcnhnnp => rmhkzb, nzaepgv => dg, rsfgddjf => oza);
end zi;



entity altrs is
  port (wsyzile : buffer time_vector(4 downto 1); zf : out integer_vector(3 downto 0));
end altrs;

library ieee;
use ieee.std_logic_1164.all;

architecture jev of altrs is
  signal wrukm : bit;
  signal ieo : bit;
  signal th : bit_vector(4 to 1);
  signal zdkywdayml : std_logic;
  signal h : time;
begin
  bxawmvs : entity work.fvjiep
    port map (ebqt => wsyzile, caxcnhnnp => h, nzaepgv => zdkywdayml, rsfgddjf => th);
  gthj : entity work.ejf
    port map (humftsiyvy => ieo);
  ncvcr : entity work.ejf
    port map (humftsiyvy => wrukm);
end jev;



-- Seed after: 10017798403045370114,5415160250146859793
