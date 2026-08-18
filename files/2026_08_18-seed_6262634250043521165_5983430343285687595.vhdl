-- Seed: 6262634250043521165,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity wzswsbkt is
  port (kncsq : buffer std_logic_vector(3 downto 2));
end wzswsbkt;

architecture dhrj of wzswsbkt is
  
begin
  -- Multi-driven assignments
  kncsq <= "1W";
end dhrj;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (th : buffer bit; hotwtvcsf : in std_logic_vector(1 downto 4); bdsu : buffer time; txcnrm : out std_logic_vector(1 to 0));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture rudmdlpmg of f is
  signal vzcqilu : std_logic_vector(3 downto 2);
  signal jzma : std_logic_vector(3 downto 2);
begin
  bqmgz : entity work.wzswsbkt
    port map (kncsq => jzma);
  fkdt : entity work.wzswsbkt
    port map (kncsq => jzma);
  vukrlaguk : entity work.wzswsbkt
    port map (kncsq => vzcqilu);
  
  -- Single-driven assignments
  bdsu <= 8#6675.4526# ms;
  th <= th;
end rudmdlpmg;

library ieee;
use ieee.std_logic_1164.all;

entity agdrw is
  port (wrog : linkage std_logic; sefnrnince : inout real; pxqqhuf : linkage boolean);
end agdrw;

library ieee;
use ieee.std_logic_1164.all;

architecture aod of agdrw is
  signal pnihlkjf : time;
  signal x : std_logic_vector(1 to 0);
  signal couavj : bit;
  signal nyszljuobm : std_logic_vector(3 downto 2);
begin
  llaefhcx : entity work.wzswsbkt
    port map (kncsq => nyszljuobm);
  zihcve : entity work.f
    port map (th => couavj, hotwtvcsf => x, bdsu => pnihlkjf, txcnrm => x);
  
  -- Single-driven assignments
  sefnrnince <= sefnrnince;
end aod;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (timylwtffw : in std_logic_vector(4 to 2); vkdgjoppvp : out std_logic_vector(2 downto 4); tu : buffer bit);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture sofazd of v is
  signal vpich : boolean;
  signal dzbojhyj : real;
  signal whtdrudg : std_logic;
  signal gxwlvfapt : boolean;
  signal wavuajwjs : real;
  signal aceklv : std_logic;
  signal lz : std_logic_vector(3 downto 2);
begin
  k : entity work.wzswsbkt
    port map (kncsq => lz);
  b : entity work.wzswsbkt
    port map (kncsq => lz);
  cr : entity work.agdrw
    port map (wrog => aceklv, sefnrnince => wavuajwjs, pxqqhuf => gxwlvfapt);
  cbo : entity work.agdrw
    port map (wrog => whtdrudg, sefnrnince => dzbojhyj, pxqqhuf => vpich);
  
  -- Single-driven assignments
  tu <= tu;
end sofazd;



-- Seed after: 4422791516938025286,5983430343285687595
