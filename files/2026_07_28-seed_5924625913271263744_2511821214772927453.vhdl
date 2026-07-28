-- Seed: 5924625913271263744,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity pkynmt is
  port (lodbupze : buffer std_logic_vector(0 downto 4); vhachoavpv : inout integer; ielcju : buffer boolean_vector(4 to 0));
end pkynmt;

architecture lg of pkynmt is
  
begin
  -- Single-driven assignments
  ielcju <= (others => TRUE);
end lg;

library ieee;
use ieee.std_logic_1164.all;

entity hiie is
  port (ecdmeasd : inout std_logic_vector(2 downto 4); gspazlk : out time; yjnjdey : out real; zuloyren : in std_logic_vector(1 downto 4));
end hiie;

library ieee;
use ieee.std_logic_1164.all;

architecture h of hiie is
  signal mib : boolean_vector(4 to 0);
  signal ipvxhytlf : integer;
  signal eeoj : std_logic_vector(0 downto 4);
  signal gcv : boolean_vector(4 to 0);
  signal qprgtesn : integer;
  signal agbq : boolean_vector(4 to 0);
  signal bgsreotew : integer;
  signal mshf : std_logic_vector(0 downto 4);
begin
  ltdqp : entity work.pkynmt
    port map (lodbupze => mshf, vhachoavpv => bgsreotew, ielcju => agbq);
  yrwgq : entity work.pkynmt
    port map (lodbupze => ecdmeasd, vhachoavpv => qprgtesn, ielcju => gcv);
  vueirf : entity work.pkynmt
    port map (lodbupze => eeoj, vhachoavpv => ipvxhytlf, ielcju => mib);
  
  -- Single-driven assignments
  yjnjdey <= yjnjdey;
  gspazlk <= gspazlk;
  
  -- Multi-driven assignments
  ecdmeasd <= mshf;
  mshf <= eeoj;
  ecdmeasd <= "";
end h;

entity jhfnnorzp is
  port (g : out string(3 to 3));
end jhfnnorzp;

library ieee;
use ieee.std_logic_1164.all;

architecture vp of jhfnnorzp is
  signal tzpfux : std_logic_vector(1 downto 4);
  signal wtihjo : real;
  signal oko : time;
  signal yldumhm : boolean_vector(4 to 0);
  signal jvoaxji : integer;
  signal eejbsuni : std_logic_vector(0 downto 4);
  signal ludjndqw : std_logic_vector(1 downto 4);
  signal e : real;
  signal k : time;
  signal d : std_logic_vector(2 downto 4);
begin
  qpvv : entity work.hiie
    port map (ecdmeasd => d, gspazlk => k, yjnjdey => e, zuloyren => ludjndqw);
  ipec : entity work.pkynmt
    port map (lodbupze => eejbsuni, vhachoavpv => jvoaxji, ielcju => yldumhm);
  nrbrfskx : entity work.hiie
    port map (ecdmeasd => d, gspazlk => oko, yjnjdey => wtihjo, zuloyren => tzpfux);
  
  -- Multi-driven assignments
  eejbsuni <= "";
  eejbsuni <= "";
  d <= (others => '0');
  tzpfux <= "";
end vp;

entity vjqlcol is
  port (mz : in real; bz : in time);
end vjqlcol;

library ieee;
use ieee.std_logic_1164.all;

architecture ypu of vjqlcol is
  signal mk : boolean_vector(4 to 0);
  signal qle : integer;
  signal ibjbddr : boolean_vector(4 to 0);
  signal eznoh : integer;
  signal ypev : real;
  signal faeqffs : time;
  signal impyqsq : std_logic_vector(0 downto 4);
begin
  fk : entity work.hiie
    port map (ecdmeasd => impyqsq, gspazlk => faeqffs, yjnjdey => ypev, zuloyren => impyqsq);
  ohbefe : entity work.pkynmt
    port map (lodbupze => impyqsq, vhachoavpv => eznoh, ielcju => ibjbddr);
  wzkpahe : entity work.pkynmt
    port map (lodbupze => impyqsq, vhachoavpv => qle, ielcju => mk);
  
  -- Multi-driven assignments
  impyqsq <= impyqsq;
  impyqsq <= "";
  impyqsq <= "";
  impyqsq <= impyqsq;
end ypu;



-- Seed after: 8925841095839143598,2511821214772927453
