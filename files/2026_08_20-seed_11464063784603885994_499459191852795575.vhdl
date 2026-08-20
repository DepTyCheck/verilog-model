-- Seed: 11464063784603885994,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity dnjekyiycn is
  port (bxeszyul : inout std_logic_vector(1 downto 4); fcopg : buffer integer; orlgerlssu : linkage std_logic_vector(1 to 3));
end dnjekyiycn;

architecture lj of dnjekyiycn is
  
begin
  -- Single-driven assignments
  fcopg <= 04;
  
  -- Multi-driven assignments
  bxeszyul <= "";
  bxeszyul <= (others => '0');
  bxeszyul <= (others => '0');
  bxeszyul <= "";
end lj;

library ieee;
use ieee.std_logic_1164.all;

entity knr is
  port (iwqmbyxgcg : out std_logic; t : linkage std_logic_vector(3 to 1); qgrk : out real);
end knr;

library ieee;
use ieee.std_logic_1164.all;

architecture wliz of knr is
  signal serz : std_logic_vector(1 to 3);
  signal mccmv : integer;
  signal zwzy : std_logic_vector(1 to 3);
  signal iemsjppe : integer;
  signal tvajpb : std_logic_vector(1 downto 4);
begin
  ldywa : entity work.dnjekyiycn
    port map (bxeszyul => tvajpb, fcopg => iemsjppe, orlgerlssu => zwzy);
  ucl : entity work.dnjekyiycn
    port map (bxeszyul => tvajpb, fcopg => mccmv, orlgerlssu => serz);
  
  -- Single-driven assignments
  qgrk <= 13.1;
  
  -- Multi-driven assignments
  zwzy <= ('1', '-', 'H');
  iwqmbyxgcg <= '0';
  iwqmbyxgcg <= 'U';
end wliz;

library ieee;
use ieee.std_logic_1164.all;

entity sxppoft is
  port (mkhlz : out time; axitvkrlh : inout real; shajdrzvu : buffer std_logic; uqfnfbusn : out time);
end sxppoft;

library ieee;
use ieee.std_logic_1164.all;

architecture klznksfd of sxppoft is
  signal gyr : std_logic_vector(1 to 3);
  signal vdhdmlv : integer;
  signal ortg : std_logic_vector(1 downto 4);
begin
  olsxsgydz : entity work.dnjekyiycn
    port map (bxeszyul => ortg, fcopg => vdhdmlv, orlgerlssu => gyr);
  
  -- Single-driven assignments
  mkhlz <= 16#6# fs;
  uqfnfbusn <= 0232 fs;
  axitvkrlh <= axitvkrlh;
  
  -- Multi-driven assignments
  ortg <= "";
  gyr <= "W0-";
end klznksfd;



-- Seed after: 13919656707884196620,499459191852795575
