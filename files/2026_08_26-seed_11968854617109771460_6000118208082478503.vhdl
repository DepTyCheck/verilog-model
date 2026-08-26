-- Seed: 11968854617109771460,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity qqacsm is
  port (pqsasemy : out std_logic_vector(1 downto 4); qwdvrnmd : in std_logic; cwvhreulv : inout std_logic);
end qqacsm;

architecture fonqmuuhm of qqacsm is
  
begin
  -- Multi-driven assignments
  cwvhreulv <= 'Z';
  cwvhreulv <= 'U';
  cwvhreulv <= qwdvrnmd;
end fonqmuuhm;

library ieee;
use ieee.std_logic_1164.all;

entity mij is
  port (zvdhua : inout integer; mrg : buffer std_logic; o : out real);
end mij;

architecture jmhzik of mij is
  
begin
  -- Single-driven assignments
  zvdhua <= 0222;
  o <= o;
  
  -- Multi-driven assignments
  mrg <= '0';
  mrg <= 'L';
  mrg <= mrg;
end jmhzik;

entity mluzwc is
  port (lbyamzlzug : linkage real; vtmigoxadt : linkage real);
end mluzwc;

library ieee;
use ieee.std_logic_1164.all;

architecture kaqg of mluzwc is
  signal thmluhwu : std_logic;
  signal fwyzfj : std_logic;
  signal exyvswud : std_logic_vector(1 downto 4);
  signal ggmmvf : std_logic;
  signal br : std_logic_vector(1 downto 4);
begin
  ig : entity work.qqacsm
    port map (pqsasemy => br, qwdvrnmd => ggmmvf, cwvhreulv => ggmmvf);
  xpkvgvvh : entity work.qqacsm
    port map (pqsasemy => exyvswud, qwdvrnmd => ggmmvf, cwvhreulv => fwyzfj);
  ypcv : entity work.qqacsm
    port map (pqsasemy => br, qwdvrnmd => ggmmvf, cwvhreulv => thmluhwu);
  haf : entity work.qqacsm
    port map (pqsasemy => br, qwdvrnmd => fwyzfj, cwvhreulv => thmluhwu);
  
  -- Multi-driven assignments
  fwyzfj <= 'L';
end kaqg;



-- Seed after: 15383196554548048856,6000118208082478503
