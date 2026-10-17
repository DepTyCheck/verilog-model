-- Seed: 15321017028400462745,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity paxrwh is
  port (swk : out bit_vector(0 to 2); wqyy : out real_vector(1 downto 1); q : inout std_logic);
end paxrwh;

architecture rghnoz of paxrwh is
  
begin
  -- Single-driven assignments
  swk <= ('0', '1', '0');
  wqyy <= (others => 3402.31041);
end rghnoz;

entity grkns is
  port (gyhbnljde : inout boolean_vector(1 downto 1); sizfm : out time);
end grkns;

library ieee;
use ieee.std_logic_1164.all;

architecture aksqldis of grkns is
  signal lxlkca : std_logic;
  signal hwggponc : real_vector(1 downto 1);
  signal nmcid : bit_vector(0 to 2);
  signal icpqbjwz : std_logic;
  signal wzzdol : real_vector(1 downto 1);
  signal trusbh : bit_vector(0 to 2);
begin
  sh : entity work.paxrwh
    port map (swk => trusbh, wqyy => wzzdol, q => icpqbjwz);
  khzhykjaqm : entity work.paxrwh
    port map (swk => nmcid, wqyy => hwggponc, q => lxlkca);
  
  -- Single-driven assignments
  sizfm <= sizfm;
  gyhbnljde <= (others => TRUE);
end aksqldis;



-- Seed after: 7377594508850990385,5444032936641997477
