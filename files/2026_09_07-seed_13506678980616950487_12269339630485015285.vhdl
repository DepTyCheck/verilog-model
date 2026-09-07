-- Seed: 13506678980616950487,12269339630485015285

entity vhbyt is
  port (atzthczhmo : in string(3 downto 5));
end vhbyt;

architecture ofnox of vhbyt is
  
begin
  
end ofnox;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (hfxslrgyx : linkage std_logic_vector(4 to 0); qzow : out real);
end f;

architecture kjwvkabh of f is
  signal fag : string(3 downto 5);
begin
  zou : entity work.vhbyt
    port map (atzthczhmo => fag);
  
  -- Single-driven assignments
  qzow <= 3300.1_4_3_4_3;
  fag <= (others => ' ');
end kjwvkabh;

entity lueycj is
  port (didgk : inout real; uvkbzoeti : in real_vector(2 to 3); xqeddmzhy : linkage boolean);
end lueycj;

architecture gkvaeyqtak of lueycj is
  signal n : string(3 downto 5);
begin
  mblk : entity work.vhbyt
    port map (atzthczhmo => n);
  
  -- Single-driven assignments
  didgk <= 2#0.1_1_0_0#;
  n <= n;
end gkvaeyqtak;

entity lpfafa is
  port (kveclrbb : inout time; tbiciqe : in boolean_vector(3 downto 0));
end lpfafa;

architecture s of lpfafa is
  signal abqfwizdrv : boolean;
  signal y : real_vector(2 to 3);
  signal qen : real;
  signal c : string(3 downto 5);
  signal sgtbrij : boolean;
  signal puf : real_vector(2 to 3);
  signal usuac : real;
begin
  cbdzg : entity work.lueycj
    port map (didgk => usuac, uvkbzoeti => puf, xqeddmzhy => sgtbrij);
  udpktbychj : entity work.vhbyt
    port map (atzthczhmo => c);
  vfu : entity work.lueycj
    port map (didgk => qen, uvkbzoeti => y, xqeddmzhy => abqfwizdrv);
end s;



-- Seed after: 6780562478963405705,12269339630485015285
