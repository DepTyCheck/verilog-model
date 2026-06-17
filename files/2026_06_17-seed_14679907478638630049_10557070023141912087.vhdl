-- Seed: 14679907478638630049,10557070023141912087

entity rhfkkhtnr is
  port (ykqnysjlgj : out time_vector(3 downto 2));
end rhfkkhtnr;

architecture bozxwmkk of rhfkkhtnr is
  
begin
  
end bozxwmkk;

library ieee;
use ieee.std_logic_1164.all;

entity cizh is
  port (etzix : linkage time; hteyybql : linkage boolean_vector(3 downto 2); teucnv : buffer std_logic_vector(4 to 4));
end cizh;

architecture wpt of cizh is
  signal sexi : time_vector(3 downto 2);
  signal oudwlt : time_vector(3 downto 2);
  signal emkc : time_vector(3 downto 2);
  signal c : time_vector(3 downto 2);
begin
  odj : entity work.rhfkkhtnr
    port map (ykqnysjlgj => c);
  ipfp : entity work.rhfkkhtnr
    port map (ykqnysjlgj => emkc);
  dcfzk : entity work.rhfkkhtnr
    port map (ykqnysjlgj => oudwlt);
  jr : entity work.rhfkkhtnr
    port map (ykqnysjlgj => sexi);
  
  -- Multi-driven assignments
  teucnv <= (others => '1');
end wpt;

entity sfrr is
  port (apykoyf : linkage string(4 downto 5); ppj : inout time);
end sfrr;

architecture spauzuzy of sfrr is
  signal jvle : time_vector(3 downto 2);
  signal glu : time_vector(3 downto 2);
  signal gtbkiksdc : time_vector(3 downto 2);
  signal sdbcdir : time_vector(3 downto 2);
begin
  kaglbqrgxs : entity work.rhfkkhtnr
    port map (ykqnysjlgj => sdbcdir);
  l : entity work.rhfkkhtnr
    port map (ykqnysjlgj => gtbkiksdc);
  avyzb : entity work.rhfkkhtnr
    port map (ykqnysjlgj => glu);
  hz : entity work.rhfkkhtnr
    port map (ykqnysjlgj => jvle);
  
  -- Single-driven assignments
  ppj <= 16#1.F# us;
end spauzuzy;



-- Seed after: 17857946457681938929,10557070023141912087
