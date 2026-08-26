-- Seed: 11095072694604204645,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity yxcmrd is
  port (h : inout time; vshwhipnr : linkage std_logic_vector(3 to 2));
end yxcmrd;

architecture nlc of yxcmrd is
  
begin
  
end nlc;

library ieee;
use ieee.std_logic_1164.all;

entity gmnrvjtuh is
  port (rsfrskfbk : in std_logic; mdyzxi : linkage std_logic; mgvon : buffer std_logic_vector(2 to 3); tvy : out std_logic_vector(3 to 2));
end gmnrvjtuh;

library ieee;
use ieee.std_logic_1164.all;

architecture fdmgnv of gmnrvjtuh is
  signal qvngbmrhy : time;
  signal smljekwvek : std_logic_vector(3 to 2);
  signal netqisxj : time;
begin
  jilgburtl : entity work.yxcmrd
    port map (h => netqisxj, vshwhipnr => smljekwvek);
  hxju : entity work.yxcmrd
    port map (h => qvngbmrhy, vshwhipnr => tvy);
  
  -- Multi-driven assignments
  smljekwvek <= tvy;
end fdmgnv;



-- Seed after: 9945003369414173408,6000118208082478503
