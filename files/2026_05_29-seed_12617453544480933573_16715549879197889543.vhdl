-- Seed: 12617453544480933573,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity pzobesynzi is
  port (nb : linkage time; xok : inout std_logic_vector(4 to 3));
end pzobesynzi;



architecture udgpndlbi of pzobesynzi is
  
begin
  
end udgpndlbi;



entity bjq is
  port (ygkm : buffer time);
end bjq;

library ieee;
use ieee.std_logic_1164.all;

architecture kfzvwayc of bjq is
  signal lcp : std_logic_vector(4 to 3);
  signal lmpvtxjm : time;
  signal gesmwtgpq : std_logic_vector(4 to 3);
  signal vscckmdxz : std_logic_vector(4 to 3);
  signal bcyasuw : time;
begin
  uhhusr : entity work.pzobesynzi
    port map (nb => bcyasuw, xok => vscckmdxz);
  xaficpqy : entity work.pzobesynzi
    port map (nb => ygkm, xok => gesmwtgpq);
  xjcwbwyr : entity work.pzobesynzi
    port map (nb => lmpvtxjm, xok => lcp);
end kfzvwayc;



-- Seed after: 1799777557525797710,16715549879197889543
