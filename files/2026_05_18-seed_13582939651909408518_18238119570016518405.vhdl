-- Seed: 13582939651909408518,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (hrp : out std_logic; tiuz : linkage time_vector(3 downto 4); oofd : in bit_vector(2 downto 2));
end r;



architecture va of r is
  
begin
  
end va;

library ieee;
use ieee.std_logic_1164.all;

entity zcuephg is
  port ( ob : buffer std_logic_vector(2 downto 2)
  ; oppvpzn : inout std_logic
  ; kjr : linkage std_logic_vector(0 downto 3)
  ; ibfd : inout std_logic_vector(0 to 4)
  );
end zcuephg;



architecture jzk of zcuephg is
  signal ndd : bit_vector(2 downto 2);
  signal wflh : time_vector(3 downto 4);
  signal pfi : bit_vector(2 downto 2);
  signal ywoex : time_vector(3 downto 4);
  signal bgsuen : bit_vector(2 downto 2);
  signal twna : time_vector(3 downto 4);
begin
  uiwpzzpt : entity work.r
    port map (hrp => oppvpzn, tiuz => twna, oofd => bgsuen);
  pfnj : entity work.r
    port map (hrp => oppvpzn, tiuz => ywoex, oofd => pfi);
  fbtv : entity work.r
    port map (hrp => oppvpzn, tiuz => wflh, oofd => ndd);
  svamtbk : entity work.r
    port map (hrp => oppvpzn, tiuz => ywoex, oofd => bgsuen);
end jzk;



-- Seed after: 15150731055081721309,18238119570016518405
