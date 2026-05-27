-- Seed: 7882673992217642586,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity awix is
  port (zpaxgnpeb : in std_logic_vector(4 downto 4); ztykb : in time);
end awix;



architecture erjdj of awix is
  
begin
  
end erjdj;



entity i is
  port (ek : inout bit; lprbaihvsq : linkage time);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture abfgihnwo of i is
  signal qpouoaxcn : time;
  signal ubrnqdmio : std_logic_vector(4 downto 4);
  signal gebbjrzs : time;
  signal chv : time;
  signal kuyju : std_logic_vector(4 downto 4);
begin
  tssfxkyu : entity work.awix
    port map (zpaxgnpeb => kuyju, ztykb => chv);
  vll : entity work.awix
    port map (zpaxgnpeb => kuyju, ztykb => gebbjrzs);
  wsr : entity work.awix
    port map (zpaxgnpeb => ubrnqdmio, ztykb => qpouoaxcn);
end abfgihnwo;

library ieee;
use ieee.std_logic_1164.all;

entity bg is
  port (lcmwrrx : in std_logic_vector(2 to 3));
end bg;

library ieee;
use ieee.std_logic_1164.all;

architecture gn of bg is
  signal e : bit;
  signal dtxd : time;
  signal a : std_logic_vector(4 downto 4);
  signal fyvdlacm : time;
  signal taele : bit;
  signal pkoj : time;
  signal logp : std_logic_vector(4 downto 4);
begin
  kuqkz : entity work.awix
    port map (zpaxgnpeb => logp, ztykb => pkoj);
  ngrmzw : entity work.i
    port map (ek => taele, lprbaihvsq => fyvdlacm);
  bhemxsed : entity work.awix
    port map (zpaxgnpeb => a, ztykb => dtxd);
  qrcowtbwu : entity work.i
    port map (ek => e, lprbaihvsq => pkoj);
end gn;



-- Seed after: 16760160961224553767,13854332967471039201
