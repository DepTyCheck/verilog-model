-- Seed: 11395054334063853887,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity rmd is
  port (foughcu : linkage std_logic_vector(2 to 1));
end rmd;



architecture ie of rmd is
  
begin
  
end ie;

library ieee;
use ieee.std_logic_1164.all;

entity mhyw is
  port (i : linkage real_vector(2 downto 1); m : out time; prl : buffer std_logic; dthjw : in real_vector(0 downto 2));
end mhyw;



architecture y of mhyw is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity bvhg is
  port (tiquqw : linkage std_logic; kjndkq : buffer time; llqwuug : inout real);
end bvhg;

library ieee;
use ieee.std_logic_1164.all;

architecture tytxlng of bvhg is
  signal pul : real_vector(0 downto 2);
  signal dfgdspvalc : std_logic;
  signal nfaukk : time;
  signal ukvne : real_vector(2 downto 1);
  signal iymdtmsh : std_logic_vector(2 to 1);
  signal dsatpkyd : std_logic_vector(2 to 1);
begin
  zauvkjn : entity work.rmd
    port map (foughcu => dsatpkyd);
  rbba : entity work.rmd
    port map (foughcu => iymdtmsh);
  ijsnl : entity work.mhyw
    port map (i => ukvne, m => nfaukk, prl => dfgdspvalc, dthjw => pul);
  b : entity work.rmd
    port map (foughcu => dsatpkyd);
end tytxlng;



-- Seed after: 8235989915322082247,7142793346053417159
