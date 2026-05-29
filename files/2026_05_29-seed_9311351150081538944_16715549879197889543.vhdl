-- Seed: 9311351150081538944,16715549879197889543



entity mlrcihqkvg is
  port (m : in real; crdbylcfbv : inout boolean; tjevuu : out severity_level);
end mlrcihqkvg;



architecture gphmvy of mlrcihqkvg is
  
begin
  
end gphmvy;

library ieee;
use ieee.std_logic_1164.all;

entity dvi is
  port (zfj : out boolean_vector(2 downto 3); eshjgmn : out std_logic_vector(2 to 3); iu : in std_logic_vector(4 to 1));
end dvi;



architecture gsiqasw of dvi is
  signal wvipupe : severity_level;
  signal coajph : boolean;
  signal msgxmgprzn : real;
begin
  hzr : entity work.mlrcihqkvg
    port map (m => msgxmgprzn, crdbylcfbv => coajph, tjevuu => wvipupe);
end gsiqasw;

library ieee;
use ieee.std_logic_1164.all;

entity rac is
  port (p : buffer std_logic_vector(3 downto 2));
end rac;



architecture tnfp of rac is
  signal sluewdmo : severity_level;
  signal grnx : boolean;
  signal sgffbwqauo : severity_level;
  signal qvrw : boolean;
  signal jng : real;
begin
  hzuoy : entity work.mlrcihqkvg
    port map (m => jng, crdbylcfbv => qvrw, tjevuu => sgffbwqauo);
  ophosln : entity work.mlrcihqkvg
    port map (m => jng, crdbylcfbv => grnx, tjevuu => sluewdmo);
end tnfp;



entity pflt is
  port (csviov : out real; xsmypg : out time; raeb : inout integer; zfovmugpqw : inout time);
end pflt;

library ieee;
use ieee.std_logic_1164.all;

architecture tjyannhdfq of pflt is
  signal rankttt : std_logic_vector(4 to 1);
  signal bysvlga : boolean_vector(2 downto 3);
  signal msglejyn : std_logic_vector(2 to 3);
begin
  kckdhaqt : entity work.rac
    port map (p => msglejyn);
  cbmj : entity work.dvi
    port map (zfj => bysvlga, eshjgmn => msglejyn, iu => rankttt);
end tjyannhdfq;



-- Seed after: 4581745275977358148,16715549879197889543
