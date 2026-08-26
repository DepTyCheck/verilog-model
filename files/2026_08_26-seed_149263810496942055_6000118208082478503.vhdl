-- Seed: 149263810496942055,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity ukwyae is
  port (uzbhvsme : inout std_logic_vector(3 downto 2));
end ukwyae;

architecture yfufrcizi of ukwyae is
  
begin
  
end yfufrcizi;

entity uc is
  port (bqreidoltk : buffer time);
end uc;

library ieee;
use ieee.std_logic_1164.all;

architecture cckxcxek of uc is
  signal fvp : std_logic_vector(3 downto 2);
  signal ppleeg : std_logic_vector(3 downto 2);
begin
  ptuwewts : entity work.ukwyae
    port map (uzbhvsme => ppleeg);
  rjryhjpu : entity work.ukwyae
    port map (uzbhvsme => fvp);
  k : entity work.ukwyae
    port map (uzbhvsme => ppleeg);
  qsqdfn : entity work.ukwyae
    port map (uzbhvsme => ppleeg);
  
  -- Single-driven assignments
  bqreidoltk <= 12402 ms;
  
  -- Multi-driven assignments
  ppleeg <= "H0";
  fvp <= "HX";
  ppleeg <= ('W', 'W');
  ppleeg <= ppleeg;
end cckxcxek;

entity fpknqylj is
  port (rokkachr : inout time);
end fpknqylj;

library ieee;
use ieee.std_logic_1164.all;

architecture sxwsvsya of fpknqylj is
  signal a : std_logic_vector(3 downto 2);
  signal opmh : std_logic_vector(3 downto 2);
  signal c : time;
begin
  ljfkozhzug : entity work.uc
    port map (bqreidoltk => c);
  ymbsg : entity work.ukwyae
    port map (uzbhvsme => opmh);
  uwncddbdw : entity work.ukwyae
    port map (uzbhvsme => a);
  rm : entity work.uc
    port map (bqreidoltk => rokkachr);
  
  -- Multi-driven assignments
  opmh <= opmh;
  a <= opmh;
  a <= a;
end sxwsvsya;



-- Seed after: 4508663366171629548,6000118208082478503
