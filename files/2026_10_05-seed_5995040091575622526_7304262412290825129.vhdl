-- Seed: 5995040091575622526,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity vs is
  port (ap : out std_logic_vector(4 downto 0); pjujhp : out std_logic; trpkti : out boolean);
end vs;

architecture s of vs is
  
begin
  -- Single-driven assignments
  trpkti <= TRUE;
  
  -- Multi-driven assignments
  ap <= ('0', 'H', 'W', '0', 'L');
  pjujhp <= 'W';
  pjujhp <= pjujhp;
  pjujhp <= pjujhp;
end s;

entity afvz is
  port (ewggdlunax : linkage real);
end afvz;

library ieee;
use ieee.std_logic_1164.all;

architecture waohwyget of afvz is
  signal rtcro : boolean;
  signal m : boolean;
  signal rwg : std_logic;
  signal ma : boolean;
  signal lymzgriu : std_logic_vector(4 downto 0);
  signal mjnaucrzdl : boolean;
  signal oocenodwc : std_logic;
  signal wdajdrr : std_logic_vector(4 downto 0);
begin
  maal : entity work.vs
    port map (ap => wdajdrr, pjujhp => oocenodwc, trpkti => mjnaucrzdl);
  d : entity work.vs
    port map (ap => lymzgriu, pjujhp => oocenodwc, trpkti => ma);
  lsu : entity work.vs
    port map (ap => wdajdrr, pjujhp => rwg, trpkti => m);
  fcbfuizzt : entity work.vs
    port map (ap => lymzgriu, pjujhp => rwg, trpkti => rtcro);
  
  -- Multi-driven assignments
  rwg <= oocenodwc;
  oocenodwc <= 'X';
end waohwyget;



-- Seed after: 10300829999336623443,7304262412290825129
