-- Seed: 14540978373863255857,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity bsbkqfze is
  port (dzejegkhco : in bit_vector(4 downto 1); cjvqrsuqj : buffer std_logic_vector(1 downto 0); z : linkage real);
end bsbkqfze;

architecture nhwn of bsbkqfze is
  
begin
  
end nhwn;

entity oqa is
  port (ifv : buffer boolean; oe : in time);
end oqa;

library ieee;
use ieee.std_logic_1164.all;

architecture r of oqa is
  signal qewysmu : real;
  signal u : std_logic_vector(1 downto 0);
  signal mhrjuikxsk : real;
  signal rqlnnj : std_logic_vector(1 downto 0);
  signal pitzwqhb : bit_vector(4 downto 1);
  signal ztisosidcd : real;
  signal yandjwd : bit_vector(4 downto 1);
  signal kxph : real;
  signal j : std_logic_vector(1 downto 0);
  signal mx : bit_vector(4 downto 1);
begin
  hmtv : entity work.bsbkqfze
    port map (dzejegkhco => mx, cjvqrsuqj => j, z => kxph);
  mgfvigoix : entity work.bsbkqfze
    port map (dzejegkhco => yandjwd, cjvqrsuqj => j, z => ztisosidcd);
  vro : entity work.bsbkqfze
    port map (dzejegkhco => pitzwqhb, cjvqrsuqj => rqlnnj, z => mhrjuikxsk);
  c : entity work.bsbkqfze
    port map (dzejegkhco => mx, cjvqrsuqj => u, z => qewysmu);
  
  -- Single-driven assignments
  ifv <= FALSE;
  pitzwqhb <= mx;
  yandjwd <= ('1', '0', '0', '0');
  mx <= pitzwqhb;
  
  -- Multi-driven assignments
  rqlnnj <= "WL";
  u <= j;
  j <= j;
  j <= j;
end r;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (za : out std_logic_vector(0 downto 1); n : in std_logic; iprhdyf : in bit);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture jtg of r is
  signal latflxrxa : real;
  signal lbc : std_logic_vector(1 downto 0);
  signal ngtvkhy : real;
  signal br : bit_vector(4 downto 1);
  signal jnriy : real;
  signal ly : std_logic_vector(1 downto 0);
  signal kcix : bit_vector(4 downto 1);
  signal kf : real;
  signal ehwphxhfjb : std_logic_vector(1 downto 0);
  signal akmsvltoyt : bit_vector(4 downto 1);
begin
  nrzzvch : entity work.bsbkqfze
    port map (dzejegkhco => akmsvltoyt, cjvqrsuqj => ehwphxhfjb, z => kf);
  ru : entity work.bsbkqfze
    port map (dzejegkhco => kcix, cjvqrsuqj => ly, z => jnriy);
  pwhjxtpyl : entity work.bsbkqfze
    port map (dzejegkhco => br, cjvqrsuqj => ehwphxhfjb, z => ngtvkhy);
  ihmhip : entity work.bsbkqfze
    port map (dzejegkhco => kcix, cjvqrsuqj => lbc, z => latflxrxa);
  
  -- Single-driven assignments
  akmsvltoyt <= ('1', '0', '0', '0');
  br <= ('0', '1', '1', '0');
  kcix <= akmsvltoyt;
  
  -- Multi-driven assignments
  ehwphxhfjb <= "XH";
  za <= za;
  lbc <= ('L', 'W');
end jtg;

entity ikt is
  port (texhil : buffer character);
end ikt;

library ieee;
use ieee.std_logic_1164.all;

architecture yitkkjxewx of ikt is
  signal hnw : real;
  signal mrpwy : bit_vector(4 downto 1);
  signal qigpxtj : real;
  signal mvuxr : real;
  signal ymiiqfs : std_logic_vector(1 downto 0);
  signal uiyuchd : bit_vector(4 downto 1);
begin
  mxa : entity work.bsbkqfze
    port map (dzejegkhco => uiyuchd, cjvqrsuqj => ymiiqfs, z => mvuxr);
  jn : entity work.bsbkqfze
    port map (dzejegkhco => uiyuchd, cjvqrsuqj => ymiiqfs, z => qigpxtj);
  poqhkut : entity work.bsbkqfze
    port map (dzejegkhco => mrpwy, cjvqrsuqj => ymiiqfs, z => hnw);
  
  -- Single-driven assignments
  mrpwy <= uiyuchd;
  texhil <= 'w';
  
  -- Multi-driven assignments
  ymiiqfs <= "1U";
  ymiiqfs <= ymiiqfs;
end yitkkjxewx;



-- Seed after: 10703127988673869919,10875537289884587119
