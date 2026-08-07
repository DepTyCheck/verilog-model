-- Seed: 1878889005810136815,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity zymup is
  port (jn : buffer std_logic; ucwnmzf : out time; df : in std_logic);
end zymup;

architecture inxgsar of zymup is
  
begin
  -- Single-driven assignments
  ucwnmzf <= ucwnmzf;
  
  -- Multi-driven assignments
  jn <= '1';
  jn <= df;
  jn <= df;
  jn <= '-';
end inxgsar;

library ieee;
use ieee.std_logic_1164.all;

entity juntbngj is
  port (ubw : out real; zne : out boolean_vector(4 to 1); usbcurez : out std_logic; pcnw : in time);
end juntbngj;

library ieee;
use ieee.std_logic_1164.all;

architecture hhnezktcfi of juntbngj is
  signal vpgtafgj : std_logic;
  signal stlyk : time;
  signal lrn : time;
  signal epc : std_logic;
  signal hlspstosk : std_logic;
  signal vsjuyipyy : time;
  signal uogtty : std_logic;
begin
  earmv : entity work.zymup
    port map (jn => uogtty, ucwnmzf => vsjuyipyy, df => hlspstosk);
  xbpehcgew : entity work.zymup
    port map (jn => epc, ucwnmzf => lrn, df => usbcurez);
  aompl : entity work.zymup
    port map (jn => uogtty, ucwnmzf => stlyk, df => vpgtafgj);
  
  -- Single-driven assignments
  zne <= zne;
  ubw <= 16#066.6465#;
  
  -- Multi-driven assignments
  usbcurez <= vpgtafgj;
  usbcurez <= usbcurez;
end hhnezktcfi;

entity dima is
  port (mntzn : buffer real);
end dima;

library ieee;
use ieee.std_logic_1164.all;

architecture laf of dima is
  signal zly : time;
  signal zwilxwsehm : boolean_vector(4 to 1);
  signal ccbas : real;
  signal dmzgiuybyo : std_logic;
  signal cvqzdkqf : time;
  signal pbhpfilr : std_logic;
  signal qj : time;
  signal ozohvovhc : std_logic;
begin
  sv : entity work.zymup
    port map (jn => ozohvovhc, ucwnmzf => qj, df => pbhpfilr);
  nnzgxrltk : entity work.zymup
    port map (jn => ozohvovhc, ucwnmzf => cvqzdkqf, df => dmzgiuybyo);
  o : entity work.juntbngj
    port map (ubw => ccbas, zne => zwilxwsehm, usbcurez => ozohvovhc, pcnw => zly);
  
  -- Single-driven assignments
  zly <= 2#1.0_0_1# us;
  mntzn <= 2#01.01#;
  
  -- Multi-driven assignments
  pbhpfilr <= 'Z';
end laf;



-- Seed after: 11085512787673844856,8068158652091157513
