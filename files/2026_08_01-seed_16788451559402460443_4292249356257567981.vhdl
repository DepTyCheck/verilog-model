-- Seed: 16788451559402460443,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity nhkwmha is
  port (a : out real; hmcsut : buffer std_logic_vector(0 to 1); krfxjyad : inout std_logic_vector(3 to 3));
end nhkwmha;

architecture wzet of nhkwmha is
  
begin
  -- Multi-driven assignments
  hmcsut <= "ZH";
  hmcsut <= "11";
  krfxjyad <= krfxjyad;
  krfxjyad <= (others => 'H');
end wzet;

library ieee;
use ieee.std_logic_1164.all;

entity vyvo is
  port (vmln : inout std_logic_vector(0 to 3));
end vyvo;

library ieee;
use ieee.std_logic_1164.all;

architecture qukulo of vyvo is
  signal urkhqc : std_logic_vector(0 to 1);
  signal kupppzsq : real;
  signal rtaif : std_logic_vector(3 to 3);
  signal vc : std_logic_vector(0 to 1);
  signal putjgn : real;
begin
  vbakfyben : entity work.nhkwmha
    port map (a => putjgn, hmcsut => vc, krfxjyad => rtaif);
  buxsvtjgu : entity work.nhkwmha
    port map (a => kupppzsq, hmcsut => urkhqc, krfxjyad => rtaif);
  
  -- Multi-driven assignments
  urkhqc <= vc;
end qukulo;

entity iayhjhgbwm is
  port (hfm : out boolean);
end iayhjhgbwm;

library ieee;
use ieee.std_logic_1164.all;

architecture fiviz of iayhjhgbwm is
  signal qwjqpc : std_logic_vector(0 to 3);
  signal tmibrme : std_logic_vector(0 to 1);
  signal rjbwjia : real;
  signal pqwrpj : std_logic_vector(0 to 1);
  signal xtnyfmczqr : real;
  signal bmyuoor : std_logic_vector(3 to 3);
  signal xbji : std_logic_vector(0 to 1);
  signal zyz : real;
begin
  i : entity work.nhkwmha
    port map (a => zyz, hmcsut => xbji, krfxjyad => bmyuoor);
  tkpu : entity work.nhkwmha
    port map (a => xtnyfmczqr, hmcsut => pqwrpj, krfxjyad => bmyuoor);
  ykywpez : entity work.nhkwmha
    port map (a => rjbwjia, hmcsut => tmibrme, krfxjyad => bmyuoor);
  lmsc : entity work.vyvo
    port map (vmln => qwjqpc);
  
  -- Single-driven assignments
  hfm <= TRUE;
  
  -- Multi-driven assignments
  tmibrme <= xbji;
end fiviz;

entity rt is
  port (weeuxvbqc : buffer character);
end rt;

library ieee;
use ieee.std_logic_1164.all;

architecture vooobnjm of rt is
  signal l : real;
  signal fhmfusvba : boolean;
  signal jlvb : std_logic_vector(3 to 3);
  signal gpquyymprn : std_logic_vector(0 to 1);
  signal ev : real;
  signal mtwb : std_logic_vector(0 to 3);
begin
  fwpnrt : entity work.vyvo
    port map (vmln => mtwb);
  qfjruxl : entity work.nhkwmha
    port map (a => ev, hmcsut => gpquyymprn, krfxjyad => jlvb);
  kwpqghoi : entity work.iayhjhgbwm
    port map (hfm => fhmfusvba);
  xlg : entity work.nhkwmha
    port map (a => l, hmcsut => gpquyymprn, krfxjyad => jlvb);
  
  -- Single-driven assignments
  weeuxvbqc <= weeuxvbqc;
  
  -- Multi-driven assignments
  mtwb <= "Z0L-";
  mtwb <= mtwb;
  mtwb <= "HWUX";
  mtwb <= mtwb;
end vooobnjm;



-- Seed after: 5574181148096927180,4292249356257567981
