-- Seed: 15542256876068311367,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity vrodj is
  port (clphmfizvs : inout std_logic; cwl : inout real);
end vrodj;

architecture fqzlwm of vrodj is
  
begin
  -- Multi-driven assignments
  clphmfizvs <= 'Z';
  clphmfizvs <= clphmfizvs;
  clphmfizvs <= 'W';
end fqzlwm;

entity kjhxnub is
  port (ebogjyik : inout real_vector(4 to 2); sjuwuha : in real);
end kjhxnub;

library ieee;
use ieee.std_logic_1164.all;

architecture xbb of kjhxnub is
  signal xedefaz : real;
  signal jr : std_logic;
  signal lmlnvpwewa : real;
  signal gm : std_logic;
begin
  kgstin : entity work.vrodj
    port map (clphmfizvs => gm, cwl => lmlnvpwewa);
  b : entity work.vrodj
    port map (clphmfizvs => jr, cwl => xedefaz);
  
  -- Single-driven assignments
  ebogjyik <= ebogjyik;
  
  -- Multi-driven assignments
  gm <= 'Z';
  jr <= 'H';
  gm <= '1';
  gm <= 'X';
end xbb;

library ieee;
use ieee.std_logic_1164.all;

entity su is
  port (ksxfqap : inout std_logic_vector(4 downto 3));
end su;

library ieee;
use ieee.std_logic_1164.all;

architecture yimlnmef of su is
  signal bkd : real;
  signal sefgvqh : real_vector(4 to 2);
  signal lvidou : real;
  signal ledvmismu : real_vector(4 to 2);
  signal enqpljgip : std_logic;
  signal sjkw : real;
  signal gdkdkpumel : real_vector(4 to 2);
begin
  edxzcqzywc : entity work.kjhxnub
    port map (ebogjyik => gdkdkpumel, sjuwuha => sjkw);
  o : entity work.vrodj
    port map (clphmfizvs => enqpljgip, cwl => sjkw);
  wbw : entity work.kjhxnub
    port map (ebogjyik => ledvmismu, sjuwuha => lvidou);
  exkgzk : entity work.kjhxnub
    port map (ebogjyik => sefgvqh, sjuwuha => bkd);
  
  -- Single-driven assignments
  lvidou <= 16#C27.24EC#;
  
  -- Multi-driven assignments
  ksxfqap <= ksxfqap;
  ksxfqap <= "0X";
end yimlnmef;



-- Seed after: 1552306413685039357,6562834012285671529
