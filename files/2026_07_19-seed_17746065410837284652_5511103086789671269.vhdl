-- Seed: 17746065410837284652,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (opdms : inout std_logic_vector(3 downto 0));
end p;

architecture yydaqprqgi of p is
  
begin
  -- Multi-driven assignments
  opdms <= opdms;
  opdms <= ('X', 'X', '-', '1');
end yydaqprqgi;

entity ps is
  port (mgcdwyofc : out time; ovtlzurhh : out bit; jgt : out boolean_vector(4 to 1));
end ps;

library ieee;
use ieee.std_logic_1164.all;

architecture bmievjwppo of ps is
  signal uqmkpw : std_logic_vector(3 downto 0);
  signal rkbm : std_logic_vector(3 downto 0);
  signal dbql : std_logic_vector(3 downto 0);
begin
  fg : entity work.p
    port map (opdms => dbql);
  ng : entity work.p
    port map (opdms => rkbm);
  zlcmmzotm : entity work.p
    port map (opdms => uqmkpw);
end bmievjwppo;

library ieee;
use ieee.std_logic_1164.all;

entity dt is
  port (dfd : out integer; dqz : linkage std_logic);
end dt;

library ieee;
use ieee.std_logic_1164.all;

architecture xxdb of dt is
  signal a : std_logic_vector(3 downto 0);
  signal jefbou : boolean_vector(4 to 1);
  signal ggewlz : bit;
  signal fvyvxeqrd : time;
  signal lmyswra : std_logic_vector(3 downto 0);
begin
  zlljitl : entity work.p
    port map (opdms => lmyswra);
  vgujapj : entity work.ps
    port map (mgcdwyofc => fvyvxeqrd, ovtlzurhh => ggewlz, jgt => jefbou);
  cggdbidk : entity work.p
    port map (opdms => a);
  shufiksm : entity work.p
    port map (opdms => a);
  
  -- Single-driven assignments
  dfd <= 8#5_0#;
  
  -- Multi-driven assignments
  lmyswra <= "-XZH";
  lmyswra <= lmyswra;
  a <= "Z1-Z";
  lmyswra <= ('1', '0', '1', '-');
end xxdb;

library ieee;
use ieee.std_logic_1164.all;

entity pcyaas is
  port (jlytbywl : out real; vpjmu : out severity_level; hopz : buffer std_logic_vector(2 to 1));
end pcyaas;

library ieee;
use ieee.std_logic_1164.all;

architecture mfu of pcyaas is
  signal vkqnshwm : std_logic_vector(3 downto 0);
  signal ugzahrb : std_logic_vector(3 downto 0);
  signal pxb : std_logic_vector(3 downto 0);
begin
  rlqpsii : entity work.p
    port map (opdms => pxb);
  k : entity work.p
    port map (opdms => ugzahrb);
  rpyrfcbkif : entity work.p
    port map (opdms => vkqnshwm);
  
  -- Single-driven assignments
  vpjmu <= vpjmu;
  
  -- Multi-driven assignments
  vkqnshwm <= ('U', 'W', 'W', 'W');
  vkqnshwm <= ('1', 'U', 'U', 'H');
end mfu;



-- Seed after: 13640075207083286369,5511103086789671269
