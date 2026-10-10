-- Seed: 8628853257137704772,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity nkgb is
  port (z : in time; zznbindb : out boolean_vector(3 downto 1); esx : inout std_logic; ykgltka : buffer std_logic_vector(0 to 3));
end nkgb;

architecture mtqhjwm of nkgb is
  
begin
  -- Single-driven assignments
  zznbindb <= zznbindb;
  
  -- Multi-driven assignments
  ykgltka <= ('X', 'W', 'Z', 'H');
  ykgltka <= ('X', 'H', 'H', 'H');
  ykgltka <= ykgltka;
  ykgltka <= ('W', 'X', '-', '0');
end mtqhjwm;

library ieee;
use ieee.std_logic_1164.all;

entity kvh is
  port (mqfuy : out std_logic_vector(3 downto 1));
end kvh;

library ieee;
use ieee.std_logic_1164.all;

architecture gzsaskijdf of kvh is
  signal h : std_logic_vector(0 to 3);
  signal cjnbvyjkc : std_logic;
  signal wferg : boolean_vector(3 downto 1);
  signal z : std_logic_vector(0 to 3);
  signal hvep : boolean_vector(3 downto 1);
  signal f : time;
  signal cghialc : std_logic_vector(0 to 3);
  signal kkziiidgdy : std_logic;
  signal fsvmkkq : boolean_vector(3 downto 1);
  signal ifockdfemq : time;
begin
  nzffyqpou : entity work.nkgb
    port map (z => ifockdfemq, zznbindb => fsvmkkq, esx => kkziiidgdy, ykgltka => cghialc);
  nuixeagd : entity work.nkgb
    port map (z => f, zznbindb => hvep, esx => kkziiidgdy, ykgltka => z);
  hbkdygiqxh : entity work.nkgb
    port map (z => ifockdfemq, zznbindb => wferg, esx => cjnbvyjkc, ykgltka => h);
  
  -- Single-driven assignments
  ifockdfemq <= 41041.1_3_0_1_3 ps;
  f <= 1 hr;
  
  -- Multi-driven assignments
  h <= cghialc;
end gzsaskijdf;

library ieee;
use ieee.std_logic_1164.all;

entity exqargr is
  port ( ibvvbt : linkage std_logic_vector(2 to 0)
  ; clftawnj : linkage std_logic
  ; ftny : buffer std_logic_vector(2 to 3)
  ; zigam : out boolean_vector(0 downto 4)
  );
end exqargr;

library ieee;
use ieee.std_logic_1164.all;

architecture zhgxqamvp of exqargr is
  signal rmxeqivqzl : boolean_vector(3 downto 1);
  signal ak : std_logic_vector(0 to 3);
  signal phjqccetz : std_logic;
  signal lun : boolean_vector(3 downto 1);
  signal xrsdwkev : time;
begin
  yq : entity work.nkgb
    port map (z => xrsdwkev, zznbindb => lun, esx => phjqccetz, ykgltka => ak);
  dbjisif : entity work.nkgb
    port map (z => xrsdwkev, zznbindb => rmxeqivqzl, esx => phjqccetz, ykgltka => ak);
  
  -- Single-driven assignments
  zigam <= zigam;
  xrsdwkev <= 3210.32131 ns;
end zhgxqamvp;



-- Seed after: 7547599440585352818,511364357853360275
