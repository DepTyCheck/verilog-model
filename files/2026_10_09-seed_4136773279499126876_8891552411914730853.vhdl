-- Seed: 4136773279499126876,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity dgfpbempg is
  port (upwbdt : buffer string(2 to 5); nf : buffer integer; kk : linkage std_logic_vector(4 to 4));
end dgfpbempg;

architecture guhtx of dgfpbempg is
  
begin
  -- Single-driven assignments
  nf <= nf;
  upwbdt <= upwbdt;
end guhtx;

library ieee;
use ieee.std_logic_1164.all;

entity ifxo is
  port (cxvsilxegu : buffer std_logic_vector(4 to 4); ndeb : linkage time; dacgun : out boolean; ytcpsix : in severity_level);
end ifxo;

library ieee;
use ieee.std_logic_1164.all;

architecture fpncszvx of ifxo is
  signal yfmgrjq : std_logic_vector(4 to 4);
  signal gefe : integer;
  signal yqh : string(2 to 5);
  signal tqo : std_logic_vector(4 to 4);
  signal azrwq : integer;
  signal qiytkv : string(2 to 5);
begin
  hptyqlerhu : entity work.dgfpbempg
    port map (upwbdt => qiytkv, nf => azrwq, kk => tqo);
  lqnei : entity work.dgfpbempg
    port map (upwbdt => yqh, nf => gefe, kk => yfmgrjq);
  
  -- Single-driven assignments
  dacgun <= dacgun;
  
  -- Multi-driven assignments
  tqo <= (others => 'X');
  yfmgrjq <= yfmgrjq;
end fpncszvx;

library ieee;
use ieee.std_logic_1164.all;

entity hnhcaszp is
  port (vl : out std_logic; emhielytqm : inout std_logic_vector(3 to 1); ts : in boolean; dox : inout real);
end hnhcaszp;

library ieee;
use ieee.std_logic_1164.all;

architecture gy of hnhcaszp is
  signal xxrdyg : severity_level;
  signal uqafhaewyg : boolean;
  signal lm : time;
  signal xa : std_logic_vector(4 to 4);
  signal fffuzz : std_logic_vector(4 to 4);
  signal pfbidh : integer;
  signal zcyctc : string(2 to 5);
begin
  dojgyrpnho : entity work.dgfpbempg
    port map (upwbdt => zcyctc, nf => pfbidh, kk => fffuzz);
  e : entity work.ifxo
    port map (cxvsilxegu => xa, ndeb => lm, dacgun => uqafhaewyg, ytcpsix => xxrdyg);
  
  -- Single-driven assignments
  dox <= 8#4_4_4_3_0.3_1_5#;
  xxrdyg <= ERROR;
  
  -- Multi-driven assignments
  xa <= fffuzz;
  vl <= 'H';
  fffuzz <= fffuzz;
  vl <= 'U';
end gy;



-- Seed after: 5553997063654979628,8891552411914730853
