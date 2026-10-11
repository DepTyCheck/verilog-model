-- Seed: 10736671106299033925,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity xiptexlb is
  port (ovhjdoi : in integer; qwualij : inout std_logic);
end xiptexlb;

architecture wckke of xiptexlb is
  
begin
  
end wckke;

library ieee;
use ieee.std_logic_1164.all;

entity mqcxlk is
  port (ycltyy : inout time; fwhuur : linkage std_logic_vector(1 to 4); zgkbhh : inout boolean_vector(3 downto 3); ya : out boolean);
end mqcxlk;

library ieee;
use ieee.std_logic_1164.all;

architecture wojdvhr of mqcxlk is
  signal a : std_logic;
  signal mvamoghljb : integer;
  signal rd : std_logic;
  signal rzxqkb : integer;
begin
  keg : entity work.xiptexlb
    port map (ovhjdoi => rzxqkb, qwualij => rd);
  tydcsf : entity work.xiptexlb
    port map (ovhjdoi => mvamoghljb, qwualij => a);
  lekvonxida : entity work.xiptexlb
    port map (ovhjdoi => rzxqkb, qwualij => a);
  
  -- Single-driven assignments
  zgkbhh <= zgkbhh;
  ycltyy <= ycltyy;
  ya <= TRUE;
  
  -- Multi-driven assignments
  rd <= rd;
  a <= 'U';
  rd <= a;
  rd <= rd;
end wojdvhr;

library ieee;
use ieee.std_logic_1164.all;

entity nvcofqws is
  port (chmdipvlwc : inout integer; mneasz : out time; imzvlbnio : buffer std_logic_vector(1 to 2));
end nvcofqws;

library ieee;
use ieee.std_logic_1164.all;

architecture d of nvcofqws is
  signal txoo : boolean;
  signal gwz : boolean_vector(3 downto 3);
  signal gk : std_logic_vector(1 to 4);
begin
  r : entity work.mqcxlk
    port map (ycltyy => mneasz, fwhuur => gk, zgkbhh => gwz, ya => txoo);
  
  -- Single-driven assignments
  chmdipvlwc <= 2_3_2_2;
end d;

library ieee;
use ieee.std_logic_1164.all;

entity iycpa is
  port (dpwgqace : in std_logic; nn : inout integer; qayzer : out boolean);
end iycpa;

library ieee;
use ieee.std_logic_1164.all;

architecture oi of iycpa is
  signal d : boolean;
  signal dm : boolean_vector(3 downto 3);
  signal itdhccmrve : time;
  signal sgc : std_logic_vector(1 to 2);
  signal zot : time;
  signal vo : integer;
  signal yqwy : boolean_vector(3 downto 3);
  signal vzywcmr : std_logic_vector(1 to 4);
  signal wnkstjyid : time;
begin
  qg : entity work.mqcxlk
    port map (ycltyy => wnkstjyid, fwhuur => vzywcmr, zgkbhh => yqwy, ya => qayzer);
  gvxqhfzq : entity work.nvcofqws
    port map (chmdipvlwc => vo, mneasz => zot, imzvlbnio => sgc);
  zjwcicf : entity work.mqcxlk
    port map (ycltyy => itdhccmrve, fwhuur => vzywcmr, zgkbhh => dm, ya => d);
  
  -- Multi-driven assignments
  vzywcmr <= ('1', 'X', 'H', 'U');
  vzywcmr <= vzywcmr;
  sgc <= sgc;
end oi;



-- Seed after: 11795904885621270281,9099465110516446487
