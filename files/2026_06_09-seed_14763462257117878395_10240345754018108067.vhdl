-- Seed: 14763462257117878395,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity qofscwrp is
  port (rjyktwb : linkage std_logic_vector(4 to 4); xnnkrpyplp : out real; ddozgorcc : inout std_logic_vector(0 downto 3));
end qofscwrp;



architecture kca of qofscwrp is
  
begin
  
end kca;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (pvotzw : linkage severity_level; ayulelkvw : inout std_logic; fckd : linkage integer; ytf : inout real_vector(3 downto 3));
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture xgn of x is
  signal rlk : real;
  signal ubwzk : real;
  signal heibmzpw : std_logic_vector(4 to 4);
  signal doqgbdv : std_logic_vector(0 downto 3);
  signal aejm : real;
  signal kufyttzw : std_logic_vector(4 to 4);
begin
  xgktytzhl : entity work.qofscwrp
    port map (rjyktwb => kufyttzw, xnnkrpyplp => aejm, ddozgorcc => doqgbdv);
  jwzqeyzig : entity work.qofscwrp
    port map (rjyktwb => heibmzpw, xnnkrpyplp => ubwzk, ddozgorcc => doqgbdv);
  upmbi : entity work.qofscwrp
    port map (rjyktwb => kufyttzw, xnnkrpyplp => rlk, ddozgorcc => doqgbdv);
end xgn;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (stsdcon : buffer std_logic; uihbfdzab : inout std_logic_vector(0 to 3));
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture pib of b is
  signal xigsmyq : std_logic_vector(0 downto 3);
  signal dpzcd : real;
  signal ikcqp : std_logic_vector(4 to 4);
  signal togxedksis : real_vector(3 downto 3);
  signal pmdqcyz : integer;
  signal kssba : severity_level;
begin
  zyj : entity work.x
    port map (pvotzw => kssba, ayulelkvw => stsdcon, fckd => pmdqcyz, ytf => togxedksis);
  krotyfe : entity work.qofscwrp
    port map (rjyktwb => ikcqp, xnnkrpyplp => dpzcd, ddozgorcc => xigsmyq);
end pib;



entity iv is
  port (awual : out real; iwdzgw : in integer_vector(3 to 4); rhmmucy : linkage bit);
end iv;

library ieee;
use ieee.std_logic_1164.all;

architecture mjydp of iv is
  signal gll : real;
  signal n : std_logic_vector(4 to 4);
  signal ujjlhlfl : real_vector(3 downto 3);
  signal wsprbllzz : integer;
  signal nuncpnkiqr : std_logic;
  signal ceah : severity_level;
  signal gbqwxxagg : real_vector(3 downto 3);
  signal gsbn : integer;
  signal esrhco : std_logic;
  signal j : severity_level;
  signal ww : std_logic_vector(0 downto 3);
  signal nxgccfwgp : real;
  signal ahzbasybv : std_logic_vector(4 to 4);
begin
  hh : entity work.qofscwrp
    port map (rjyktwb => ahzbasybv, xnnkrpyplp => nxgccfwgp, ddozgorcc => ww);
  zopcwbi : entity work.x
    port map (pvotzw => j, ayulelkvw => esrhco, fckd => gsbn, ytf => gbqwxxagg);
  fs : entity work.x
    port map (pvotzw => ceah, ayulelkvw => nuncpnkiqr, fckd => wsprbllzz, ytf => ujjlhlfl);
  sbuafuohod : entity work.qofscwrp
    port map (rjyktwb => n, xnnkrpyplp => gll, ddozgorcc => ww);
end mjydp;



-- Seed after: 6375750110897903946,10240345754018108067
