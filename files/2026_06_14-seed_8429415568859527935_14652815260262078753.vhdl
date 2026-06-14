-- Seed: 8429415568859527935,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity qylbtwi is
  port (d : linkage severity_level; cbb : in std_logic_vector(0 to 3); hurhha : in std_logic_vector(3 to 4); ukhvwzqwkt : inout integer);
end qylbtwi;

architecture tlezfgsw of qylbtwi is
  
begin
  -- Single-driven assignments
  ukhvwzqwkt <= 2#0_0#;
end tlezfgsw;

entity olrevzuh is
  port (flx : buffer integer; dm : linkage integer; ilykmkfkvp : linkage severity_level; ftnuu : linkage boolean);
end olrevzuh;

library ieee;
use ieee.std_logic_1164.all;

architecture kb of olrevzuh is
  signal bzjv : integer;
  signal om : severity_level;
  signal dhbqmq : integer;
  signal rlpalzhx : std_logic_vector(3 to 4);
  signal bzwlofjgbz : severity_level;
  signal jgyohnl : std_logic_vector(3 to 4);
  signal kbjtkupr : std_logic_vector(0 to 3);
begin
  bjnolv : entity work.qylbtwi
    port map (d => ilykmkfkvp, cbb => kbjtkupr, hurhha => jgyohnl, ukhvwzqwkt => flx);
  w : entity work.qylbtwi
    port map (d => bzwlofjgbz, cbb => kbjtkupr, hurhha => rlpalzhx, ukhvwzqwkt => dhbqmq);
  dqddnd : entity work.qylbtwi
    port map (d => om, cbb => kbjtkupr, hurhha => jgyohnl, ukhvwzqwkt => bzjv);
  
  -- Multi-driven assignments
  rlpalzhx <= "ZW";
end kb;

library ieee;
use ieee.std_logic_1164.all;

entity wemjiacx is
  port (cdcb : linkage std_logic; n : buffer std_logic_vector(4 downto 3); j : linkage time);
end wemjiacx;

library ieee;
use ieee.std_logic_1164.all;

architecture wnz of wemjiacx is
  signal zrl : integer;
  signal dim : severity_level;
  signal tirdchj : integer;
  signal varf : std_logic_vector(3 to 4);
  signal bihz : std_logic_vector(0 to 3);
  signal etc : severity_level;
  signal prcmzhxa : integer;
  signal vscrvvw : std_logic_vector(3 to 4);
  signal adajudwin : severity_level;
  signal gwckryipn : integer;
  signal xkudpvs : std_logic_vector(0 to 3);
  signal mx : severity_level;
begin
  hhp : entity work.qylbtwi
    port map (d => mx, cbb => xkudpvs, hurhha => n, ukhvwzqwkt => gwckryipn);
  iwpraa : entity work.qylbtwi
    port map (d => adajudwin, cbb => xkudpvs, hurhha => vscrvvw, ukhvwzqwkt => prcmzhxa);
  vvzserf : entity work.qylbtwi
    port map (d => etc, cbb => bihz, hurhha => varf, ukhvwzqwkt => tirdchj);
  lb : entity work.qylbtwi
    port map (d => dim, cbb => xkudpvs, hurhha => n, ukhvwzqwkt => zrl);
  
  -- Multi-driven assignments
  varf <= ('Z', '-');
  varf <= ('X', '1');
  vscrvvw <= ('W', 'L');
  xkudpvs <= "1ZH0";
end wnz;



-- Seed after: 4257420808563810416,14652815260262078753
