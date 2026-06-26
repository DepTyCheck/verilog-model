-- Seed: 13176695000902335011,12011142928354116943

entity dllyqtp is
  port (hbmlaq : out bit_vector(3 downto 1); nbn : buffer boolean; u : out boolean_vector(2 to 2); tfmasv : linkage integer);
end dllyqtp;

architecture vvkwgekjt of dllyqtp is
  
begin
  -- Single-driven assignments
  nbn <= FALSE;
  hbmlaq <= ('0', '1', '1');
  u <= (others => TRUE);
end vvkwgekjt;

entity hptbrvrlu is
  port (ofwdrz : in time; wdguhcffd : buffer real; dvmgk : inout integer);
end hptbrvrlu;

architecture qxgef of hptbrvrlu is
  signal qxhc : boolean_vector(2 to 2);
  signal gtxcltnslx : boolean;
  signal hrrf : bit_vector(3 downto 1);
  signal iz : integer;
  signal mwq : boolean_vector(2 to 2);
  signal lgzmetfxzj : boolean;
  signal nzhwxfg : bit_vector(3 downto 1);
  signal gbvzt : integer;
  signal xynjg : boolean_vector(2 to 2);
  signal nzuaz : boolean;
  signal vfw : bit_vector(3 downto 1);
  signal qypqh : integer;
  signal mjweilmdr : boolean_vector(2 to 2);
  signal kbviazmvc : boolean;
  signal mgsbsjnfs : bit_vector(3 downto 1);
begin
  ve : entity work.dllyqtp
    port map (hbmlaq => mgsbsjnfs, nbn => kbviazmvc, u => mjweilmdr, tfmasv => qypqh);
  hmiowcqi : entity work.dllyqtp
    port map (hbmlaq => vfw, nbn => nzuaz, u => xynjg, tfmasv => gbvzt);
  poausrlx : entity work.dllyqtp
    port map (hbmlaq => nzhwxfg, nbn => lgzmetfxzj, u => mwq, tfmasv => iz);
  kuycxif : entity work.dllyqtp
    port map (hbmlaq => hrrf, nbn => gtxcltnslx, u => qxhc, tfmasv => dvmgk);
end qxgef;

library ieee;
use ieee.std_logic_1164.all;

entity mhmt is
  port (eumoozcd : buffer character; eoelopcjcz : inout std_logic; q : in std_logic_vector(3 downto 3));
end mhmt;

architecture okxsyyw of mhmt is
  signal ohihp : integer;
  signal n : boolean_vector(2 to 2);
  signal kncbdafw : boolean;
  signal wrkdrw : bit_vector(3 downto 1);
begin
  rrawxqp : entity work.dllyqtp
    port map (hbmlaq => wrkdrw, nbn => kncbdafw, u => n, tfmasv => ohihp);
  
  -- Single-driven assignments
  eumoozcd <= 'l';
  
  -- Multi-driven assignments
  eoelopcjcz <= '-';
  eoelopcjcz <= 'X';
  eoelopcjcz <= 'W';
  eoelopcjcz <= '0';
end okxsyyw;



-- Seed after: 9137313665375000211,12011142928354116943
